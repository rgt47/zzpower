#' Generic Server Factory for Power Analysis Tests
#'
#' Creates a complete server function for any power analysis test using the registry.
#' This factory generates reactive expressions and output handlers dynamically based
#' on test specifications.
#'
#' @keywords internal

#' Create Server for a Power Analysis Test
#'
#' Dynamically generates complete server logic based on test registry specification.
#'
#' @param id Module ID for the test
#' @param test_spec Test specification from registry
#' @param registry_func Function to get the registry (for access to other test helpers)
#'
#' @return Shiny module server function
#'
#' @keywords internal
create_generic_test_server <- function(id, test_spec, registry_func = get_power_test_registry) {

  function(input, output, session) {
    consts <- ZZPOWER_CONSTANTS

    # ===== VALIDATION REACTIVE =====
    # Validates all inputs and provides user feedback
    validation <- shiny::reactive({
      test_spec$validation(as.list(input))
    })

    # Display validation messages
    output[[paste0(id, "_validation")]] <- shiny::renderUI({
      issues <- validation()
      if (length(issues) > 0) {
        shiny::div(
          class = "alert alert-warning alert-dismissible fade show",
          role = "alert",
          shiny::HTML(
            paste(
              "<strong>Input Issues:</strong><br/>",
              paste("• ", issues, collapse = "<br/>")
            )
          ),
          shiny::tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "alert",
            `aria-label` = "Close"
          )
        )
      } else {
        NULL
      }
    })

    # ===== STUDY PARAMETERS REACTIVE =====
    # Calculates sample sizes and other design parameters
    study_parameters <- shiny::reactive({
      if (length(validation()) > 0) {
        return(NULL)
      }
      test_spec$sample_size_calc(as.list(input))
    })

    # Display sample size information
    output[[paste0(id, "_sample_size_display")]] <- shiny::renderUI({
      params <- study_parameters()
      if (is.null(params)) {
        return(shiny::p("Please fix input issues above"))
      }

      # Format based on what parameters are returned
      if (!is.null(params$n1) && !is.null(params$n2)) {
        shiny::div(
          shiny::p(
            shiny::strong("Group 1: "),
            sprintf("%.0f", params$n1)
          ),
          shiny::p(
            shiny::strong("Group 2: "),
            sprintf("%.0f", params$n2)
          ),
          shiny::p(
            shiny::strong("Total: "),
            sprintf("%.0f", params$n1 + params$n2)
          )
        )
      } else if (!is.null(params$n)) {
        shiny::div(
          shiny::p(
            shiny::strong("Sample Size: "),
            sprintf("%.0f", params$n)
          )
        )
      } else {
        NULL
      }
    })

    # ===== EFFECT SIZE RANGE REACTIVE =====
    # Generates sequence of effect sizes for power curve
    effect_size_range <- shiny::reactive({
      if (length(validation()) > 0) {
        return(NULL)
      }

      test_id <- id
      registry <- registry_func()
      spec <- registry[[test_id]]

      # Get the effect size range based on selected method and parameters
      method <- input[[paste0(test_id, "_effect_method")]] %||% spec$effect_size_methods[1]
      method_params <- spec$effect_size_params[[method]]

      # Get user-specified range (uses same naming convention as UI)
      es_min <- input[[paste0(test_id, "_", method, "_es")]][1] %||% method_params$default_min
      es_max <- input[[paste0(test_id, "_", method, "_es")]][2] %||% method_params$default_max

      # Generate sequence
      effect_sizes <- seq(es_min, es_max, length.out = consts$EFFECT_SIZE_SEQ_LENGTH)

      # Get any additional parameters (e.g., sd0 for difference method)
      additional_params <- list()
      if (!is.null(method_params$requires)) {
        for (param_name in names(method_params$requires)) {
          param_value <- input[[paste0(test_id, "_", method, "_", param_name)]]
          if (!is.null(param_value)) {
            additional_params[[param_name]] <- param_value
          }
        }
      }

      # Standardize effect sizes using test-specific function
      standardized <- spec$standardize(effect_sizes, method, c(as.list(input), additional_params))

      list(
        effect_sizes = effect_sizes,
        standardized = standardized,
        method = method
      )
    })

    # ===== POWER RESULTS REACTIVE =====
    # Calculates power for each effect size in the range
    power_results <- shiny::reactive({
      if (is.null(effect_size_range()) || length(validation()) > 0) {
        return(NULL)
      }

      es_range <- effect_size_range()
      params <- study_parameters()

      # Get standardized effect sizes
      standardized_es <- es_range$standardized

      # Get sample sizes
      if (!is.null(params$n1) && !is.null(params$n2)) {
        n1 <- params$n1
        n2 <- params$n2
      } else if (!is.null(params$n)) {
        n1 <- params$n
        n2 <- NULL
      } else {
        return(NULL)
      }

      # Get Type I error rate
      type1 <- input[[paste0(id, "_type1")]] %||% consts$TYPE1_DEFAULT

      # Get test direction (two-sided or one-sided)
      alternative <- "two.sided"
      if (!is.null(input[[paste0(id, "_onesided")]]) &&
          input[[paste0(id, "_onesided")]]) {
        alternative <- "one.sided"
      }

      # Calculate power for each effect size
      power_vec <- sapply(standardized_es, function(es) {
        tryCatch({
          if (!is.null(n2)) {
            # Two-sample test
            result <- test_spec$power_function(
              h = es, n1 = n1, n2 = n2,
              sig.level = type1, alternative = alternative
            )
          } else {
            # One-sample or paired test
            result <- test_spec$power_function(
              n = n1, d = es,  # Note: different param names for different tests
              sig.level = type1, alternative = alternative
            )
          }

          # Extract power (might be in $power or other name)
          if (!is.null(result$power)) {
            result$power
          } else if (!is.null(result$estimate)) {
            result$estimate
          } else {
            NA
          }
        }, error = function(e) NA)
      })

      # Create results dataframe
      data.frame(
        effect_size = es_range$effect_sizes,
        standardized_es = standardized_es,
        power = power_vec
      )
    })

    # ===== POWER PLOT OUTPUT =====
    output[[paste0(id, "_power_plot")]] <- shiny::renderPlot({
      results <- power_results()
      if (is.null(results)) {
        return(NULL)
      }

      es_range <- effect_size_range()

      # Get labels for axes
      registry <- registry_func()
      spec <- registry[[id]]
      method <- es_range$method

      # Get axis label based on effect size method
      x_label <- switch(method,
        "cohens_d" = "Effect Size (Cohen's d)",
        "percent_reduction" = "Percent Reduction",
        "difference" = "Difference",
        "active_change" = "Treatment Change",
        "proportions" = "Proportion (p1)",
        "difference" = "Proportion Difference",
        "odds_ratio" = "Odds Ratio",
        "relative_risk" = "Relative Risk",
        "correlation" = "Correlation Coefficient (r)",
        "Effect Size"
      )

      # Create plot
      ggplot2::ggplot(results, ggplot2::aes(x = .data$effect_size, y = .data$power)) +
        ggplot2::geom_line(linewidth = 1, color = "#2c3e50") +
        ggplot2::geom_point(size = 2, color = "#2c3e50") +
        ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "#e74c3c", linewidth = 0.5) +
        ggplot2::labs(
          title = paste("Power Curve -", spec$name),
          x = x_label,
          y = "Statistical Power",
          caption = "Dashed line indicates 80% power threshold"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.title = ggplot2::element_text(size = 12),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::ylim(0, 1)
    })

    # ===== RESULTS TABLE OUTPUT =====
    output[[paste0(id, "_results_table")]] <- DT::renderDataTable({
      results <- power_results()
      if (is.null(results)) {
        return(NULL)
      }

      # Format for display
      display_df <- data.frame(
        `Effect Size` = format(results$effect_size, digits = 3),
        `Standardized` = format(results$standardized_es, digits = 3),
        `Power` = format(results$power, digits = 3),
        check.names = FALSE
      )

      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          dom = "lftip"
        )
      )
    })

    # ===== STUDY SUMMARY OUTPUT =====
    output[[paste0(id, "_summary")]] <- shiny::renderText({
      if (length(validation()) > 0) {
        return("Please fix input issues to see study summary")
      }

      params <- study_parameters()
      es_range <- effect_size_range()

      if (is.null(params) || is.null(es_range)) {
        return("Calculating...")
      }

      registry <- registry_func()
      spec <- registry[[id]]

      # Build summary text
      summary_text <- paste(
        "TEST:", spec$name,
        "\n\nDESCRIPTION:", spec$description,
        "\n\nEFFECT SIZE METHOD:", es_range$method,
        "\nEffect Size Range:",
        sprintf("%.3f to %.3f", min(es_range$effect_sizes), max(es_range$effect_sizes)),
        "\n\nSAMPLE SIZE:"
      )

      if (!is.null(params$n1) && !is.null(params$n2)) {
        summary_text <- paste(
          summary_text,
          "\nGroup 1 (n):", sprintf("%.0f", params$n1),
          "\nGroup 2 (n):", sprintf("%.0f", params$n2),
          "\nTotal (n):", sprintf("%.0f", params$n1 + params$n2)
        )
      } else if (!is.null(params$n)) {
        summary_text <- paste(
          summary_text,
          "\nSample Size (n):", sprintf("%.0f", params$n)
        )
      }

      summary_text <- paste(
        summary_text,
        "\n\nTYPE I ERROR:",
        sprintf("%.4f", input[[paste0(id, "_type1")]] %||% consts$TYPE1_DEFAULT),
        "\nTEST DIRECTION:",
        if_else(input[[paste0(id, "_onesided")]], "One-sided", "Two-sided")
      )

      summary_text
    })

    # ===== REPORT DOWNLOAD =====
    output[[paste0(id, "_download_report")]] <- shiny::downloadHandler(
      filename = function() {
        format <- input[[paste0(id, "_report_format")]] %||% "text"
        ext <- if_else(format == "html", "html", "txt")
        paste0("power_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        if (length(validation()) > 0) {
          stop("Cannot generate report with input validation issues")
        }

        format <- input[[paste0(id, "_report_format")]] %||% "text"
        results <- power_results()
        params <- study_parameters()
        es_range <- effect_size_range()

        # Prepare report data
        report_data <- list(
          test_id = id,
          test_name = test_spec$name,
          format = format,
          timestamp = Sys.time(),
          r_version = paste(R.version$major, R.version$minor, sep = "."),
          parameters = as.list(input),
          sample_sizes = params,
          effect_size_range = es_range,
          power_results = results,
          type1_error = input[[paste0(id, "_type1")]] %||% consts$TYPE1_DEFAULT,
          one_sided = input[[paste0(id, "_onesided")]] %||% FALSE
        )

        # Generate report using generic function
        report_content <- .generate_generic_report(report_data, test_spec)

        # Write to file
        writeLines(report_content, file)
      }
    )
  }
}

#' Generate Generic Report
#'
#' Creates text or HTML report for any power analysis test.
#'
#' @param report_data List with test data
#' @param test_spec Test specification from registry
#'
#' @return Character vector with report content
#'
#' @keywords internal
.generate_generic_report <- function(report_data, test_spec) {

  format <- report_data$format

  if (format == "html") {
    return(.format_generic_html_report(report_data, test_spec))
  } else {
    return(.format_generic_text_report(report_data, test_spec))
  }
}

#' Format Generic Text Report
#'
#' @keywords internal
.format_generic_text_report <- function(report_data, test_spec) {

  lines <- c(
    "================================================================================",
    paste("POWER ANALYSIS REPORT:", test_spec$name),
    "================================================================================",
    "",
    paste("Generated:", format(report_data$timestamp, "%Y-%m-%d %H:%M:%S")),
    paste("R Version:", report_data$r_version),
    "",
    "TEST SPECIFICATION",
    "--------------------------------------------",
    paste("Test Type:", test_spec$name),
    paste("Description:", test_spec$description),
    "",
    "EFFECT SIZE METHOD",
    "--------------------------------------------",
    paste("Method:", report_data$effect_size_range$method),
    paste("Range:",
      sprintf("%.4f to %.4f",
        min(report_data$effect_size_range$effect_sizes),
        max(report_data$effect_size_range$effect_sizes)
      )
    ),
    "",
    "SAMPLE SIZE ALLOCATION",
    "--------------------------------------------"
  )

  # Add sample size details
  if (!is.null(report_data$sample_sizes$n1) &&
      !is.null(report_data$sample_sizes$n2)) {
    lines <- c(lines,
      paste("Group 1 (n1):", sprintf("%.0f", report_data$sample_sizes$n1)),
      paste("Group 2 (n2):", sprintf("%.0f", report_data$sample_sizes$n2)),
      paste("Total (N):",
        sprintf("%.0f", report_data$sample_sizes$n1 + report_data$sample_sizes$n2)
      )
    )
  } else if (!is.null(report_data$sample_sizes$n)) {
    lines <- c(lines,
      paste("Sample Size (n):", sprintf("%.0f", report_data$sample_sizes$n))
    )
  }

  lines <- c(lines,
    "",
    "STATISTICAL PARAMETERS",
    "--------------------------------------------",
    paste("Type I Error Rate (alpha):", sprintf("%.4f", report_data$type1_error)),
    paste("Test Direction:", if_else(report_data$one_sided, "One-sided", "Two-sided")),
    "",
    "POWER ANALYSIS RESULTS",
    "--------------------------------------------",
    "Effect Size | Standardized | Power",
    "--------- | ----------- | -----"
  )

  # Add results table
  for (i in seq_len(nrow(report_data$power_results))) {
    row <- report_data$power_results[i, ]
    lines <- c(lines,
      sprintf("%.4f | %.4f | %.4f",
        row$effect_size,
        row$standardized_es,
        row$power
      )
    )
  }

  lines <- c(lines,
    "",
    "================================================================================",
    "End of Report",
    "================================================================================",
    ""
  )

  lines
}

#' Format Generic HTML Report
#'
#' @keywords internal
.format_generic_html_report <- function(report_data, test_spec) {

  # Create HTML content
  html <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Power Analysis Report</title>
  <style>
    body {
      font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
      line-height: 1.6;
      color: #333;
      background-color: #f5f5f5;
      margin: 0;
      padding: 20px;
    }
    .container {
      max-width: 900px;
      margin: 0 auto;
      background-color: white;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h1 {
      color: #2c3e50;
      border-bottom: 3px solid #3498db;
      padding-bottom: 10px;
    }
    h2 {
      color: #34495e;
      margin-top: 25px;
      font-size: 18px;
    }
    .metadata {
      background-color: #ecf0f1;
      padding: 15px;
      border-radius: 5px;
      margin-bottom: 20px;
      font-size: 13px;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 15px 0;
    }
    th {
      background-color: #3498db;
      color: white;
      padding: 12px;
      text-align: left;
      font-weight: 600;
    }
    td {
      padding: 10px 12px;
      border-bottom: 1px solid #ddd;
    }
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    .parameter-group {
      margin-bottom: 20px;
    }
    .parameter-row {
      display: flex;
      margin-bottom: 8px;
    }
    .parameter-label {
      font-weight: 600;
      width: 200px;
      color: #2c3e50;
    }
    .parameter-value {
      color: #555;
    }
    .footer {
      margin-top: 30px;
      padding-top: 20px;
      border-top: 1px solid #ddd;
      font-size: 12px;
      color: #666;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>' , test_spec$name, '</h1>

    <div class="metadata">
      Generated: ', format(report_data$timestamp, "%Y-%m-%d %H:%M:%S"), '<br/>
      R Version: ', report_data$r_version, '<br/>
      Report Format: HTML
    </div>

    <h2>Test Specification</h2>
    <div class="parameter-group">
      <div class="parameter-row">
        <span class="parameter-label">Test Type:</span>
        <span class="parameter-value">', test_spec$name, '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Description:</span>
        <span class="parameter-value">', test_spec$description, '</span>
      </div>
    </div>

    <h2>Effect Size Specification</h2>
    <div class="parameter-group">
      <div class="parameter-row">
        <span class="parameter-label">Method:</span>
        <span class="parameter-value">', report_data$effect_size_range$method, '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Range:</span>
        <span class="parameter-value">',
        sprintf("%.4f to %.4f",
          min(report_data$effect_size_range$effect_sizes),
          max(report_data$effect_size_range$effect_sizes)
        ), '</span>
      </div>
    </div>

    <h2>Sample Size</h2>
    <div class="parameter-group">'
  )

  if (!is.null(report_data$sample_sizes$n1) &&
      !is.null(report_data$sample_sizes$n2)) {
    html <- paste0(html, '
      <div class="parameter-row">
        <span class="parameter-label">Group 1 (n1):</span>
        <span class="parameter-value">',
        sprintf("%.0f", report_data$sample_sizes$n1), '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Group 2 (n2):</span>
        <span class="parameter-value">',
        sprintf("%.0f", report_data$sample_sizes$n2), '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Total (N):</span>
        <span class="parameter-value">',
        sprintf("%.0f", report_data$sample_sizes$n1 + report_data$sample_sizes$n2), '</span>
      </div>'
    )
  } else if (!is.null(report_data$sample_sizes$n)) {
    html <- paste0(html, '
      <div class="parameter-row">
        <span class="parameter-label">Sample Size (n):</span>
        <span class="parameter-value">',
        sprintf("%.0f", report_data$sample_sizes$n), '</span>
      </div>'
    )
  }

  html <- paste0(html, '
    </div>

    <h2>Statistical Parameters</h2>
    <div class="parameter-group">
      <div class="parameter-row">
        <span class="parameter-label">Type I Error Rate:</span>
        <span class="parameter-value">',
        sprintf("%.4f", report_data$type1_error), '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Test Direction:</span>
        <span class="parameter-value">',
        if_else(report_data$one_sided, "One-sided", "Two-sided"), '</span>
      </div>
    </div>

    <h2>Power Analysis Results</h2>
    <table>
      <thead>
        <tr>
          <th>Effect Size</th>
          <th>Standardized</th>
          <th>Power</th>
        </tr>
      </thead>
      <tbody>'
  )

  # Add results table rows
  for (i in seq_len(nrow(report_data$power_results))) {
    row <- report_data$power_results[i, ]
    html <- paste0(html, '
        <tr>
          <td>', sprintf("%.4f", row$effect_size), '</td>
          <td>', sprintf("%.4f", row$standardized_es), '</td>
          <td>', sprintf("%.4f", row$power), '</td>
        </tr>'
    )
  }

  html <- paste0(html, '
      </tbody>
    </table>

    <div class="footer">
      <p>This report was automatically generated by zzpower. '
      , 'For questions or assistance, consult the package documentation.</p>
    </div>
  </div>
</body>
</html>'
  )

  html
}
