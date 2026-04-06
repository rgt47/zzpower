#' Generic Server Factory for Power Analysis Tests
#'
#' Creates complete server logic for any power analysis test using
#' shiny::moduleServer(). Each test tab runs as an independent module
#' with auto-scoped input/output namespacing.
#'
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom shiny reactiveValuesToList

#' Create Module Server for a Power Analysis Test
#'
#' Registers all reactive expressions and output renderers for a
#' single test tab via moduleServer(). Input IDs are automatically
#' scoped by the module namespace.
#'
#' @param id Module ID (must match the test_id used in the UI)
#' @param test_spec Test specification from registry
#' @param registry_func Function returning the full test registry
#'
#' @return The return value of moduleServer() (called for side effects)
#'
#' @keywords internal
create_generic_test_server <- function(id, test_spec,
                                       registry_func = get_power_test_registry) {
  shiny::moduleServer(id, function(input, output, session) {
    consts <- ZZPOWER_CONSTANTS
    param_names <- names(test_spec$parameters)

    .collect_params <- function() {
      vals <- lapply(param_names, function(p) input[[p]])
      names(vals) <- param_names
      vals
    }

    # ===== VALIDATION =====
    validation <- shiny::reactive({
      test_spec$validation(.collect_params())
    })

    is_valid <- shiny::reactive({
      length(validation()) == 0
    })

    output$validation <- shiny::renderUI({
      issues <- validation()
      if (length(issues) > 0) {
        shiny::div(
          class = "alert alert-warning alert-dismissible fade show",
          role = "alert",
          shiny::HTML(paste(
            "<strong>Input Issues:</strong><br/>",
            paste("- ", issues, collapse = "<br/>")
          )),
          shiny::tags$button(
            type = "button", class = "btn-close",
            `data-bs-dismiss` = "alert", `aria-label` = "Close"
          )
        )
      }
    })

    # ===== STUDY PARAMETERS =====
    study_parameters <- shiny::reactive({
      shiny::req(is_valid())
      test_spec$sample_size_calc(.collect_params())
    })

    output$sample_size_display <- shiny::renderUI({
      params <- shiny::req(study_parameters())

      if (!is.null(params$n1) && !is.null(params$n2)) {
        shiny::div(
          shiny::p(shiny::strong("Group 1: "), sprintf("%.0f", params$n1)),
          shiny::p(shiny::strong("Group 2: "), sprintf("%.0f", params$n2)),
          shiny::p(shiny::strong("Total: "),
            sprintf("%.0f", params$n1 + params$n2))
        )
      } else if (!is.null(params$n)) {
        shiny::div(
          shiny::p(shiny::strong("Sample Size: "), sprintf("%.0f", params$n))
        )
      }
    })

    # ===== EFFECT SIZE RANGE =====
    effect_size_range <- shiny::reactive({
      shiny::req(is_valid())

      spec <- test_spec
      method <- input$effect_method %||% spec$effect_size_methods[1]
      method_params <- spec$effect_size_params[[method]]

      es_min <- input[[paste0(method, "_es")]][1] %||% method_params$default_min
      es_max <- input[[paste0(method, "_es")]][2] %||% method_params$default_max

      effect_sizes <- seq(es_min, es_max, length.out = consts$EFFECT_SIZE_SEQ_LENGTH)

      additional_params <- list()
      if (!is.null(method_params$requires)) {
        for (param_name in names(method_params$requires)) {
          param_value <- input[[paste0(method, "_", param_name)]]
          if (!is.null(param_value)) {
            additional_params[[param_name]] <- param_value
          }
        }
      }

      std_params <- c(.collect_params(), additional_params)
      standardized <- spec$standardize(effect_sizes, method, std_params)

      list(
        effect_sizes = effect_sizes,
        standardized = standardized,
        method = method
      )
    })

    # ===== POWER RESULTS =====
    power_results <- shiny::reactive({
      es_range <- shiny::req(effect_size_range())
      params <- shiny::req(study_parameters())

      standardized_es <- es_range$standardized

      if (!is.null(params$n1) && !is.null(params$n2)) {
        n1 <- params$n1
        n2 <- params$n2
      } else if (!is.null(params$n)) {
        n1 <- params$n
        n2 <- NULL
      } else {
        return(NULL)
      }

      type1 <- input$type1 %||% consts$TYPE1_DEFAULT
      alternative <- if (isTRUE(input$onesided)) "one.sided" else "two.sided"

      fn <- test_spec$power_function
      fn_args <- names(formals(fn))
      es_name <- intersect(c("d", "h", "r"), fn_args)[1]

      power_vec <- vapply(standardized_es, function(es) {
        tryCatch({
          call_args <- list(sig.level = type1, alternative = alternative)
          call_args[[es_name]] <- es

          if (!is.null(n2) && "n1" %in% fn_args) {
            call_args$n1 <- n1
            call_args$n2 <- n2
          } else if ("n" %in% fn_args) {
            call_args$n <- n1
          }

          if ("type" %in% fn_args && !is.null(test_spec$test_type)) {
            call_args$type <- test_spec$test_type
          }

          result <- do.call(fn, call_args)
          result$power %||% NA_real_
        }, warning = function(w) NA_real_,
           error = function(e) NA_real_)
      }, FUN.VALUE = numeric(1))

      data.frame(
        effect_size = es_range$effect_sizes,
        standardized_es = standardized_es,
        power = power_vec
      )
    })

    # ===== POWER PLOT =====
    output$power_plot <- shiny::renderPlot({
      results <- shiny::req(power_results())
      results <- results[!is.na(results$power), , drop = FALSE]
      shiny::req(nrow(results) > 0)
      es_range <- effect_size_range()
      spec <- test_spec
      method <- es_range$method

      x_label <- switch(method,
        "cohens_d" = "Effect Size (Cohen's d)",
        "percent_reduction" = "Percent Reduction",
        "difference" = "Difference",
        "active_change" = "Treatment Change",
        "proportions" = "Proportion (p1)",
        "odds_ratio" = "Odds Ratio",
        "relative_risk" = "Relative Risk",
        "correlation" = "Correlation Coefficient (r)",
        "hazard_ratio" = "Hazard Ratio (HR)",
        "prop_range" = "Proportion at Highest Dose",
        "Effect Size"
      )

      ggplot2::ggplot(
        results,
        ggplot2::aes(x = .data$effect_size, y = .data$power)
      ) +
        ggplot2::geom_line(linewidth = 1, color = "#2c3e50") +
        ggplot2::geom_point(size = 2, color = "#2c3e50") +
        ggplot2::geom_hline(
          yintercept = 0.8, linetype = "dashed",
          color = "#e74c3c", linewidth = 0.5
        ) +
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

    # ===== RESULTS TABLE =====
    output$results_table <- DT::renderDataTable({
      results <- shiny::req(power_results())

      display_df <- data.frame(
        `Effect Size` = format(results$effect_size, digits = 3),
        `Standardized` = format(results$standardized_es, digits = 3),
        `Power` = format(results$power, digits = 3),
        check.names = FALSE
      )

      DT::datatable(display_df, options = list(pageLength = 10, dom = "lftip"))
    })

    # ===== STUDY SUMMARY =====
    output$summary <- shiny::renderText({
      shiny::req(is_valid())
      params <- shiny::req(study_parameters())
      es_range <- shiny::req(effect_size_range())
      spec <- test_spec

      summary_text <- paste(
        "TEST:", spec$name,
        "\n\nDESCRIPTION:", spec$description,
        "\n\nEFFECT SIZE METHOD:", es_range$method,
        "\nEffect Size Range:",
        sprintf("%.3f to %.3f",
          min(es_range$effect_sizes), max(es_range$effect_sizes)),
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

      type1 <- input$type1 %||% consts$TYPE1_DEFAULT
      one_sided <- isTRUE(input$onesided)

      paste(
        summary_text,
        "\n\nTYPE I ERROR:", sprintf("%.4f", type1),
        "\nTEST DIRECTION:", ifelse(one_sided, "One-sided", "Two-sided")
      )
    })

    # ===== REPORT DOWNLOAD =====
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$report_format %||% "text"
        ext <- ifelse(fmt == "html", "html", "txt")
        paste0("power_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        shiny::req(is_valid())

        fmt <- input$report_format %||% "text"
        results <- power_results()
        params <- study_parameters()
        es_range <- effect_size_range()

        report_data <- list(
          test_id = id,
          test_name = test_spec$name,
          format = fmt,
          timestamp = Sys.time(),
          r_version = paste(R.version$major, R.version$minor, sep = "."),
          parameters = reactiveValuesToList(input),
          sample_sizes = params,
          effect_size_range = es_range,
          power_results = results,
          type1_error = input$type1 %||% consts$TYPE1_DEFAULT,
          one_sided = isTRUE(input$onesided)
        )

        report_content <- .generate_generic_report(report_data, test_spec)
        writeLines(report_content, file)
      }
    )
  })
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
  if (report_data$format == "html") {
    .format_generic_html_report(report_data, test_spec)
  } else {
    .format_generic_text_report(report_data, test_spec)
  }
}

#' Format Generic Text Report
#'
#' @keywords internal
.format_generic_text_report <- function(report_data, test_spec) {

  lines <- c(
    "========================================================================",
    paste("POWER ANALYSIS REPORT:", test_spec$name),
    "========================================================================",
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

  if (!is.null(report_data$sample_sizes$n1) &&
      !is.null(report_data$sample_sizes$n2)) {
    lines <- c(lines,
      paste("Group 1 (n1):", sprintf("%.0f", report_data$sample_sizes$n1)),
      paste("Group 2 (n2):", sprintf("%.0f", report_data$sample_sizes$n2)),
      paste("Total (N):",
        sprintf("%.0f",
          report_data$sample_sizes$n1 + report_data$sample_sizes$n2))
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
    paste("Type I Error Rate (alpha):",
      sprintf("%.4f", report_data$type1_error)),
    paste("Test Direction:",
      ifelse(report_data$one_sided, "One-sided", "Two-sided")),
    "",
    "POWER ANALYSIS RESULTS",
    "--------------------------------------------",
    "Effect Size | Standardized | Power",
    "--------- | ----------- | -----"
  )

  for (i in seq_len(nrow(report_data$power_results))) {
    row <- report_data$power_results[i, ]
    lines <- c(lines,
      sprintf("%.4f | %.4f | %.4f",
        row$effect_size, row$standardized_es, row$power)
    )
  }

  c(lines, "",
    "========================================================================",
    "End of Report",
    "========================================================================",
    "")
}

#' Format Generic HTML Report
#'
#' @keywords internal
.format_generic_html_report <- function(report_data, test_spec) {

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
    <h1>', test_spec$name, '</h1>

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
        <span class="parameter-value">',
        report_data$effect_size_range$method, '</span>
      </div>
      <div class="parameter-row">
        <span class="parameter-label">Range:</span>
        <span class="parameter-value">',
        sprintf("%.4f to %.4f",
          min(report_data$effect_size_range$effect_sizes),
          max(report_data$effect_size_range$effect_sizes)),
        '</span>
      </div>
    </div>

    <h2>Sample Size</h2>
    <div class="parameter-group">')

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
        sprintf("%.0f",
          report_data$sample_sizes$n1 + report_data$sample_sizes$n2),
        '</span>
      </div>')
  } else if (!is.null(report_data$sample_sizes$n)) {
    html <- paste0(html, '
      <div class="parameter-row">
        <span class="parameter-label">Sample Size (n):</span>
        <span class="parameter-value">',
        sprintf("%.0f", report_data$sample_sizes$n), '</span>
      </div>')
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
        ifelse(report_data$one_sided, "One-sided", "Two-sided"),
        '</span>
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
      <tbody>')

  for (i in seq_len(nrow(report_data$power_results))) {
    row <- report_data$power_results[i, ]
    html <- paste0(html, '
        <tr>
          <td>', sprintf("%.4f", row$effect_size), '</td>
          <td>', sprintf("%.4f", row$standardized_es), '</td>
          <td>', sprintf("%.4f", row$power), '</td>
        </tr>')
  }

  paste0(html, '
      </tbody>
    </table>

    <div class="footer">
      <p>Report generated by zzpower. See package documentation
      for methodology details.</p>
    </div>
  </div>
</body>
</html>')
}
