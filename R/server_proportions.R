#' Create Server Logic for Two Proportions Analysis
#'
#' Internal function to create the server-side logic for
#' two-proportion comparison power analysis.
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme_minimal scale_y_continuous geom_vline
#' @importFrom DT renderDataTable datatable formatRound
#' @importFrom pwr pwr.2p2n.test
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom shiny reactive req observe updateNumericInput renderUI renderPlot renderText downloadHandler

create_proportions_server <- function() {
  function(input, output, session) {

    # Validate input parameters
    prop_validation <- shiny::reactive({
      issues <- character()

      # Validate total sample size
      if ((input$prop_total_n %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }

      # Validate allocation ratio if unequal
      if (input$prop_allocation == "unequal") {
        if ((input$prop_ratio %||% 1) <= 0) {
          issues <- c(issues, "Allocation ratio must be positive")
        }
      }

      # Validate Type I error
      type1 <- input$prop_type1 %||% 0.05
      if (type1 <= 0 || type1 >= 1) {
        issues <- c(issues, "Type I error must be between 0 and 1")
      }

      # Validate proportions or differences depending on method
      if (input$prop_dmeth == "proportions") {
        p1 <- input$prop_p1 %||% 0.5
        p2 <- input$prop_p2 %||% 0.3
        if (p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1) {
          issues <- c(issues, "Proportions must be between 0 and 1")
        }
      }

      if (input$prop_dmeth %in% c("difference", "or", "rr")) {
        baseline <- switch(input$prop_dmeth,
          "difference" = input$prop_baseline_diff,
          "or" = input$prop_baseline_or,
          "rr" = input$prop_baseline_rr
        ) %||% 0.5

        if (baseline <= 0 || baseline >= 1) {
          issues <- c(issues, "Baseline proportion must be between 0 and 1")
        }
      }

      issues
    })

    # Display validation messages
    output$prop_validation_messages <- shiny::renderUI({
      messages <- prop_validation()

      if (length(messages) > 0) {
        shiny::div(
          class = "alert alert-warning",
          role = "alert",
          shiny::h5("Input Issues:"),
          shiny::tagList(
            lapply(messages, function(msg) shiny::p(msg))
          )
        )
      }
    })

    # Calculate sample sizes
    prop_study_parameters <- shiny::reactive({
      shiny::req(input$prop_total_n, input$prop_allocation)

      total_n <- input$prop_total_n
      allocation <- input$prop_allocation

      if (allocation == "equal") {
        n1 <- total_n / 2
        n2 <- total_n / 2
      } else {
        ratio <- input$prop_ratio %||% 1
        n1 <- ratio * total_n / (ratio + 1)
        n2 <- total_n / (ratio + 1)
      }

      type1_val <- input$prop_type1 %||% 0.05
      onesided_val <- input$prop_onesided %||% FALSE
      sided <- if (onesided_val) 1 else 2

      list(
        n1 = n1,
        n2 = n2,
        total_n = total_n,
        sig_level = type1_val / sided
      )
    })

    # Generate effect size range based on method
    prop_effect_size_range <- shiny::reactive({
      shiny::req(input$prop_dmeth)
      consts <- ZZPOWER_CONSTANTS

      method <- input$prop_dmeth

      if (method == "proportions") {
        p1_min <- input$prop_p1 %||% consts$PROPORTION_DEFAULT_1
        p2_val <- input$prop_p2 %||% consts$PROPORTION_DEFAULT_2

        # Create range around p1
        p1_range <- c(
          max(0.01, p1_min - 0.25),
          min(0.99, p1_min + 0.25)
        )
        list(p1_range = p1_range, p2 = p2_val)

      } else if (method == "difference") {
        diff_range <- input$prop_diff %||% c(-0.3, -0.05)
        baseline <- input$prop_baseline_diff %||% 0.5
        list(diff_range = diff_range, baseline = baseline)

      } else if (method == "or") {
        or_range <- input$prop_or %||% c(1.2, 3)
        baseline <- input$prop_baseline_or %||% 0.5
        list(or_range = or_range, baseline = baseline)

      } else if (method == "rr") {
        rr_range <- input$prop_rr %||% c(1.2, 3)
        baseline <- input$prop_baseline_rr %||% 0.5
        list(rr_range = rr_range, baseline = baseline)
      }
    })

    # Convert to Cohen's h
    prop_cohens_h <- shiny::reactive({
      shiny::req(prop_effect_size_range(), input$prop_dmeth)
      consts <- ZZPOWER_CONSTANTS

      method <- input$prop_dmeth
      range_data <- prop_effect_size_range()

      effect_result <- generate_prop_effect_sizes(
        method = method,
        p1 = if (method == "proportions") range_data$p1_range[2] else NULL,
        p2 = if (method == "proportions") range_data$p2 else NULL,
        diff_min = if (method == "difference") range_data$diff_range[1] else NULL,
        diff_max = if (method == "difference") range_data$diff_range[2] else NULL,
        or_min = if (method == "or") range_data$or_range[1] else NULL,
        or_max = if (method == "or") range_data$or_range[2] else NULL,
        rr_min = if (method == "rr") range_data$rr_range[1] else NULL,
        rr_max = if (method == "rr") range_data$rr_range[2] else NULL,
        baseline = range_data$baseline %||% 0.5,
        n_points = consts$EFFECT_SIZE_SEQ_LENGTH
      )

      list(
        effect_sizes = effect_result$effect_sizes,
        cohens_h = effect_result$cohens_h
      )
    })

    # Calculate power for each effect size
    prop_power_results <- shiny::reactive({
      shiny::req(prop_cohens_h(), prop_study_parameters())

      params <- prop_study_parameters()
      h_values <- prop_cohens_h()$cohens_h
      es_values <- prop_cohens_h()$effect_sizes

      if (params$n1 <= 0 || params$n2 <= 0) {
        return(data.frame(
          effect_size = es_values,
          cohens_h = h_values,
          power = rep(NA, length(h_values))
        ))
      }

      power_values <- sapply(h_values, function(h) {
        tryCatch({
          pwr::pwr.2p2n.test(
            h = h,
            n1 = params$n1,
            n2 = params$n2,
            sig.level = params$sig_level
          )$power
        }, error = function(e) NA)
      })

      data.frame(
        effect_size = es_values,
        cohens_h = h_values,
        power = power_values
      )
    })

    # Sample size display
    output$prop_sample_size_display <- shiny::renderUI({
      shiny::req(prop_study_parameters())
      params <- prop_study_parameters()

      shiny::tagList(
        shiny::p(shiny::HTML(paste0(
          "<strong>Sample Sizes:</strong><br>",
          "Group 1: ", round(params$n1),
          ", Group 2: ", round(params$n2),
          " (Total: ", round(params$total_n), ")"
        )))
      )
    })

    # Power plot
    output$prop_power_plot <- shiny::renderPlot({
      shiny::req(prop_power_results())
      consts <- ZZPOWER_CONSTANTS

      results <- prop_power_results()
      method <- input$prop_dmeth

      if (all(is.na(results$power))) {
        plot.new()
        text(0.5, 0.5, "Invalid parameters for power calculation", cex = 1.2)
        return()
      }

      x_label <- get_prop_axis_label(method)

      # Find effect size for target power
      power_target_idx <- which.min(abs(results$power - consts$POWER_TARGET))
      power_target_es <- if (length(power_target_idx) > 0) {
        results$effect_size[power_target_idx]
      } else {
        NA
      }

      p <- ggplot2::ggplot(results, ggplot2::aes(x = .data$effect_size, y = .data$power)) +
        ggplot2::geom_line(color = consts$POWER_CURVE_COLOR, linewidth = 1) +
        ggplot2::geom_hline(yintercept = consts$POWER_TARGET,
                            color = consts$POWER_REFERENCE_COLOR,
                            linetype = consts$POWER_HLINE_STYLE) +
        ggplot2::labs(
          title = "Power Curve: Two Proportions Comparison",
          x = x_label,
          y = "Statistical Power"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
        ggplot2::theme_minimal()

      if (!is.na(power_target_es)) {
        p <- p + ggplot2::geom_vline(xintercept = power_target_es,
                                     color = consts$POWER_REFERENCE_COLOR,
                                     linetype = consts$POWER_VLINE_STYLE)
      }

      p
    })

    # Results table
    output$prop_results_table <- DT::renderDataTable({
      shiny::req(prop_power_results())
      consts <- ZZPOWER_CONSTANTS

      results <- prop_power_results()
      method <- input$prop_dmeth

      # Create display dataframe with appropriate label
      display_df <- data.frame(
        Effect_Size = results$effect_size,
        Power = results$power
      )
      colnames(display_df)[1] <- paste0("Effect Size (", method, ")")

      DT::datatable(
        display_df,
        options = list(
          pageLength = consts$TABLE_PAGE_LENGTH,
          scrollY = consts$POWER_CURVE_HEIGHT,
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = 1:2, digits = consts$TABLE_DECIMAL_PLACES)
    })

    # Summary text
    output$prop_summary_text <- shiny::renderText({
      shiny::req(prop_power_results(), prop_study_parameters())
      consts <- ZZPOWER_CONSTANTS

      results <- prop_power_results()
      params <- prop_study_parameters()
      method <- input$prop_dmeth

      # Find effect size for target power
      power_target_idx <- which.min(abs(results$power - consts$POWER_TARGET))
      power_target_es <- if (length(power_target_idx) > 0) {
        results$effect_size[power_target_idx]
      } else {
        NA
      }
      max_power <- max(results$power, na.rm = TRUE)

      type1_val <- input$prop_type1 %||% consts$TYPE1_DEFAULT
      onesided_val <- input$prop_onesided %||% FALSE

      paste0(
        "Study Design Summary:\n",
        "====================\n",
        "Total Sample Size: ", round(params$total_n), "\n",
        "  Group 1: ", round(params$n1), "\n",
        "  Group 2: ", round(params$n2), "\n",
        "Effect Size Method: ", method, "\n",
        "Type I Error Rate: ", type1_val, "\n",
        "Test Type: ", ifelse(onesided_val, "One-sided", "Two-sided"), "\n\n",
        "Power Analysis Results:\n",
        "====================\n",
        "Maximum Power: ", round(max_power, consts$TABLE_DECIMAL_PLACES), "\n",
        if (!is.na(power_target_es)) {
          paste0("Effect Size for ", round(consts$POWER_TARGET * 100), "% Power: ",
                 round(power_target_es, consts$TABLE_DECIMAL_PLACES))
        } else {
          paste0("Effect Size for ", round(consts$POWER_TARGET * 100), "% Power: Not achievable in tested range")
        }
      )
    })

    # Report download
    output$prop_download_report <- shiny::downloadHandler(
      filename = function() {
        format_ext <- tolower(input$prop_report_format)
        ext <- switch(format_ext,
          "pdf" = "html",
          "text" = "txt",
          "html" = "html"
        )
        paste0("proportions_power_report_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        report_format <- tolower(input$prop_report_format)
        format_map <- switch(report_format,
          "pdf" = "html",
          "html" = "html",
          "text" = "text"
        )

        report_content <- generate_proportions_report(
          input = input,
          power_results = prop_power_results,
          study_parameters = prop_study_parameters,
          format = format_map
        )

        writeLines(report_content, file)
      }
    )
  }
}
