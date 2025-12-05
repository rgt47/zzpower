#' Create Server Logic for the Shiny Application
#'
#' Internal function to create the server-side logic.
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme_minimal scale_y_continuous geom_vline
#' @importFrom DT renderDataTable datatable formatRound
#' @importFrom pwr pwr.t2n.test
#' @importFrom graphics plot.new text
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom shiny reactive req observe updateNumericInput updateSliderInput renderUI renderPlot renderText downloadHandler
create_server <- function() {
  function(input, output, session) {

    # Validate input parameters
    parameter_validation <- shiny::reactive({
      issues <- character()

      # Validate sample size
      if ((input$N %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }

      # Validate dropout and drop-in don't exceed 100%
      total_loss <- (input$dropout %||% 0.1) + (input$dropin %||% 0)
      if (total_loss > 1) {
        issues <- c(issues, "Dropout + Drop-in rates cannot exceed 100%")
      }

      # Validate allocation ratio
      if ((input$ratio %||% 1) <= 0) {
        issues <- c(issues, "Allocation ratio must be positive")
      }

      # Validate type I error
      type1 <- input$type1 %||% 0.05
      if (type1 <= 0 || type1 >= 1) {
        issues <- c(issues, "Type I error must be between 0 and 1")
      }

      # Validate standard deviations when needed
      if (input$dmeth %in% c("diff", "pct", "active")) {
        if ((input$sd0 %||% 10) <= 0) {
          issues <- c(issues, "Standard deviation must be positive")
        }
      }

      # Validate effect size ranges (where applicable)
      if (input$dmeth == "std" && !is.null(input$del)) {
        if (length(input$del) == 2 && input$del[1] > input$del[2]) {
          issues <- c(issues, "Min effect size must be <= max effect size")
        }
      }
      if (input$dmeth == "diff" && !is.null(input$dff)) {
        if (length(input$dff) == 2 && input$dff[1] > input$dff[2]) {
          issues <- c(issues, "Min difference must be <= max difference")
        }
      }
      if (input$dmeth == "pct" && !is.null(input$pct)) {
        if (length(input$pct) == 2 && input$pct[1] > input$pct[2]) {
          issues <- c(issues, "Min percent must be <= max percent")
        }
      }
      if (input$dmeth == "active" && !is.null(input$active)) {
        if (length(input$active) == 2 && input$active[1] > input$active[2]) {
          issues <- c(issues, "Min treatment change must be <= max treatment change")
        }
      }

      issues
    })

    # Display validation messages
    output$validation_messages <- shiny::renderUI({
      messages <- parameter_validation()

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

    # Granular reactive for effect size range - only depends on method and its specific inputs
    effect_size_range <- shiny::reactive({
      shiny::req(input$dmeth)

      switch(input$dmeth,
        "std" = input$del %||% c(0.2, 1.0),
        "diff" = input$dff %||% c(1, 5),
        "pct" = input$pct %||% c(0.1, 0.5),
        "active" = input$active %||% c(0, 6)
      )
    })

    # Centralized effect size calculation - only depends on range
    effect_sizes <- shiny::reactive({
      shiny::req(effect_size_range())
      consts <- ZZPOWER_CONSTANTS
      range_vec <- effect_size_range()

      if (length(range_vec) != 2) {
        return(seq(consts$COHENS_D_DEFAULT_MIN, consts$COHENS_D_DEFAULT_MAX,
                   length.out = consts$EFFECT_SIZE_SEQ_LENGTH))
      }

      seq(range_vec[1], range_vec[2], length.out = consts$EFFECT_SIZE_SEQ_LENGTH)
    })

    # Convert effect sizes to Cohen's d scale
    cohens_d <- shiny::reactive({
      shiny::req(input$dmeth, effect_sizes())

      es <- effect_sizes()

      switch(input$dmeth,
        "std" = es,
        "diff" = es / (input$sd0 %||% 10),
        "pct" = (es * (input$d0 %||% 10)) / (input$sd0 %||% 10),
        "active" = ((input$d0 %||% 10) - es) / (input$sd0 %||% 10)
      )
    })
    
    # Calculate sample sizes accounting for dropout and group ratio
    study_parameters <- shiny::reactive({
      shiny::req(input$N)

      ratio_val <- input$ratio %||% 1
      dropout_val <- input$dropout %||% 0.1
      dropin_val <- input$dropin %||% 0

      n1_comp <- ratio_val * input$N / (ratio_val + 1) *
                 (1 - (dropin_val + dropout_val))
      n2_comp <- input$N / (ratio_val + 1) *
                 (1 - (dropin_val + dropout_val))

      n1_itt <- ratio_val * input$N / (ratio_val + 1)
      n2_itt <- input$N / (ratio_val + 1)

      type1_val <- input$type1 %||% 0.05
      onesided_val <- input$onesided %||% FALSE
      sided <- if (onesided_val) 1 else 2

      list(
        n1_comp = n1_comp,
        n2_comp = n2_comp,
        n1_itt = n1_itt,
        n2_itt = n2_itt,
        sig_level = type1_val / sided
      )
    })
    
    # Calculate power for each effect size
    power_results <- shiny::reactive({
      shiny::req(cohens_d(), study_parameters())
      
      params <- study_parameters()
      d_values <- cohens_d()
      
      if (params$n1_comp <= 0 || params$n2_comp <= 0) {
        return(data.frame(
          effect_size = effect_sizes(),
          cohens_d = d_values,
          power = rep(NA, length(d_values))
        ))
      }
      
      power_values <- sapply(d_values, function(d) {
        tryCatch({
          pwr::pwr.t2n.test(
            n1 = params$n1_comp,
            n2 = params$n2_comp,
            sig.level = params$sig_level,
            d = d
          )$power
        }, error = function(e) NA)
      })
      
      data.frame(
        effect_size = effect_sizes(),
        cohens_d = d_values,
        power = power_values
      )
    })
    
    # Sample size display
    output$sample_size_display <- shiny::renderUI({
      shiny::req(study_parameters())
      params <- study_parameters()
      
      shiny::tagList(
        shiny::p(shiny::HTML(paste0(
          "<strong>ITT Sample Sizes:</strong><br>",
          "Active: ", round(params$n1_itt), 
          ", Control: ", round(params$n2_itt)
        ))),
        shiny::p(shiny::HTML(paste0(
          "<strong>Completers:</strong><br>",
          "Active: ", round(params$n1_comp), 
          ", Control: ", round(params$n2_comp)
        )))
      )
    })
    
    # Power plot
    output$power_plot <- shiny::renderPlot({
      shiny::req(power_results())
      consts <- ZZPOWER_CONSTANTS

      results <- power_results()

      if (all(is.na(results$power))) {
        plot.new()
        text(0.5, 0.5, "Invalid parameters for power calculation", cex = 1.2)
        return()
      }

      x_label <- switch(input$dmeth,
        "std" = "Effect Size (Standard Deviations)",
        "diff" = "Difference in Scores",
        "pct" = "Percent Reduction",
        "active" = "Treatment Group Change"
      )

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
          title = "Power Curve Analysis",
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
    output$results_table <- DT::renderDataTable({
      shiny::req(power_results())
      consts <- ZZPOWER_CONSTANTS

      results <- power_results()
      display_df <- results[, c("effect_size", "power")]
      colnames(display_df) <- c("Effect Size", "Power")

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
    output$summary_text <- shiny::renderText({
      shiny::req(power_results(), study_parameters())
      consts <- ZZPOWER_CONSTANTS

      results <- power_results()
      params <- study_parameters()

      # Find effect size for target power
      power_target_idx <- which.min(abs(results$power - consts$POWER_TARGET))
      power_target_es <- if (length(power_target_idx) > 0) {
        results$effect_size[power_target_idx]
      } else {
        NA
      }
      max_power <- max(results$power, na.rm = TRUE)

      type1_val <- input$type1 %||% consts$TYPE1_DEFAULT
      dropout_val <- input$dropout %||% consts$DROPOUT_DEFAULT
      onesided_val <- input$onesided %||% FALSE

      paste0(
        "Study Design Summary:\n",
        "====================\n",
        "Total Sample Size: ", input$N, "\n",
        "Dropout Rate: ", round(dropout_val * 100, 1), "%\n",
        "Effect Size Method: ", input$dmeth, "\n",
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
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        format_ext <- tolower(input$report_format)
        # Map format names to extensions
        ext <- switch(format_ext,
          "pdf" = "html",  # PDF uses HTML (save-as-PDF in browser)
          "word" = "docx",
          "html" = "html",
          "text" = "txt"
        )
        paste0("power_analysis_report_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        report_format <- tolower(input$report_format)
        # Map format names to report generation formats
        format_map <- switch(report_format,
          "pdf" = "html",
          "word" = "html",  # Generate HTML, user can save as DOCX if needed
          "html" = "html",
          "text" = "text"
        )

        report_content <- generate_power_report(
          input = input,
          power_results = power_results,
          study_parameters = study_parameters,
          format = format_map
        )

        writeLines(report_content, file)
      }
    )
  }
}