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
    
    # Initialize default values
    shiny::observe({
      # Set default values for conditional inputs
      if (is.null(input$sd0)) {
        shiny::updateNumericInput(session, "sd0", value = 10)
      }
      if (is.null(input$d0)) {
        shiny::updateNumericInput(session, "d0", value = 10)
      }
      if (is.null(input$ratio)) {
        shiny::updateNumericInput(session, "ratio", value = 1)
      }
      if (is.null(input$type1)) {
        shiny::updateNumericInput(session, "type1", value = 0.05)
      }
      if (is.null(input$dropin)) {
        shiny::updateSliderInput(session, "dropin", value = 0)
      }
    })
    
    # Centralized effect size calculation
    effect_sizes <- shiny::reactive({
      shiny::req(input$dmeth)
      
      range_vec <- switch(input$dmeth,
        "std" = if (!is.null(input$del)) input$del else c(0.2, 1.0),
        "diff" = if (!is.null(input$dff)) input$dff else c(1, 5),  
        "pct" = if (!is.null(input$pct)) input$pct else c(0.1, 0.5),
        "active" = if (!is.null(input$active)) input$active else c(0, 6)
      )
      
      if (length(range_vec) != 2) return(seq(0.2, 1.0, length.out = 16))
      
      seq(range_vec[1], range_vec[2], length.out = 16)
    })
    
    # Convert effect sizes to Cohen's d scale
    cohens_d <- shiny::reactive({
      shiny::req(input$dmeth, effect_sizes())
      
      es <- effect_sizes()
      
      switch(input$dmeth,
        "std" = es,
        "diff" = {
          sd_val <- if (!is.null(input$sd0)) input$sd0 else 10
          es / sd_val
        },
        "pct" = {
          d0_val <- if (!is.null(input$d0)) input$d0 else 10
          sd_val <- if (!is.null(input$sd0)) input$sd0 else 10
          (es * d0_val) / sd_val
        },
        "active" = {
          d0_val <- if (!is.null(input$d0)) input$d0 else 10
          sd_val <- if (!is.null(input$sd0)) input$sd0 else 10
          (d0_val - es) / sd_val
        }
      )
    })
    
    # Calculate sample sizes accounting for dropout and group ratio
    study_parameters <- shiny::reactive({
      shiny::req(input$N)
      
      ratio_val <- if (!is.null(input$ratio)) input$ratio else 1
      dropout_val <- if (!is.null(input$dropout)) input$dropout else 0.1
      dropin_val <- if (!is.null(input$dropin)) input$dropin else 0
      
      n1_comp <- ratio_val * input$N / (ratio_val + 1) * 
                 (1 - (dropin_val + dropout_val))
      n2_comp <- input$N / (ratio_val + 1) * 
                 (1 - (dropin_val + dropout_val))
      
      n1_itt <- ratio_val * input$N / (ratio_val + 1)
      n2_itt <- input$N / (ratio_val + 1)
      
      type1_val <- if (!is.null(input$type1)) input$type1 else 0.05
      onesided_val <- if (!is.null(input$onesided)) input$onesided else FALSE
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
      
      # Find effect size for 80% power
      power_80_idx <- which.min(abs(results$power - 0.8))
      power_80_es <- if (length(power_80_idx) > 0) results$effect_size[power_80_idx] else NA
      
      p <- ggplot2::ggplot(results, ggplot2::aes(x = .data$effect_size, y = .data$power)) +
        ggplot2::geom_line(color = "blue", size = 1) +
        ggplot2::geom_hline(yintercept = 0.8, color = "red", linetype = "dashed") +
        ggplot2::labs(
          title = "Power Curve Analysis",
          x = x_label,
          y = "Statistical Power"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
        ggplot2::theme_minimal()
      
      if (!is.na(power_80_es)) {
        p <- p + ggplot2::geom_vline(xintercept = power_80_es, color = "red", linetype = "dotted")
      }
      
      p
    })
    
    # Results table
    output$results_table <- DT::renderDataTable({
      shiny::req(power_results())
      
      results <- power_results()
      display_df <- results[, c("effect_size", "power")]
      colnames(display_df) <- c("Effect Size", "Power")
      
      DT::datatable(
        display_df,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = 1:2, digits = 3)
    })
    
    # Summary text
    output$summary_text <- shiny::renderText({
      shiny::req(power_results(), study_parameters())
      
      results <- power_results()
      params <- study_parameters()
      
      # Find effect size for 80% power
      power_80_idx <- which.min(abs(results$power - 0.8))
      power_80_es <- if (length(power_80_idx) > 0) results$effect_size[power_80_idx] else NA
      max_power <- max(results$power, na.rm = TRUE)
      
      type1_val <- if (!is.null(input$type1)) input$type1 else 0.05
      dropout_val <- if (!is.null(input$dropout)) input$dropout else 0.1
      onesided_val <- if (!is.null(input$onesided)) input$onesided else FALSE
      
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
        "Maximum Power: ", round(max_power, 3), "\n",
        if (!is.na(power_80_es)) {
          paste0("Effect Size for 80% Power: ", round(power_80_es, 3))
        } else {
          "Effect Size for 80% Power: Not achievable in tested range"
        }
      )
    })
    
    # Report download
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("power_analysis_report_", Sys.Date(), ".", 
               tolower(input$report_format))
      },
      content = function(file) {
        # Simple text report
        results <- power_results()
        params <- study_parameters()
        
        report_lines <- c(
          "Power Analysis Report",
          "Generated: ", as.character(Sys.time()),
          "",
          "Study Parameters:",
          paste("Total Sample Size:", input$N),
          paste("Dropout Rate:", round((if (!is.null(input$dropout)) input$dropout else 0.1) * 100, 1), "%"),
          paste("Effect Size Method:", input$dmeth),
          "",
          "Results:",
          paste("Effect Size Range:", 
                round(min(results$effect_size), 3), "to", 
                round(max(results$effect_size), 3)),
          paste("Power Range:", 
                round(min(results$power, na.rm = TRUE), 3), "to",
                round(max(results$power, na.rm = TRUE), 3))
        )
        
        writeLines(report_lines, file)
      }
    )
  }
}