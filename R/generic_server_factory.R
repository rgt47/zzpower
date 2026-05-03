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
#' @export
create_generic_test_server <- function(id, test_spec,
                                       registry_func = get_power_test_registry) {
  shiny::moduleServer(id, function(input, output, session) {
    consts <- ZZPOWER_CONSTANTS
    param_names <- names(test_spec$parameters)
    design_names <- setdiff(param_names, "sample_size")

    fn <- test_spec$power_function
    fn_formals <- names(formals(fn))
    es_param <- intersect(c("d", "h", "r", "f"), fn_formals)[1]

    .collect_params <- function() {
      vals <- lapply(param_names, function(p) input[[p]])
      names(vals) <- param_names
      vals
    }

    .collect_design_params <- function() {
      vals <- lapply(design_names, function(p) input[[paste0("ss_", p)]])
      names(vals) <- design_names
      vals
    }

    .power_at_n <- function(es, total_n, design_params, type1, alternative) {
      params_in <- c(list(sample_size = total_n), design_params)
      params <- test_spec$sample_size_calc(params_in)

      call_args <- list(sig.level = type1)
          if ("alternative" %in% fn_formals) call_args$alternative <- alternative
      call_args[[es_param]] <- es

      if (!is.null(params$n1) && "n1" %in% fn_formals) {
        call_args$n1 <- params$n1
        call_args$n2 <- params$n2
      } else if ("n" %in% fn_formals) {
        call_args$n <- params$n
      }

      if ("type" %in% fn_formals && !is.null(test_spec$test_type)) {
        call_args$type <- test_spec$test_type
      }

      if (!is.null(test_spec$power_args)) {
        for (nm in names(test_spec$power_args)) {
          if (nm %in% fn_formals) {
            val <- test_spec$power_args[[nm]]
            if (is.character(val) && val %in% names(params)) {
              call_args[[nm]] <- params[[val]]
            } else {
              call_args[[nm]] <- val
            }
          }
        }
      }

      tryCatch({
        result <- do.call(fn, call_args)
        result$power %||% NA_real_
      }, warning = function(w) NA_real_,
         error = function(e) NA_real_)
    }

    .find_required_n <- function(es, target_power, design_params,
                                  type1, alternative) {
      n_lo <- 10
      n_hi <- 10000

      p_lo <- .power_at_n(es, n_lo, design_params, type1, alternative)
      while (is.na(p_lo) && n_lo < 100) {
        n_lo <- n_lo + 10
        p_lo <- .power_at_n(es, n_lo, design_params, type1, alternative)
      }
      if (is.na(p_lo)) return(NA_real_)

      p_hi <- .power_at_n(es, n_hi, design_params, type1, alternative)
      if (is.na(p_hi)) return(NA_real_)
      if (p_hi < target_power) return(NA_real_)
      if (p_lo >= target_power) return(n_lo)

      for (iter in seq_len(30)) {
        n_mid <- ceiling((n_lo + n_hi) / 2)
        p_mid <- .power_at_n(es, n_mid, design_params, type1, alternative)
        if (is.na(p_mid)) return(NA_real_)

        if (p_mid < target_power) n_lo <- n_mid else n_hi <- n_mid
        if (n_hi - n_lo <= 1) break
      }

      n_hi
    }

    solve_mode <- shiny::reactive({
      input$solve_for %||% "power"
    })

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
      } else {
        # Invisible spacer keeps sidebar height stable so the page
        # does not reflow when validation issues appear/disappear.
        shiny::div(style = "height: 0; visibility: hidden;",
                    " ")
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
      if (solve_mode() == "power") shiny::req(is_valid())

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

    # ===== POWER RESULTS (power mode) =====
    power_results <- shiny::reactive({
      shiny::req(solve_mode() == "power")
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

      power_vec <- vapply(standardized_es, function(es) {
        tryCatch({
          call_args <- list(sig.level = type1)
          if ("alternative" %in% fn_formals) call_args$alternative <- alternative
          call_args[[es_param]] <- es

          if (!is.null(n2) && "n1" %in% fn_formals) {
            call_args$n1 <- n1
            call_args$n2 <- n2
          } else if ("n" %in% fn_formals) {
            call_args$n <- n1
          }

          if ("type" %in% fn_formals && !is.null(test_spec$test_type)) {
            call_args$type <- test_spec$test_type
          }

          if (!is.null(test_spec$power_args)) {
            for (nm in names(test_spec$power_args)) {
              if (nm %in% fn_formals) {
                val <- test_spec$power_args[[nm]]
                if (is.character(val) && val %in% names(params)) {
                  call_args[[nm]] <- params[[val]]
                } else {
                  call_args[[nm]] <- val
                }
              }
            }
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
    }) |> shiny::bindCache(
      solve_mode(),
      effect_size_range(),
      study_parameters(),
      input$type1,
      input$onesided,
      cache = "session"
    )

    # ===== SAMPLE SIZE RESULTS (sample size mode) =====
    sample_size_results <- shiny::reactive({
      shiny::req(solve_mode() == "sample_size")
      es_range <- shiny::req(effect_size_range())

      target_power <- input$target_power %||% 0.80
      type1 <- input$type1 %||% consts$TYPE1_DEFAULT
      alternative <- if (isTRUE(input$onesided)) "one.sided" else "two.sided"
      design <- .collect_design_params()

      required_n <- vapply(es_range$standardized, function(es) {
        .find_required_n(es, target_power, design, type1, alternative)
      }, FUN.VALUE = numeric(1))

      data.frame(
        effect_size = es_range$effect_sizes,
        standardized_es = es_range$standardized,
        required_n = required_n
      )
    }) |> shiny::bindCache(
      solve_mode(),
      effect_size_range(),
      input$target_power,
      input$type1,
      input$onesided,
      .collect_design_params(),
      cache = "session"
    )

    # ===== HEADLINE VALUE-BOX HELPERS =====
    .headline_es_label <- function(es_method) {
      switch(es_method %||% "",
        "cohens_d" = "d", "percent_reduction" = "%",
        "difference" = "diff", "active_change" = "change",
        "proportions" = "p1", "odds_ratio" = "OR",
        "relative_risk" = "RR", "correlation" = "r",
        "hazard_ratio" = "HR", "prop_range" = "p",
        "ES")
    }

    headline_data <- shiny::reactive({
      shiny::req(is_valid())
      mode <- solve_mode()
      es_range <- shiny::req(effect_size_range())
      es_label <- .headline_es_label(es_range$method)

      if (mode == "power") {
        results <- shiny::req(power_results())
        ok <- results[!is.na(results$power), , drop = FALSE]
        if (nrow(ok) == 0L) return(NULL)

        max_idx <- which.max(ok$power)
        cross_idx <- which(ok$power >= 0.80)[1]

        list(
          mode = "power",
          es_label = es_label,
          max_power = ok$power[max_idx],
          max_power_es = ok$effect_size[max_idx],
          es_at_80 = if (length(cross_idx) > 0L && !is.na(cross_idx)) {
            ok$effect_size[cross_idx]
          } else {
            NA_real_
          },
          n_total = study_parameters()$n_total %||%
            study_parameters()$n %||% NA_real_
        )
      } else {
        results <- shiny::req(sample_size_results())
        ok <- results[!is.na(results$required_n), , drop = FALSE]
        if (nrow(ok) == 0L) return(NULL)

        list(
          mode = "sample_size",
          es_label = es_label,
          n_min = min(ok$required_n),
          n_min_es = ok$effect_size[which.min(ok$required_n)],
          n_max = max(ok$required_n),
          n_max_es = ok$effect_size[which.max(ok$required_n)],
          target_power = input$target_power %||% 0.80
        )
      }
    })

    output$headline_box1_title <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") "Effect size for 80% power" else "Smallest N required"
    })
    output$headline_box1_value <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") {
        if (is.na(d$es_at_80)) "Not reached" else
          sprintf("%s = %.3f", d$es_label, d$es_at_80)
      } else {
        sprintf("N = %.0f (at %s = %.3f)",
                d$n_min, d$es_label, d$n_min_es)
      }
    })

    output$headline_box2_title <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") "Maximum power in range" else "Largest N required"
    })
    output$headline_box2_value <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") {
        sprintf("%.1f%% (at %s = %.3f)",
                d$max_power * 100, d$es_label, d$max_power_es)
      } else {
        sprintf("N = %.0f (at %s = %.3f)",
                d$n_max, d$es_label, d$n_max_es)
      }
    })

    output$headline_box3_title <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") "Total sample size" else "Target power"
    })
    output$headline_box3_value <- shiny::renderText({
      d <- shiny::req(headline_data(), cancelOutput = TRUE)
      if (is.null(d)) return("--")
      if (d$mode == "power") {
        if (is.na(d$n_total)) "(see inputs)" else
          sprintf("N = %.0f", d$n_total)
      } else {
        sprintf("%.0f%%", d$target_power * 100)
      }
    })

    # ===== X-AXIS LABEL HELPER =====
    .es_x_label <- function(method) {
      switch(method,
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
    }

    # ===== PLOT OUTPUT =====
    output$power_plot <- shiny::renderPlot({
      # Keep the previous plot on screen while inputs are in
      # transient flight. Only show the validation message if the
      # initial state is invalid (no prior plot to keep).
      shiny::req(is_valid(), cancelOutput = TRUE)
      mode <- solve_mode()
      es_range <- shiny::req(effect_size_range(), cancelOutput = TRUE)
      x_label <- .es_x_label(es_range$method)

      if (mode == "power") {
        results <- shiny::req(power_results(), cancelOutput = TRUE)
        results <- results[!is.na(results$power), , drop = FALSE]
        shiny::req(nrow(results) > 0, cancelOutput = TRUE)

        # Identify the smallest effect size that reaches 80% power
        # for an in-line annotation.
        cross_idx <- which(results$power >= 0.80)[1]
        annot_layers <- if (!is.na(cross_idx) && length(cross_idx)) {
          x80 <- results$effect_size[cross_idx]
          y80 <- results$power[cross_idx]
          list(
            ggplot2::geom_point(
              data = data.frame(x = x80, y = y80),
              ggplot2::aes(x = .data$x, y = .data$y),
              inherit.aes = FALSE,
              size = 4, color = "#C69214"
            ),
            ggplot2::annotate(
              "label", x = x80, y = y80,
              label = sprintf("80%% at %.3f", x80),
              hjust = -0.1, vjust = 1.4, size = 3.4,
              fill = "#FFF7E0",
              colour = "#7A5A00",
              label.size = 0
            )
          )
        } else {
          list()
        }

        ggplot2::ggplot(
          results,
          ggplot2::aes(x = .data$effect_size, y = .data$power)
        ) +
          ggplot2::geom_line(linewidth = 1, color = "#00629B") +
          ggplot2::geom_point(size = 2, color = "#00629B") +
          ggplot2::geom_hline(
            yintercept = 0.8, linetype = "dashed",
            color = "#C69214", linewidth = 0.5
          ) +
          annot_layers +
          ggplot2::labs(
            title = paste("Power Curve -", test_spec$name),
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
          ggplot2::scale_y_continuous(
            limits = c(0, 1),
            labels = function(x) paste0(round(x * 100), "%")
          )

      } else {
        results <- shiny::req(sample_size_results(),
                                cancelOutput = TRUE)
        results <- results[!is.na(results$required_n), , drop = FALSE]
        shiny::req(nrow(results) > 0, cancelOutput = TRUE)
        target <- input$target_power %||% 0.80

        # Annotate the smallest required-N point so the answer is
        # readable directly off the curve.
        min_idx <- which.min(results$required_n)
        annot_layers <- if (length(min_idx) && !is.na(min_idx)) {
          xm <- results$effect_size[min_idx]
          ym <- results$required_n[min_idx]
          list(
            ggplot2::geom_point(
              data = data.frame(x = xm, y = ym),
              ggplot2::aes(x = .data$x, y = .data$y),
              inherit.aes = FALSE,
              size = 4, color = "#C69214"
            ),
            ggplot2::annotate(
              "label", x = xm, y = ym,
              label = sprintf("min N = %.0f", ym),
              hjust = -0.1, vjust = 1.4, size = 3.4,
              fill = "#FFF7E0",
              colour = "#7A5A00",
              label.size = 0
            )
          )
        } else {
          list()
        }

        ggplot2::ggplot(
          results,
          ggplot2::aes(x = .data$effect_size, y = .data$required_n)
        ) +
          ggplot2::geom_line(linewidth = 1, color = "#00629B") +
          ggplot2::geom_point(size = 2, color = "#00629B") +
          annot_layers +
          ggplot2::labs(
            title = paste("Required Sample Size -", test_spec$name),
            x = x_label,
            y = "Total Sample Size (N)",
            caption = sprintf("Target power = %.0f%%", target * 100)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            axis.title = ggplot2::element_text(size = 12),
            panel.grid.minor = ggplot2::element_blank()
          )
      }
    })

    # ===== RESULTS TABLE =====
    output$results_table <- DT::renderDT({
      shiny::req(is_valid(), cancelOutput = TRUE)
      mode <- solve_mode()

      if (mode == "power") {
        results <- shiny::req(power_results(), cancelOutput = TRUE)
        display_df <- data.frame(
          `Effect Size` = results$effect_size,
          `Standardized` = results$standardized_es,
          `Power` = results$power,
          check.names = FALSE
        )
        dt <- DT::datatable(
          display_df,
          options = list(pageLength = nrow(display_df), dom = "t"),
          rownames = FALSE
        )
        DT::formatRound(dt, c("Effect Size", "Standardized"), 3) |>
          DT::formatPercentage("Power", 1)
      } else {
        results <- shiny::req(sample_size_results(),
                                cancelOutput = TRUE)
        display_df <- data.frame(
          `Effect Size` = results$effect_size,
          `Standardized` = results$standardized_es,
          `Required N` = results$required_n,
          check.names = FALSE
        )
        dt <- DT::datatable(
          display_df,
          options = list(pageLength = nrow(display_df), dom = "t"),
          rownames = FALSE
        )
        DT::formatRound(dt, c("Effect Size", "Standardized"), 3) |>
          DT::formatRound("Required N", 0)
      }
    })

    # ===== STUDY SUMMARY =====
    .summary_dt_row <- function(label, value) {
      shiny::tagList(
        shiny::tags$dt(class = "col-sm-5 text-muted small fw-normal",
                        label),
        shiny::tags$dd(class = "col-sm-7 small mb-1", value)
      )
    }

    output$summary <- shiny::renderUI({
      shiny::req(is_valid(), cancelOutput = TRUE)
      mode <- solve_mode()
      es_range <- shiny::req(effect_size_range(),
                              cancelOutput = TRUE)

      type1 <- input$type1 %||% consts$TYPE1_DEFAULT
      one_sided <- isTRUE(input$onesided)

      mode_label <- if (mode == "power") {
        "Solve for power"
      } else {
        "Solve for sample size"
      }

      rows <- list(
        .summary_dt_row("Test", test_spec$name),
        .summary_dt_row("Mode", mode_label),
        .summary_dt_row("Effect-size method", es_range$method),
        .summary_dt_row(
          "Effect-size range",
          sprintf("%.3f to %.3f",
                   min(es_range$effect_sizes),
                   max(es_range$effect_sizes))
        )
      )

      if (mode == "power") {
        params <- shiny::req(study_parameters())
        if (!is.null(params$n1) && !is.null(params$n2)) {
          rows <- c(rows, list(
            .summary_dt_row("Group 1 (n)", sprintf("%.0f", params$n1)),
            .summary_dt_row("Group 2 (n)", sprintf("%.0f", params$n2)),
            .summary_dt_row("Total (N)",
                             sprintf("%.0f", params$n1 + params$n2))
          ))
        } else if (!is.null(params$n)) {
          rows <- c(rows, list(
            .summary_dt_row("Sample size (n)",
                             sprintf("%.0f", params$n))
          ))
        }
      } else {
        target <- input$target_power %||% 0.80
        rows <- c(rows, list(
          .summary_dt_row("Target power",
                           sprintf("%.0f%%", target * 100))
        ))
      }

      rows <- c(rows, list(
        .summary_dt_row("Type I error (alpha)",
                         sprintf("%.4f", type1)),
        .summary_dt_row("Test direction",
                         if (one_sided) "One-sided" else "Two-sided")
      ))

      shiny::tags$dl(class = "row mb-0", rows)
    })

    # ===== REPORT DOWNLOAD =====
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$report_format %||% "text"
        ext <- ifelse(fmt == "html", "html", "txt")
        paste0("power_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        mode <- solve_mode()
        fmt <- input$report_format %||% "text"

        if (mode == "power") {
          results <- shiny::req(power_results())
          params <- study_parameters()
        } else {
          results <- shiny::req(sample_size_results())
          params <- NULL
        }

        es_range <- effect_size_range()

        report_data <- list(
          test_id = id,
          test_name = test_spec$name,
          format = fmt,
          solve_mode = mode,
          timestamp = Sys.time(),
          r_version = paste(R.version$major, R.version$minor, sep = "."),
          parameters = .collect_params(),
          sample_sizes = params,
          effect_size_range = es_range,
          power_results = results,
          target_power = input$target_power %||% 0.80,
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
