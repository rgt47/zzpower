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
    ns <- session$ns
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

    # ===== METHODS PARAGRAPH (Gap 1) =====
    # Build a calc_context using the user's currently-selected
    # design + the *lower end* of the effect-size range (the most
    # conservative assumption, which produces the largest N).
    # Then thread in the Wave 1 provenance fields and render the
    # Glueck-Muller-shaped paragraph.
    methods_paragraph_ctx <- shiny::reactive({
      shiny::req(is_valid())

      spec   <- test_spec
      method <- input$effect_method %||% spec$effect_size_methods[1]
      method_params <- spec$effect_size_params[[method]]

      es_min <- input[[paste0(method, "_es")]][1] %||%
                  method_params$default_min

      design <- .collect_params()
      if (!is.null(method_params$requires)) {
        for (param_name in names(method_params$requires)) {
          val <- input[[paste0(method, "_", param_name)]]
          if (!is.null(val)) design[[param_name]] <- val
        }
      }

      alpha <- input$type1 %||% consts$TYPE1_DEFAULT
      alternative <- if (isTRUE(input$onesided)) {
        "one.sided"
      } else {
        "two.sided"
      }

      pc_args <- c(
        list(
          test          = spec$id,
          effect_size   = es_min,
          effect_method = method,
          alpha         = alpha,
          alternative   = alternative
        ),
        design[setdiff(names(design), "sample_size")]
      )

      if (solve_mode() == "sample_size") {
        pc_args$target_power <- input$target_power %||% 0.80
      } else {
        pc_args$sample_size <- input$sample_size
      }

      ctx <- do.call(power_calc, pc_args)

      ctx$effect_source         <- input$effect_source %||% ""
      ctx$effect_doi            <- input$effect_doi %||% ""
      ctx$sensitivity_factor    <- input$sensitivity_factor
      ctx$include_sex_paragraph <- isTRUE(input$include_sex_paragraph)

      ctx
    })

    output$methods_paragraph_text <- shiny::renderText({
      ctx <- methods_paragraph_ctx()
      .render_methods_paragraph(ctx)
    })

    # ===== SENSITIVITY TABLE (Gap 2) =====
    # Editable effect-size column seeded from the registry's
    # default_effect_grid; the four N columns recompute whenever
    # the effect column or any design input changes.
    sensitivity_grid    <- shiny::reactiveVal(NULL)
    sensitivity_method  <- shiny::reactiveVal(NULL)

    shiny::observe({
      method <- input$effect_method %||%
                  test_spec$effect_size_methods[1]
      cur_method <- shiny::isolate(sensitivity_method())

      # Re-seed when the test panel first opens or the user
      # switches effect-size method (Cohen's d -> difference, etc).
      if (is.null(sensitivity_grid()) ||
          !identical(cur_method, method)) {
        seed <- test_spec$default_effect_grid[[method]] %||%
                  c(0.2, 0.5, 0.8)
        sensitivity_grid(as.numeric(seed))
        sensitivity_method(method)
      }
    })

    sensitivity_table_df <- shiny::reactive({
      shiny::req(is_valid())
      grid <- sensitivity_grid()
      shiny::req(grid, length(grid) > 0)

      method <- input$effect_method %||%
                  test_spec$effect_size_methods[1]
      method_params <- test_spec$effect_size_params[[method]]

      design <- .collect_params()
      if (!is.null(method_params$requires)) {
        for (param_name in names(method_params$requires)) {
          val <- input[[paste0(method, "_", param_name)]]
          if (!is.null(val)) design[[param_name]] <- val
        }
      }

      alpha <- input$type1 %||% consts$TYPE1_DEFAULT
      alternative <- if (isTRUE(input$onesided)) {
        "one.sided"
      } else {
        "two.sided"
      }

      args <- c(
        list(
          test             = test_spec$id,
          effect_grid      = grid,
          effect_method    = method,
          power_thresholds = c(0.80, 0.90),
          alpha            = alpha,
          alternative      = alternative
        ),
        design[setdiff(names(design), "sample_size")]
      )

      tryCatch(do.call(power_table, args),
               error = function(e) NULL)
    })

    output$sensitivity_table <- DT::renderDT({
      df <- sensitivity_table_df()
      shiny::req(df)

      display <- data.frame(
        `Effect size`        = df$effect_size,
        `Standardised`       = round(df$effect_size_std, 3),
        `N enrolled @ 80%`   = round(df$n_total_enrolled_p80),
        `N evaluable @ 80%`  = round(df$n_total_evaluable_p80),
        `N enrolled @ 90%`   = round(df$n_total_enrolled_p90),
        `N evaluable @ 90%`  = round(df$n_total_evaluable_p90),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        display,
        editable = list(target = "cell",
                        disable = list(columns = 1:5)),
        options = list(
          dom = "t", paging = FALSE,
          searching = FALSE, info = FALSE,
          columnDefs = list(
            list(className = "dt-left",  targets = 0),
            list(className = "dt-right", targets = 1:5)
          )
        ),
        rownames = FALSE,
        selection = "none"
      )
    })

    shiny::observeEvent(input$sensitivity_table_cell_edit, {
      edit <- input$sensitivity_table_cell_edit
      grid <- sensitivity_grid()

      # DT reports col == 0 for the (only editable) effect-size
      # column; row is 0-indexed.
      if (!is.null(edit) && identical(edit$col, 0L)) {
        new_val <- suppressWarnings(as.numeric(edit$value))
        idx <- edit$row + 1L
        if (!is.na(new_val) && new_val > 0 &&
            idx >= 1L && idx <= length(grid)) {
          grid[idx] <- new_val
          sensitivity_grid(grid)
        }
      }
    })

    output$download_sensitivity_csv <- shiny::downloadHandler(
      filename = function() {
        sprintf("zzpower_sensitivity_%s_%s.csv",
                test_spec$id,
                format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        df <- sensitivity_table_df()
        if (is.null(df)) return(NULL)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$download_sensitivity_md <- shiny::downloadHandler(
      filename = function() {
        sprintf("zzpower_sensitivity_%s_%s.md",
                test_spec$id,
                format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        df <- sensitivity_table_df()
        if (is.null(df)) return(NULL)
        # Caption with footnote per Sec.2.3 conventions.
        method <- input$effect_method %||%
                    test_spec$effect_size_methods[1]
        alt_str <- if (isTRUE(input$onesided)) {
          "one-sided"
        } else {
          "two-sided"
        }
        cap <- sprintf(
          paste0("Sample sizes required to achieve 80%% and 90%% ",
                 "power across plausible effect sizes (%s). ",
                 "Calculations assume a %s test, alpha = %s. ",
                 "Computed using zzpower (Thomas, R.G. 2026); ",
                 "method follows %s."),
          method, alt_str,
          format(input$type1 %||% 0.05),
          test_spec$formula_citation %||% "--"
        )
        md <- .df_to_markdown(df, caption = cap)
        writeLines(md, file)
      }
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
    .build_power_ggplot <- function() {
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

        # Identify the smallest effect size that reaches 80% (gold,
        # primary) and 90% (gray, secondary) power thresholds.
        cross_80 <- which(results$power >= 0.80)[1]
        cross_90 <- which(results$power >= 0.90)[1]

        layer_80 <- if (!is.na(cross_80) && length(cross_80)) {
          x80 <- results$effect_size[cross_80]
          y80 <- results$power[cross_80]
          df_80 <- data.frame(
            x = x80, y = y80,
            lbl = sprintf("80%% at %.3f", x80),
            stringsAsFactors = FALSE
          )
          list(
            ggplot2::geom_point(
              data = df_80,
              mapping = ggplot2::aes(x = x, y = y),
              inherit.aes = FALSE,
              size = 4, color = "#C69214"
            ),
            ggplot2::geom_label(
              data = df_80,
              mapping = ggplot2::aes(x = x, y = y, label = lbl),
              inherit.aes = FALSE,
              hjust = -0.1, vjust = 1.4, size = 3.4,
              fill = "#FFF7E0",
              colour = "#7A5A00",
              label.size = 0
            )
          )
        } else {
          list()
        }

        layer_90 <- if (!is.na(cross_90) && length(cross_90)) {
          x90 <- results$effect_size[cross_90]
          y90 <- results$power[cross_90]
          df_90 <- data.frame(
            x = x90, y = y90,
            lbl = sprintf("90%% at %.3f", x90),
            stringsAsFactors = FALSE
          )
          list(
            ggplot2::geom_point(
              data = df_90,
              mapping = ggplot2::aes(x = x, y = y),
              inherit.aes = FALSE,
              size = 3, color = "#9aa0a6", alpha = 0.7
            ),
            ggplot2::geom_label(
              data = df_90,
              mapping = ggplot2::aes(x = x, y = y, label = lbl),
              inherit.aes = FALSE,
              hjust = -0.1, vjust = 1.4, size = 3.0,
              fill = "#F2F3F4",
              colour = "#5f6368", alpha = 0.85,
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
            yintercept = 0.9, linetype = "dotted",
            color = "#9aa0a6", linewidth = 0.4, alpha = 0.7
          ) +
          ggplot2::geom_hline(
            yintercept = 0.8, linetype = "dashed",
            color = "#C69214", linewidth = 0.5
          ) +
          layer_90 +
          layer_80 +
          ggplot2::labs(
            title = paste("Power Curve -", test_spec$name),
            x = x_label,
            y = "Statistical Power",
            caption = paste(
              "Dashed gold = 80% threshold;",
              "dotted gray = 90% threshold"
            )
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
          df_min <- data.frame(
            x = xm, y = ym,
            lbl = sprintf("min N = %.0f", ym),
            stringsAsFactors = FALSE
          )
          list(
            ggplot2::geom_point(
              data = df_min,
              mapping = ggplot2::aes(x = x, y = y),
              inherit.aes = FALSE,
              size = 4, color = "#C69214"
            ),
            ggplot2::geom_label(
              data = df_min,
              mapping = ggplot2::aes(x = x, y = y, label = lbl),
              inherit.aes = FALSE,
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
    }

    output$power_plot <- shiny::renderPlot({
      .build_power_ggplot()
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

    # ===== TYPST INSTALL PROMPT =====
    # When the user switches report format to PDF, offer to install
    # the 'typst' package (and its CLI binary) on the fly. Asking
    # once per session is sufficient.
    typst_prompt_shown <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$report_format, {
      if (identical(input$report_format, "pdf") &&
          !requireNamespace("typst", quietly = TRUE) &&
          !isTRUE(typst_prompt_shown())) {
        typst_prompt_shown(TRUE)
        shiny::showModal(shiny::modalDialog(
          title = "Install Typst for fast PDF rendering?",
          easyClose = TRUE,
          shiny::p(
            "PDF reports use Typst, a modern typesetting engine.",
            "The R package 'typst' is not yet installed."
          ),
          shiny::p(
            "Click ", shiny::strong("Install"), " to fetch it from",
            "CRAN and download the Typst CLI binary",
            "(roughly 30 MB, ~1 minute). The Shiny session will",
            "pause briefly during the install."
          ),
          shiny::p(
            class = "text-muted small",
            "If you skip, PDF will fall back to a LaTeX render",
            "(requires tinytex / MacTeX). You can change this",
            "decision later by re-selecting PDF."
          ),
          footer = shiny::tagList(
            shiny::modalButton("Skip"),
            shiny::actionButton(
              ns("install_typst_now"),
              "Install Typst",
              class = "btn-primary",
              icon = shiny::icon("download")
            )
          )
        ))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$install_typst_now, {
      shiny::removeModal()
      shiny::withProgress(
        message = "Installing typst R package...",
        value = 0.1,
        {
          # Step 1: ensure a CRAN mirror is set.
          repos <- getOption("repos")
          if (is.null(repos) ||
              identical(unname(repos["CRAN"]), "@CRAN@")) {
            options(repos = c(CRAN = "https://cloud.r-project.org"))
          }

          ok_pkg <- tryCatch({
            utils::install.packages("typst", quiet = TRUE)
            requireNamespace("typst", quietly = TRUE)
          }, error = function(e) {
            message("install.packages('typst') failed: ", e$message)
            FALSE
          })

          if (!ok_pkg) {
            shiny::showNotification(
              paste(
                "Could not install the 'typst' R package.",
                "PDF will fall back to LaTeX."
              ),
              type = "error", duration = 8
            )
            return(invisible(NULL))
          }

          shiny::setProgress(
            value = 0.5,
            message = "Downloading Typst CLI binary..."
          )
          ok_bin <- tryCatch({
            typst::install_typst()
            TRUE
          }, error = function(e) {
            message("typst::install_typst() failed: ", e$message)
            FALSE
          })

          if (ok_bin) {
            shiny::showNotification(
              "Typst installed. PDF will use Typst from now on.",
              type = "message", duration = 6
            )
          } else {
            shiny::showNotification(
              paste(
                "Installed the R package but the Typst CLI",
                "binary did not download. PDF will fall back",
                "to LaTeX."
              ),
              type = "warning", duration = 8
            )
          }
        }
      )
    })

    # ===== REPORT DOWNLOAD =====
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$report_format %||% "text"
        ext <- switch(fmt,
                       html = "html",
                       pdf  = "pdf",
                       word = "docx",
                       "txt")
        paste0(
          "power_report_",
          format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext
        )
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

        if (fmt %in% c("pdf", "word")) {
          rendered <- .render_generic_report_binary(
            report_data, test_spec, fmt
          )
          if (is.null(rendered)) {
            shiny::showNotification(
              paste(
                "PDF/Word export requires the 'rmarkdown' package",
                "and (for PDF) a working LaTeX install.",
                "Falling back to a text report."
              ),
              type = "warning", duration = 8
            )
            report_data$format <- "text"
            writeLines(
              .generate_generic_report(report_data, test_spec),
              file
            )
          } else {
            file.copy(rendered, file, overwrite = TRUE)
          }
        } else {
          report_content <- .generate_generic_report(
            report_data, test_spec
          )
          writeLines(report_content, file)
        }
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

#' Markdown body suitable for rmarkdown rendering to PDF or Word
#'
#' Produces a self-contained markdown document. PDF uses
#' `pdf_document` (requires LaTeX); Word uses `word_document`.
#'
#' @keywords internal
#' Reshape the report_data list into a minimal calc_context
#'
#' The Wave 3 reproducibility-script renderer (`.render_repro_script`)
#' consumes a calc_context. Reports already have all the same
#' information in the report_data list shape, so this helper just
#' rewires the names. Returns NULL if the report lacks the data
#' needed (e.g. sample-size mode with no closed-form sample_sizes).
#'
#' @keywords internal
.report_data_to_ctx <- function(report_data) {
  ss <- report_data$sample_sizes
  es_range <- report_data$effect_size_range

  if (is.null(es_range)) return(NULL)

  # Lower end of the standardized effect-size range (most
  # conservative; matches the methods-paragraph convention).
  std <- es_range$standardized
  if (length(std) == 0) return(NULL)
  std <- std[!is.na(std)]
  if (length(std) == 0) return(NULL)
  ix_min <- which.min(abs(std))
  es_std <- std[ix_min]
  es_raw <- es_range$effect_sizes[ix_min]

  # Find achieved power at this effect (power mode), if available.
  achieved <- NULL
  pr <- report_data$power_results
  if (!is.null(pr) && "power" %in% names(pr)) {
    diffs <- abs(pr$standardized_es - es_std)
    j <- which.min(diffs)
    if (length(j)) achieved <- pr$power[j]
  }

  # In sample-size mode the report does not carry a single
  # sample_sizes row -- synthesise one at the smallest required N
  # for the lower-end effect.
  if (is.null(ss) && !is.null(pr) && "required_n" %in% names(pr)) {
    j <- which.min(abs(pr$standardized_es - es_std))
    if (length(j) && !is.na(pr$required_n[j])) {
      reg <- get_power_test_registry()
      spec <- reg[[report_data$test_id]]
      design <- report_data$parameters %||% list()
      ss <- spec$sample_size_calc(c(
        list(sample_size = pr$required_n[j]),
        design[setdiff(names(design), "sample_size")]
      ))
    }
  }
  if (is.null(ss)) return(NULL)

  list(
    test_id         = report_data$test_id,
    sample_sizes    = ss,
    effect_size     = es_raw,
    effect_size_std = es_std,
    alpha           = report_data$type1_error,
    alternative     = if (isTRUE(report_data$one_sided)) {
                        "one.sided"
                      } else {
                        "two.sided"
                      },
    achieved_power  = achieved,
    target_power    = report_data$target_power
  )
}

.format_generic_md_report <- function(report_data, test_spec) {
  ts <- format(report_data$timestamp, "%Y-%m-%d %H:%M:%S")
  parts <- c(
    sprintf("# Power Analysis Report: %s", test_spec$name),
    "",
    sprintf("**Generated:** %s  ", ts),
    sprintf("**R version:** %s  ", report_data$r_version),
    sprintf("**Solve mode:** %s",
             if (identical(report_data$solve_mode, "sample_size")) {
               "Solve for sample size"
             } else {
               "Solve for power"
             }),
    "",
    "## Test specification",
    "",
    sprintf("- **Test type:** %s", test_spec$name),
    sprintf("- **Description:** %s", test_spec$description),
    "",
    "## Effect-size method",
    "",
    sprintf("- **Method:** %s", report_data$effect_size_range$method),
    sprintf(
      "- **Range:** %.4f to %.4f",
      min(report_data$effect_size_range$effect_sizes),
      max(report_data$effect_size_range$effect_sizes)
    ),
    "",
    "## Sample size",
    ""
  )

  if (!is.null(report_data$sample_sizes$n1) &&
      !is.null(report_data$sample_sizes$n2)) {
    parts <- c(parts,
      sprintf("- **Group 1 (n1):** %.0f",
               report_data$sample_sizes$n1),
      sprintf("- **Group 2 (n2):** %.0f",
               report_data$sample_sizes$n2),
      sprintf("- **Total (N):** %.0f",
               report_data$sample_sizes$n1 +
                 report_data$sample_sizes$n2),
      ""
    )
  } else if (!is.null(report_data$sample_sizes$n)) {
    parts <- c(parts,
      sprintf("- **Sample size (n):** %.0f",
               report_data$sample_sizes$n),
      ""
    )
  } else {
    parts <- c(parts, "_(Sample-size search results below.)_", "")
  }

  parts <- c(parts,
    "## Statistical parameters",
    "",
    sprintf("- **Type I error (alpha):** %.4f",
             report_data$type1_error),
    sprintf("- **Test direction:** %s",
             if (isTRUE(report_data$one_sided)) "One-sided" else
               "Two-sided"),
    "",
    "## Power-analysis results",
    "",
    "| Effect Size | Standardized | Power |",
    "|---:|---:|---:|"
  )

  parts <- c(parts, vapply(
    seq_len(nrow(report_data$power_results)),
    function(i) {
      r <- report_data$power_results[i, ]
      power_col <- if ("power" %in% names(r)) {
        sprintf("%.4f", r$power)
      } else if ("required_n" %in% names(r)) {
        if (is.na(r$required_n)) "NA" else
          sprintf("%.0f (N)", r$required_n)
      } else {
        "--"
      }
      sprintf("| %.4f | %.4f | %s |",
               r$effect_size, r$standardized_es, power_col)
    },
    character(1)
  ))

  # Gap 9: reproducibility R script section.
  ctx <- .report_data_to_ctx(report_data)
  if (!is.null(ctx)) {
    repro <- .render_repro_script(ctx, fence = TRUE)
    if (nzchar(repro)) {
      parts <- c(parts,
        "",
        "## Reproducibility script",
        "",
        paste0("Run this block in a fresh R session with `pwr` ",
               "(and, for non-pwr tests, `zzpower`) installed to ",
               "exactly reproduce the headline calculation:"),
        "",
        repro
      )
    }
  }

  parts <- c(parts,
    "",
    "---",
    "",
    sprintf("*Citation:* %s", .zzpower_citation_line())
  )

  paste(parts, collapse = "\n")
}

#' Typst source for PDF rendering
#'
#' Produces a self-contained Typst document. Typst is preferred over
#' LaTeX for the PDF backend because it has no TeX-Live install
#' burden and renders an order of magnitude faster.
#'
#' @keywords internal
.format_generic_typst_report <- function(report_data, test_spec) {
  esc <- function(s) {
    s <- as.character(s)
    s <- gsub("\\\\", "\\\\\\\\", s)
    s <- gsub("\"", "\\\\\"", s)
    s
  }
  ts <- format(report_data$timestamp, "%Y-%m-%d %H:%M:%S")
  mode_label <- if (identical(report_data$solve_mode, "sample_size")) {
    "Solve for sample size"
  } else {
    "Solve for power"
  }

  has_n1n2 <- !is.null(report_data$sample_sizes$n1) &&
              !is.null(report_data$sample_sizes$n2)
  has_n   <- !is.null(report_data$sample_sizes$n)

  size_block <- if (has_n1n2) {
    sprintf(paste(
      "- *Group 1 (n1):* %.0f",
      "- *Group 2 (n2):* %.0f",
      "- *Total (N):* %.0f",
      sep = "\n"
    ),
    report_data$sample_sizes$n1,
    report_data$sample_sizes$n2,
    report_data$sample_sizes$n1 + report_data$sample_sizes$n2)
  } else if (has_n) {
    sprintf("- *Sample size (n):* %.0f", report_data$sample_sizes$n)
  } else {
    "_(Sample-size search results below.)_"
  }

  table_rows <- vapply(
    seq_len(nrow(report_data$power_results)),
    function(i) {
      r <- report_data$power_results[i, ]
      power_col <- if ("power" %in% names(r)) {
        sprintf("%.4f", r$power)
      } else if ("required_n" %in% names(r)) {
        if (is.na(r$required_n)) "--" else
          sprintf("%.0f (N)", r$required_n)
      } else {
        "--"
      }
      sprintf("  [%.4f], [%.4f], [%s],",
               r$effect_size, r$standardized_es, power_col)
    },
    character(1)
  )

  src <- c(
    sprintf("#set document(title: \"Power Analysis Report: %s\")",
             esc(test_spec$name)),
    "#set page(margin: 0.9in, numbering: \"1\")",
    "#set text(size: 11pt)",
    "#set par(justify: true, leading: 0.65em)",
    "#show heading.where(level: 1): set text(size: 18pt, weight: \"bold\")",
    "#show heading.where(level: 2): set text(size: 13pt, weight: \"bold\")",
    "",
    sprintf("= Power Analysis Report: %s", esc(test_spec$name)),
    "",
    sprintf("*Generated:* %s \\", ts),
    sprintf("*R version:* %s \\", esc(report_data$r_version)),
    sprintf("*Solve mode:* %s", mode_label),
    "",
    "== Test specification",
    "",
    sprintf("- *Test type:* %s", esc(test_spec$name)),
    sprintf("- *Description:* %s", esc(test_spec$description)),
    "",
    "== Effect-size method",
    "",
    sprintf("- *Method:* %s", esc(report_data$effect_size_range$method)),
    sprintf("- *Range:* %.4f to %.4f",
             min(report_data$effect_size_range$effect_sizes),
             max(report_data$effect_size_range$effect_sizes)),
    "",
    "== Sample size",
    "",
    size_block,
    "",
    "== Statistical parameters",
    "",
    sprintf("- *Type I error (alpha):* %.4f", report_data$type1_error),
    sprintf("- *Test direction:* %s",
             if (isTRUE(report_data$one_sided)) "One-sided" else
               "Two-sided"),
    "",
    "== Power-analysis results",
    "",
    "#table(",
    "  columns: 3,",
    "  align: right,",
    "  stroke: 0.4pt,",
    "  [*Effect Size*], [*Standardized*], [*Power*],",
    table_rows,
    ")",
    ""
  )

  # Gap 9: reproducibility R script. Typst raw blocks need
  # explicit escaping; emit as a raw block with `lang: "r"`.
  ctx <- .report_data_to_ctx(report_data)
  if (!is.null(ctx)) {
    repro <- .render_repro_script(ctx, fence = FALSE)
    if (nzchar(repro)) {
      src <- c(src,
        "== Reproducibility script",
        "",
        paste0("Run this block in a fresh R session with `pwr` ",
               "(and, for non-pwr tests, `zzpower`) installed to ",
               "reproduce the headline calculation."),
        "",
        "#raw(",
        sprintf("  \"%s\",", esc(repro)),
        "  lang: \"r\",",
        "  block: true",
        ")",
        ""
      )
    }
  }

  src <- c(src,
    "#line(length: 100%, stroke: 0.4pt + gray)",
    "",
    sprintf(
      "#text(size: 9pt, fill: gray.darken(40%%))[#emph[Citation:] %s]",
      esc(.zzpower_citation_line())
    )
  )

  paste(src, collapse = "\n")
}

#' Render the markdown report to PDF or Word
#'
#' For PDF, prefer Typst (no TeX install needed, fast). Fall back to
#' rmarkdown's xelatex pipeline if Typst is unavailable.
#'
#' @return Path to the rendered file, or `NULL` if no rendering
#'   backend is installed or rendering fails.
#' @keywords internal
.render_generic_report_binary <- function(report_data, test_spec, fmt) {
  if (fmt == "pdf") {
    pdf_path <- .render_pdf_via_typst(report_data, test_spec)
    if (!is.null(pdf_path)) return(pdf_path)
    return(.render_pdf_via_latex(report_data, test_spec))
  }
  if (fmt == "word") {
    return(.render_via_rmarkdown(report_data, test_spec, "word"))
  }
  NULL
}

#' @keywords internal
.render_pdf_via_typst <- function(report_data, test_spec) {
  if (!requireNamespace("typst", quietly = TRUE)) {
    return(NULL)
  }
  src <- .format_generic_typst_report(report_data, test_spec)
  src_file <- tempfile(fileext = ".typ")
  out_file <- tempfile(fileext = ".pdf")
  writeLines(src, src_file)

  tryCatch({
    typst::typst_compile(src_file, output = out_file)
    out_file
  }, error = function(e) {
    message("typst render failed: ", e$message)
    NULL
  })
}

#' @keywords internal
.render_pdf_via_latex <- function(report_data, test_spec) {
  .render_via_rmarkdown(report_data, test_spec, "pdf")
}

#' @keywords internal
.render_via_rmarkdown <- function(report_data, test_spec, fmt) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(NULL)
  }
  md <- .format_generic_md_report(report_data, test_spec)
  md_file <- tempfile(fileext = ".md")
  writeLines(md, md_file)

  out_format <- if (fmt == "pdf") {
    rmarkdown::pdf_document(latex_engine = "xelatex")
  } else if (fmt == "word") {
    rmarkdown::word_document()
  } else {
    return(NULL)
  }

  tryCatch(
    rmarkdown::render(
      md_file,
      output_format = out_format,
      quiet = TRUE,
      envir = new.env()
    ),
    error = function(e) {
      message("rmarkdown render failed: ", e$message)
      NULL
    }
  )
}

#' Citation text for inclusion in downloaded reports
#'
#' Returns a one-line citation string. Pulls from the package's
#' `inst/CITATION` when available; otherwise falls back to a
#' hard-coded string keyed off the installed version.
#'
#' @keywords internal
.zzpower_citation_line <- function() {
  # Build a plain-text citation directly rather than going through
  # `utils::citation()` + `format(style = "text")`. The latter
  # produces markdown-style emphasis around the title (`_..._`) and
  # angle-bracket URLs that look broken when rendered into the
  # HTML report or the citation footer of the text report.
  ver <- tryCatch(
    as.character(utils::packageVersion("zzpower")),
    error = function(e) "0.4.0"
  )
  sprintf(paste(
    "Thomas, R.G. (2026). zzpower: Interactive Power Analysis",
    "Calculator for Clinical Trial Designs.",
    "R package version %s. https://github.com/rgt47/zzpower"
  ), ver)
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

  # Gap 9: reproducibility R script section.
  ctx <- .report_data_to_ctx(report_data)
  if (!is.null(ctx)) {
    repro <- .render_repro_script(ctx, fence = FALSE)
    if (nzchar(repro)) {
      lines <- c(lines, "",
        "REPRODUCIBILITY SCRIPT",
        "--------------------------------------------",
        strsplit(repro, "\n", fixed = TRUE)[[1]]
      )
    }
  }

  c(lines, "",
    "========================================================================",
    "End of Report",
    "========================================================================",
    "",
    paste("Citation:", .zzpower_citation_line()),
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

  # Gap 9: reproducibility R script section.
  repro_html <- ""
  ctx <- .report_data_to_ctx(report_data)
  if (!is.null(ctx)) {
    repro <- .render_repro_script(ctx, fence = FALSE)
    if (nzchar(repro)) {
      repro_html <- paste0(
        '<h2>Reproducibility script</h2>',
        '<p>Run this block in a fresh R session with <code>pwr</code> ',
        '(and, for non-pwr tests, <code>zzpower</code>) installed to ',
        'exactly reproduce the headline calculation.</p>',
        '<pre style="background:#f5f5f5; padding:1rem; ',
        'border-radius:4px; overflow-x:auto;"><code>',
        htmltools::htmlEscape(repro),
        '</code></pre>'
      )
    }
  }

  paste0(html, '
      </tbody>
    </table>

    ', repro_html, '

    <div class="footer">
      <p>Report generated by zzpower. See package documentation
      for methodology details.</p>
      <p style="font-size: 0.8rem; color: #888; margin-top: 0.5rem;">
        <em>Citation:</em> ',
    htmltools::htmlEscape(.zzpower_citation_line()),
    '</p>
    </div>
  </div>
</body>
</html>')
}
