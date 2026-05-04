#' Generic UI Builder
#'
#' Dynamically generates UI for any power analysis test based on
#' registry definition. All inputs are built statically at UI
#' construction time to avoid renderUI invalidation loops.
#'
#' @keywords internal
#' @importFrom bslib layout_sidebar layout_columns sidebar card
#'   card_header card_body
#' @importFrom shiny NS sliderInput numericInput radioButtons
#'   conditionalPanel checkboxInput plotOutput downloadButton
#'   uiOutput verbatimTextOutput tagList
#' @importFrom DT DTOutput

#' Wrap a parameter label with an info-icon tooltip
#'
#' If the parameter spec includes a `description`, the label is
#' followed by a small info-circle icon that shows the description on
#' hover. If not, the label is returned plain.
#'
#' @param param_spec Named list from the registry: must contain `label`
#'   and may contain `description`.
#' @return A character or `tagList` suitable as the `label` argument
#'   of `shiny::sliderInput`, `shiny::numericInput`, etc.
#' @keywords internal
param_label_with_tooltip <- function(param_spec) {
  if (is.null(param_spec$description) ||
      !nzchar(param_spec$description)) {
    return(param_spec$label)
  }
  shiny::tagList(
    param_spec$label,
    " ",
    bslib::tooltip(
      bsicons::bs_icon("info-circle", class = "text-muted small"),
      param_spec$description,
      placement = "right"
    )
  )
}

#' Create Module UI for a Power Analysis Test
#'
#' Module UI function. Uses NS(id) for all input/output IDs. Inputs
#' are generated statically from the registry spec (no renderUI) to
#' prevent reactive invalidation loops.
#'
#' @param test_id Module ID (must match a registry entry)
#'
#' @return A bslib layout_sidebar suitable for embedding in a tab
#'
#' @export
create_generic_test_ui <- function(test_id) {
  ns <- shiny::NS(test_id)
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]

  if (is.null(test_spec)) {
    stop("Test '", test_id, "' not found in registry")
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::uiOutput(ns("validation")),

      shiny::radioButtons(
        ns("solve_for"), "Solve for",
        choices = c("Power" = "power", "Sample Size" = "sample_size"),
        selected = "power", inline = TRUE
      ),

      bslib::accordion(
        id = ns("sidebar_accordion"),
        open = c("Sample Size & Design", "Effect Size"),
        bslib::accordion_panel(
          "Sample Size & Design",
          icon = bsicons::bs_icon("people"),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'power'",
                                 ns("solve_for")),
            build_sample_size_inputs(test_spec, ns)
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'sample_size'",
                                 ns("solve_for")),
            shiny::sliderInput(
              ns("target_power"), "Target power",
              min = 0.5, max = 0.99, value = 0.80, step = 0.01
            ),
            build_design_inputs(test_spec, ns)
          )
        ),
        bslib::accordion_panel(
          "Effect Size",
          icon = bsicons::bs_icon("rulers"),
          build_effect_size_inputs(test_spec, ns)
        ),
        bslib::accordion_panel(
          "Advanced Settings",
          icon = bsicons::bs_icon("sliders"),
          build_advanced_settings(ns)
        )
      )
    ),

    bslib::layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title = shiny::textOutput(ns("headline_box1_title")),
        value = shiny::textOutput(ns("headline_box1_value")),
        showcase = bsicons::bs_icon("bullseye"),
        theme = "primary"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("headline_box2_title")),
        value = shiny::textOutput(ns("headline_box2_value")),
        showcase = bsicons::bs_icon("graph-up"),
        theme = "info"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("headline_box3_title")),
        value = shiny::textOutput(ns("headline_box3_value")),
        showcase = bsicons::bs_icon("people"),
        theme = "secondary"
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        height = "420px",
        bslib::card_header("Power Curve"),
        bslib::card_body(
          fillable = TRUE,
          min_height = "320px",
          shiny::plotOutput(ns("power_plot"), height = "320px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        height = "420px",
        bslib::card_header("Results"),
        bslib::card_body(
          fillable = TRUE,
          min_height = "320px",
          DT::DTOutput(ns("results_table"))
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        height = "260px",
        bslib::card_header("Study Summary"),
        bslib::card_body(
          min_height = "180px",
          shiny::uiOutput(ns("summary"))
        )
      ),
      bslib::card(
        height = "260px",
        bslib::card_header("Generate Report"),
        bslib::card_body(
          shiny::radioButtons(
            ns("report_format"), "Format",
            choices = c(
              "Text" = "text",
              "HTML" = "html",
              "PDF"  = "pdf",
              "Word" = "word"
            ),
            inline = TRUE
          ),
          shiny::downloadButton(ns("download_report"), "Download Report")
        )
      )
    )
  )
}

#' Build Sample Size Input Controls (Static)
#'
#' Generates input controls at UI build time from test specification.
#' Conditional inputs use conditionalPanel (client-side JS) to avoid
#' server-side renderUI loops.
#'
#' @param test_spec Test specification from registry
#' @param ns Namespace function from NS(test_id)
#'
#' @return Shiny tagList with input controls
#'
#' @keywords internal
build_sample_size_inputs <- function(test_spec, ns) {
  params <- test_spec$parameters

  sample_params <- params[grepl(
    "sample|allocation|ratio|dropout|pairs|event|groups",
    names(params)
  )]

  controls <- lapply(names(sample_params), function(param_name) {
    param_spec <- sample_params[[param_name]]

    widget <- switch(param_spec$type,
      "slider" = shiny::sliderInput(
        ns(param_name),
        label = param_label_with_tooltip(param_spec),
        min = param_spec$min,
        max = param_spec$max,
        value = param_spec$default,
        step = param_spec$step
      ),
      "numeric" = shiny::numericInput(
        ns(param_name),
        label = param_label_with_tooltip(param_spec),
        value = param_spec$default,
        min = param_spec$min %||% 0,
        max = param_spec$max %||% Inf
      ),
      "radio" = shiny::radioButtons(
        ns(param_name),
        label = param_label_with_tooltip(param_spec),
        choices = param_spec$options,
        selected = param_spec$default
      ),
      NULL
    )

    if (!is.null(param_spec$condition)) {
      param_ref <- trimws(gsub("==.*", "", param_spec$condition))
      condition_value <- gsub(".*'([^']+)'.*", "\\1", param_spec$condition)
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == '%s'", ns(param_ref), condition_value
        ),
        widget
      )
    } else {
      widget
    }
  })

  shiny::tagList(controls)
}

#' Build Design Parameter Inputs (for Sample Size Mode)
#'
#' Shows design parameters (allocation, dropout, event probability,
#' etc.) but excludes sample_size since that is the solve target.
#' Input IDs use a "ss_" prefix to avoid collisions with the power
#' mode inputs.
#'
#' @param test_spec Test specification from registry
#' @param ns Namespace function from NS(test_id)
#'
#' @return Shiny tagList with design controls
#'
#' @keywords internal
build_design_inputs <- function(test_spec, ns) {
  params <- test_spec$parameters

  design_params <- params[grepl(
    "allocation|ratio|dropout|event|groups",
    names(params)
  )]

  if (length(design_params) == 0) return(NULL)

  controls <- lapply(names(design_params), function(param_name) {
    param_spec <- design_params[[param_name]]
    input_id <- paste0("ss_", param_name)

    widget <- switch(param_spec$type,
      "slider" = shiny::sliderInput(
        ns(input_id),
        label = param_label_with_tooltip(param_spec),
        min = param_spec$min,
        max = param_spec$max,
        value = param_spec$default,
        step = param_spec$step
      ),
      "numeric" = shiny::numericInput(
        ns(input_id),
        label = param_label_with_tooltip(param_spec),
        value = param_spec$default,
        min = param_spec$min %||% 0,
        max = param_spec$max %||% Inf
      ),
      "radio" = shiny::radioButtons(
        ns(input_id),
        label = param_label_with_tooltip(param_spec),
        choices = param_spec$options,
        selected = param_spec$default
      ),
      NULL
    )

    if (!is.null(param_spec$condition)) {
      param_ref <- trimws(gsub("==.*", "", param_spec$condition))
      condition_value <- gsub(".*'([^']+)'.*", "\\1", param_spec$condition)
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == '%s'", ns(paste0("ss_", param_ref)), condition_value
        ),
        widget
      )
    } else {
      widget
    }
  })

  shiny::tagList(controls)
}

#' Build Effect Size Input Controls (Static)
#'
#' Generates effect size method selection and range inputs at UI build
#' time. Method switching is handled entirely client-side via
#' conditionalPanel.
#'
#' @param test_spec Test specification from registry
#' @param ns Namespace function from NS(test_id)
#'
#' @return Shiny tagList with effect size controls
#'
#' @keywords internal
build_effect_size_inputs <- function(test_spec, ns) {
  methods <- test_spec$effect_size_methods
  effect_params <- test_spec$effect_size_params

  method_selector <- shiny::radioButtons(
    ns("effect_method"),
    "Effect Size Method",
    choices = stats::setNames(methods, methods),
    selected = methods[1]
  )

  method_panels <- lapply(methods, function(method) {
    method_params <- effect_params[[method]]

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == '%s'", ns("effect_method"), method
      ),
      shiny::sliderInput(
        ns(paste0(method, "_es")),
        label = method_params$label,
        min = method_params$min,
        max = method_params$max,
        value = c(method_params$default_min, method_params$default_max),
        step = 0.05
      ),
      if (!is.null(method_params$requires)) {
        shiny::tagList(lapply(
          names(method_params$requires),
          function(param_name) {
            param <- method_params$requires[[param_name]]
            shiny::numericInput(
              ns(paste0(method, "_", param_name)),
              label = param$label,
              value = param$default
            )
          }
        ))
      }
    )
  })

  shiny::tagList(method_selector, method_panels)
}

#' Build Advanced Settings (Static)
#'
#' Generates advanced parameter controls (alpha, test direction).
#'
#' @param ns Namespace function from NS(test_id)
#'
#' @return Shiny tagList with advanced controls
#'
#' @keywords internal
build_advanced_settings <- function(ns) {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::numericInput(
        ns("type1"),
        label = "Type I Error (alpha)",
        value = consts$TYPE1_DEFAULT,
        min = consts$TYPE1_MIN,
        max = consts$TYPE1_MAX,
        step = consts$TYPE1_STEP
      ),
      shiny::checkboxInput(
        ns("onesided"),
        label = "One-sided Test",
        value = FALSE
      )
    )
  )
}

#' Get Effect Size Range for a Test
#'
#' Standalone utility for generating effect size sequences. Used in
#' tests and non-module contexts with manually prefixed input IDs.
#'
#' @param test_id Test identifier
#' @param input List or Shiny input with prefixed IDs
#'
#' @return List with effect_sizes, standardized, and method
#'
#' @keywords internal
get_effect_size_range <- function(test_id, input) {
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]
  consts <- ZZPOWER_CONSTANTS

  method <- input[[paste0(test_id, "_effect_method")]] %||%
    test_spec$effect_size_methods[1]
  method_params <- test_spec$effect_size_params[[method]]

  es_min <- input[[paste0(test_id, "_", method, "_es")]][1] %||%
    method_params$default_min
  es_max <- input[[paste0(test_id, "_", method, "_es")]][2] %||%
    method_params$default_max

  effect_sizes <- seq(es_min, es_max, length.out = consts$EFFECT_SIZE_SEQ_LENGTH)

  additional_params <- list()
  if (!is.null(method_params$requires)) {
    for (param_name in names(method_params$requires)) {
      param_value <- input[[paste0(test_id, "_", method, "_", param_name)]]
      if (!is.null(param_value)) {
        additional_params[[param_name]] <- param_value
      }
    }
  }

  standardized <- test_spec$standardize(
    effect_sizes, method, as.list(input)
  )

  list(
    effect_sizes = effect_sizes,
    standardized = standardized,
    method = method
  )
}
