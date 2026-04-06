#' Generic UI Builder
#'
#' Dynamically generates UI for any power analysis test based on
#' registry definition. Uses shiny NS() for proper module namespacing.
#'
#' @keywords internal
#' @importFrom bslib layout_sidebar layout_columns sidebar card
#'   card_header card_body
#' @importFrom shiny NS sliderInput numericInput radioButtons
#'   conditionalPanel checkboxInput plotOutput downloadButton
#'   uiOutput verbatimTextOutput tagList
#' @importFrom DT dataTableOutput

#' Create Module UI for a Power Analysis Test
#'
#' Module UI function following the Shiny module convention. Uses
#' NS(id) for all input/output IDs so the server can access them
#' without manual prefixing.
#'
#' @param test_id Module ID (must match a registry entry)
#'
#' @return A bslib layout_sidebar suitable for embedding in a tab
#'
#' @keywords internal
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

      shiny::h6("Sample Size & Design"),
      shiny::uiOutput(ns("sample_inputs")),

      shiny::hr(),

      shiny::h6("Effect Size Specification"),
      shiny::uiOutput(ns("effect_inputs")),

      shiny::hr(),

      shiny::checkboxInput(ns("show_advanced"), "Show Advanced Settings"),
      shiny::conditionalPanel(
        condition = sprintf("input['%s']", ns("show_advanced")),
        shiny::uiOutput(ns("advanced"))
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Power Curve"),
        bslib::card_body(
          fillable = TRUE,
          shiny::plotOutput(ns("power_plot"))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Results"),
        bslib::card_body(
          fillable = TRUE,
          DT::dataTableOutput(ns("results_table"))
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Study Summary"),
        bslib::card_body(
          shiny::verbatimTextOutput(ns("summary"))
        )
      ),
      bslib::card(
        bslib::card_header("Generate Report"),
        bslib::card_body(
          shiny::radioButtons(
            ns("report_format"), "Format",
            choices = c("Text" = "text", "HTML" = "html"),
            inline = TRUE
          ),
          shiny::downloadButton(ns("download_report"), "Download Report")
        )
      )
    )
  )
}

#' Render Sample Size Input Controls
#'
#' Dynamically generates input controls based on test specification.
#' Called from renderUI inside a module server, so ns (session$ns) is
#' required to namespace the dynamically created inputs.
#'
#' @param test_id Test identifier (for registry lookup)
#' @param input Module-scoped Shiny input object
#' @param ns Namespace function (session$ns from moduleServer)
#'
#' @return Shiny tagList with input controls
#'
#' @keywords internal
render_sample_size_inputs <- function(test_id, input, ns) {
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]
  params <- test_spec$parameters

  sample_params <- params[grepl(
    "sample|allocation|ratio|dropout|pairs|event|groups",
    names(params)
  )]

  controls <- lapply(names(sample_params), function(param_name) {
    param_spec <- sample_params[[param_name]]

    if (!is.null(param_spec$condition)) {
      param_ref <- trimws(gsub("==.*", "", param_spec$condition))
      condition_value <- gsub(".*'([^']+)'.*", "\\1", param_spec$condition)
      actual_value <- input[[param_ref]]
      if (is.null(actual_value) || actual_value != condition_value) {
        return(NULL)
      }
    }

    switch(param_spec$type,
      "slider" = shiny::sliderInput(
        ns(param_name),
        label = param_spec$label,
        min = param_spec$min,
        max = param_spec$max,
        value = param_spec$default,
        step = param_spec$step
      ),
      "numeric" = shiny::numericInput(
        ns(param_name),
        label = param_spec$label,
        value = param_spec$default,
        min = param_spec$min %||% 0,
        max = param_spec$max %||% Inf
      ),
      "radio" = shiny::radioButtons(
        ns(param_name),
        label = param_spec$label,
        choices = param_spec$options,
        selected = param_spec$default
      ),
      NULL
    )
  })

  shiny::tagList(controls)
}

#' Render Effect Size Input Controls
#'
#' Dynamically generates effect size method selection and range inputs.
#'
#' @param test_id Test identifier (for registry lookup)
#' @param input Module-scoped Shiny input object
#' @param ns Namespace function (session$ns from moduleServer)
#'
#' @return Shiny tagList with effect size controls
#'
#' @keywords internal
render_effect_size_inputs <- function(test_id, input, ns) {
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]

  methods <- test_spec$effect_size_methods
  effect_params <- test_spec$effect_size_params

  selected_method <- input$effect_method %||% methods[1]

  method_selector <- shiny::radioButtons(
    ns("effect_method"),
    "Effect Size Method",
    choices = setNames(methods, methods),
    selected = selected_method
  )

  method_panels <- lapply(methods, function(method) {
    method_params <- effect_params[[method]]

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == '%s'", ns("effect_method"), method),
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

#' Render Advanced Settings
#'
#' Generates advanced parameter controls (alpha, test direction).
#'
#' @param ns Namespace function (session$ns from moduleServer)
#'
#' @return Shiny tagList with advanced controls
#'
#' @keywords internal
render_advanced_settings <- function(ns) {
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
