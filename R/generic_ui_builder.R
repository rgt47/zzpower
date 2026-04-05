#' Generic UI Builder
#'
#' Dynamically generates UI for any power analysis test based on
#' registry definition. Returns layout suitable for embedding inside
#' a nav_panel tab.
#'
#' @keywords internal
#' @importFrom bslib layout_sidebar sidebar card card_header
#' @importFrom shiny sliderInput numericInput radioButtons
#'   conditionalPanel checkboxInput fluidRow column plotOutput
#'   downloadButton uiOutput verbatimTextOutput
#' @importFrom DT dataTableOutput

#' Create UI for a Power Analysis Test
#'
#' Dynamically generates a sidebar layout with controls and results
#' panels based on test registry specification.
#'
#' @param test_id Test identifier (must exist in registry)
#'
#' @return A bslib layout_sidebar object for embedding in a tab
#'
#' @keywords internal
create_generic_test_ui <- function(test_id) {
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]

  if (is.null(test_spec)) {
    stop("Test '", test_id, "' not found in registry")
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::uiOutput(paste0(test_id, "_validation")),

      shiny::h6("Sample Size & Design"),
      shiny::uiOutput(paste0(test_id, "_sample_inputs")),

      shiny::hr(),

      shiny::h6("Effect Size Specification"),
      shiny::uiOutput(paste0(test_id, "_effect_inputs")),

      shiny::hr(),

      shiny::checkboxInput(
        paste0(test_id, "_show_advanced"),
        "Show Advanced Settings"
      ),
      shiny::conditionalPanel(
        condition = paste0(
          "input.", test_id, "_show_advanced == true"
        ),
        shiny::uiOutput(paste0(test_id, "_advanced"))
      )
    ),

    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Power Curve"),
          shiny::plotOutput(paste0(test_id, "_power_plot"))
        )
      ),
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Results"),
          DT::dataTableOutput(paste0(test_id, "_results_table"))
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          bslib::card_header("Study Summary"),
          shiny::verbatimTextOutput(paste0(test_id, "_summary"))
        )
      ),
      shiny::column(6,
        bslib::card(
          bslib::card_header("Generate Report"),
          shiny::radioButtons(
            paste0(test_id, "_report_format"),
            "Format",
            choices = c("Text" = "text", "HTML" = "html"),
            inline = TRUE
          ),
          shiny::downloadButton(
            paste0(test_id, "_download_report"),
            "Download Report"
          )
        )
      )
    )
  )
}

#' Render Sample Size Input Controls
#'
#' Dynamically generates input controls based on test specification.
#'
#' @param test_id Test identifier
#' @param input Shiny input object
#'
#' @return Shiny tagList with input controls
#'
#' @keywords internal
render_sample_size_inputs <- function(test_id, input) {
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
      condition_id <- paste0(
        test_id, "_",
        trimws(gsub("==.*", "", param_spec$condition))
      )
      condition_value <- gsub(".*'([^']+)'.*", "\\1", param_spec$condition)

      actual_value <- input[[condition_id]]
      if (is.null(actual_value) || actual_value != condition_value) {
        return(NULL)
      }
    }

    input_id <- paste0(test_id, "_", param_name)

    switch(param_spec$type,
      "slider" = shiny::sliderInput(
        input_id,
        label = param_spec$label,
        min = param_spec$min,
        max = param_spec$max,
        value = param_spec$default,
        step = param_spec$step
      ),
      "numeric" = shiny::numericInput(
        input_id,
        label = param_spec$label,
        value = param_spec$default,
        min = param_spec$min %||% 0,
        max = param_spec$max %||% Inf
      ),
      "radio" = shiny::radioButtons(
        input_id,
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
#' @param test_id Test identifier
#' @param input Shiny input object
#'
#' @return Shiny tagList with effect size controls
#'
#' @keywords internal
render_effect_size_inputs <- function(test_id, input) {
  registry <- get_power_test_registry()
  test_spec <- registry[[test_id]]

  methods <- test_spec$effect_size_methods
  effect_params <- test_spec$effect_size_params

  method_id <- paste0(test_id, "_effect_method")
  selected_method <- input[[method_id]] %||% methods[1]

  method_selector <- shiny::radioButtons(
    method_id,
    "Effect Size Method",
    choices = setNames(methods, methods),
    selected = selected_method
  )

  method_panels <- lapply(methods, function(method) {
    method_params <- effect_params[[method]]

    shiny::conditionalPanel(
      condition = paste0("input.", method_id, " == '", method, "'"),
      shiny::sliderInput(
        paste0(test_id, "_", method, "_es"),
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
              paste0(test_id, "_", method, "_", param_name),
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
#' @param test_id Test identifier
#'
#' @return Shiny tagList with advanced controls
#'
#' @keywords internal
render_advanced_settings <- function(test_id) {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6,
        shiny::numericInput(
          paste0(test_id, "_type1"),
          label = "Type I Error (alpha)",
          value = consts$TYPE1_DEFAULT,
          min = consts$TYPE1_MIN,
          max = consts$TYPE1_MAX,
          step = consts$TYPE1_STEP
        )
      ),
      shiny::column(6,
        shiny::checkboxInput(
          paste0(test_id, "_onesided"),
          label = "One-sided Test",
          value = FALSE
        )
      )
    )
  )
}

#' Get Effect Size Range for a Test
#'
#' Generates sequence of effect sizes for power curve.
#'
#' @param test_id Test identifier
#' @param input Shiny input object
#'
#' @return List with effect_sizes (original scale) and standardized
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

  params <- c(list(method = method), additional_params)
  standardized <- test_spec$standardize(
    effect_sizes, method, as.list(input)
  )

  list(
    effect_sizes = effect_sizes,
    standardized = standardized,
    method = method
  )
}
