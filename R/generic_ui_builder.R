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
    ),

    # Gap 2: sensitivity table. The Effect-size column (column 0)
    # is editable; double-click a cell to override and the four
    # N columns recompute. Default rows seed from the registry's
    # `default_effect_grid` for the active effect-size method.
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::span(
            bsicons::bs_icon("table"),
            " Sensitivity table"
          ),
          shiny::div(
            shiny::actionButton(
              ns("sensitivity_add_row"),
              label = "Add row",
              class = "btn btn-sm btn-outline-secondary me-1",
              icon = shiny::icon("plus")
            ),
            shiny::actionButton(
              ns("sensitivity_delete_row"),
              label = "Delete selected",
              class = "btn btn-sm btn-outline-secondary me-1",
              icon = shiny::icon("trash")
            ),
            shiny::downloadButton(
              ns("download_sensitivity_csv"),
              label = "CSV",
              class = "btn btn-sm btn-outline-secondary me-1"
            ),
            shiny::downloadButton(
              ns("download_sensitivity_md"),
              label = "Markdown",
              class = "btn btn-sm btn-outline-secondary"
            )
          )
        )
      ),
      bslib::card_body(
        shiny::div(
          class = "small text-muted",
          shiny::p(
            shiny::strong("What this table shows: "),
            "the total sample size required to achieve the listed ",
            "power across a plausible range of effect sizes. ",
            "Reviewers cite this kind of sensitivity analysis when ",
            "your assumed effect size is uncertain. Edit, add, or ",
            "delete rows to match the range of effect sizes ",
            "reported in your prior literature."
          ),
          shiny::p(
            shiny::strong("Columns: "),
            shiny::tags$code("Effect size"),
            " is the assumed effect on the native scale (e.g. ",
            "Cohen's d, hazard ratio); ",
            shiny::tags$code("Standardised"),
            " is the same effect on the universal scale used by ",
            "the power formula. ",
            shiny::tags$code("N evaluable"),
            " is the analyzable sample size after losses to ",
            "follow-up (the value the power calculation uses); ",
            shiny::tags$code("N enrolled"),
            " is the larger ITT count you actually recruit, ",
            "inflated for the dropout rate set in the sidebar. ",
            "Each pair (",
            shiny::tags$code("@ 80%"), ", ", shiny::tags$code("@ 90%"),
            ") is computed at that target power."
          ),
          shiny::p(
            shiny::strong("How to edit: "),
            "double-click a cell in the ",
            shiny::tags$code("Effect size"), " column to override ",
            "it; the four N columns recompute. ",
            shiny::tags$code("Add row"),
            " inserts a new effect-size value at the median; ",
            shiny::tags$code("Delete selected"),
            " removes the row(s) you've clicked to highlight."
          )
        ),
        shiny::tags$div(
          style = "margin-top: 0.5rem;",
          DT::DTOutput(ns("sensitivity_table"))
        )
      )
    ),

    # Gap 1: methods-section paragraph generator. Last card on the
    # page, collapsed by default. Click "Show / Hide" to toggle the
    # paragraph body. The intro inside explains in plain language
    # what the paragraph is for and how to use it; reviewers should
    # not paste the generated text verbatim without editing.
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::span(
            bsicons::bs_icon("file-earmark-text"),
            " Draft methods paragraph"
          ),
          shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-primary me-1",
              id = ns("toggle_methods_btn"),
              "Show",
              onclick = sprintf(
                paste0(
                  "var b = document.getElementById('%s'); ",
                  "var hidden = (b.style.display === 'none' ",
                  "|| b.style.display === ''); ",
                  "b.style.display = hidden ? 'block' : 'none'; ",
                  "this.innerText = hidden ? 'Hide' : 'Show';"
                ),
                ns("methods_body")
              )
            ),
            shiny::actionButton(
              ns("copy_methods_paragraph"),
              label = "Copy",
              class = "btn btn-sm btn-outline-primary",
              onclick = sprintf(
                paste0(
                  "navigator.clipboard.writeText(",
                  "document.getElementById('%s').innerText); ",
                  "this.innerText='Copied'; ",
                  "setTimeout(function(b){b.innerText='Copy';}, 1500, this);"
                ),
                ns("methods_paragraph_text")
              )
            )
          )
        )
      ),
      bslib::card_body(
        shiny::tags$div(
          id = ns("methods_body"),
          style = "display: none;",
          shiny::tags$div(
            class = "alert alert-info small",
            shiny::p(
              shiny::strong("What this is: "),
              "the text below is a ", shiny::strong("draft"),
              " sample-size statement assembled from your current ",
              "inputs. The wording follows what NIH reviewers ",
              "expect (the Statistical Design and Power section of ",
              "the Human Subjects form) and what regulatory ",
              "guidelines require (ICH E9, the international ",
              "standard for clinical-trial statistics). ",
              "It is taken nearly verbatim from those guidance ",
              "documents with your slider values filled in."
            ),
            shiny::p(
              shiny::strong("How to use it: "),
              "treat this as a starting point, not finished prose. ",
              "Edit it to match your study's terminology, your ",
              "primary outcome, and the direction of your effect. ",
              "Replace ",
              shiny::tags$em("\"pilot data, citation pending\""),
              " by entering your effect-size source in the ",
              shiny::tags$em("Effect-Size Source"),
              " text box under ",
              shiny::tags$em("Advanced Settings"),
              " in the sidebar. Adjust the sensitivity-multiplier ",
              "and sex-as-biological-variable controls to match ",
              "the conventions of your funding agency."
            )
          ),
          shiny::tags$div(
            style = "white-space: pre-wrap; line-height: 1.5; padding: 0.5rem; background: #f8f9fa; border-radius: 4px; margin-top: 0.5rem;",
            shiny::textOutput(ns("methods_paragraph_text"),
                              inline = FALSE)
          )
        )
      )
    ),

    # Per-panel formula citation footnote. Pulled from
    # test_spec$formula_citation (populated in Wave 1) -- one
    # italic line below all the cards, above the global page
    # footer. Surfaces the methods-paper citation for the
    # currently-viewed test so reviewers reading the panel do
    # not have to scroll into the methods-paragraph card.
    if (!is.null(test_spec$formula_citation) &&
        nzchar(test_spec$formula_citation)) {
      shiny::tags$p(
        class = "text-muted small mt-3",
        style = "font-style: italic; line-height: 1.4;",
        shiny::tags$strong("Method: "),
        test_spec$formula_citation
      )
    }
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
    ),

    # Gap 5: effect-size provenance. The Wave 2 methods-paragraph
    # generator and the table-builder caption both inject these
    # strings; default empty falls back to "pilot data (citation
    # pending)" in the rendered output.
    shiny::tags$hr(),
    shiny::tags$h6(
      bsicons::bs_icon("quote"), " Effect-Size Source",
      class = "text-muted small"
    ),
    shiny::textInput(
      ns("effect_source"),
      label = "Citation",
      placeholder = "Smith et al. 2019, JAMA, n=30, observed d=0.52"
    ),
    shiny::textInput(
      ns("effect_doi"),
      label = "DOI or URL",
      placeholder = "10.1001/jama.2019.12345"
    ),

    # Gap 6: sensitivity-row knob. Multiplier on the headline
    # effect size for the conservative-effect sentence the
    # Wave 2 generator appends to the methods paragraph.
    shiny::tags$hr(),
    shiny::sliderInput(
      ns("sensitivity_factor"),
      label = "Sensitivity Multiplier",
      min = 0.3, max = 1, value = 0.7, step = 0.05
    ),
    shiny::tags$small(
      class = "text-muted",
      "Used to compute power if the true effect is smaller than ",
      "the headline (e.g. 0.7 -> 30% smaller). Reported in the ",
      "methods paragraph per ICH E9 Sec.3.5."
    ),

    # Gap 12: sex-as-biological-variable paragraph toggle.
    shiny::tags$hr(),
    shiny::checkboxInput(
      ns("include_sex_paragraph"),
      label = "Append NIH Sex-as-a-Biological-Variable paragraph",
      value = TRUE
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
