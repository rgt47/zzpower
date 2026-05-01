#' Create UI Components for the Shiny Application
#'
#' Internal functions to create the user interface components for the
#' zzpower Shiny application. These functions generate the layout,
#' input controls, and output displays.
#'
#' @details
#' The UI is organized into the following sections:
#' \itemize{
#'   \item Sidebar: Input controls for sample size, effect size, and advanced settings
#'   \item Main area: Power curve plot, results table, study summary, and report generation
#'   \item Responsive: Uses Bootstrap 5 for mobile-friendly design
#' }
#'
#' @keywords internal
#' @importFrom bslib bs_theme page_sidebar sidebar card card_header tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shiny sliderInput numericInput radioButtons conditionalPanel checkboxInput fluidRow column tagList div htmlOutput hr verbatimTextOutput plotOutput downloadButton
#' @importFrom DT dataTableOutput

#' Create a Tooltip UI Element
#'
#' Creates an accessible tooltip that appears on hover, used throughout
#' the application to provide context-sensitive help for input parameters.
#'
#' @keywords internal
#' @param icon Icon element (default: info circle from Bootstrap Icons)
#' @param text Tooltip text to display on hover
#' @param placement Tooltip placement relative to icon (default: "right")
#'   One of "top", "right", "bottom", or "left"
#'
#' @return A tooltip element that can be inserted into Shiny UI
#'
#' @examples
#' \dontrun{
#' # Create a tooltip next to a label
#' label <- div(
#'   "Sample Size",
#'   create_tooltip(text = "Total number of participants")
#' )
#' }
create_tooltip <- function(icon = bsicons::bs_icon("info-circle"),
                           text,
                           placement = "right") {
  bslib::tooltip(icon, text, placement = placement)
}

#' Create Main UI
#'
#' Assembles the complete user interface for the zzpower application,
#' combining all input and output components into a cohesive layout.
#'
#' @keywords internal
#' @return A Shiny UI object of class "bslib_page"
create_ui <- function() {
  
  bslib::page_sidebar(
    title = "Power Analysis Calculator for Two-Group Designs",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    
    sidebar = bslib::sidebar(
      shiny::uiOutput("validation_messages"),
      create_sample_size_inputs(),
      shiny::hr(),
      create_effect_size_inputs(),
      shiny::hr(),
      create_advanced_settings()
    ),
    
    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Power Curve"),
          shiny::plotOutput("power_plot")
        )
      ),
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Detailed Results"),
          DT::dataTableOutput("results_table")
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          bslib::card_header("Study Design Summary"),
          shiny::verbatimTextOutput("summary_text")
        )
      ),
      shiny::column(6,
        bslib::card(
          bslib::card_header("Generate Report"),
          shiny::radioButtons(
            "report_format", 
            "Report Format", 
            choices = c("PDF", "HTML", "Word"),
            inline = TRUE
          ),
          shiny::downloadButton("download_report", "Download Report")
        )
      )
    )
  )
}

#' Create Sample Size Input Components
#'
#' Generates interactive input controls for specifying sample size and dropout rate.
#' These are fundamental parameters for power analysis and appear prominently in the sidebar.
#'
#' @keywords internal
#' @details
#' This function creates two input controls:
#' \itemize{
#'   \item Total Sample Size slider (20-500 participants)
#'   \item Dropout Rate slider (0-50%)
#'   \item Reactive display showing calculated completer sample sizes
#' }
#'
#' The sample size display dynamically updates to show both intention-to-treat (ITT)
#' and completer sample sizes based on the dropout rate.
#'
#' @return A Shiny tag list containing sample size input controls
#'
#' @seealso [create_effect_size_inputs()], [create_advanced_settings()]
create_sample_size_inputs <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::sliderInput(
      "N",
      label = shiny::div(
        "Total Sample Size",
        create_tooltip(text = "Total number of participants in both groups combined")
      ),
      min = consts$SAMPLE_SIZE_MIN,
      max = consts$SAMPLE_SIZE_MAX,
      value = consts$SAMPLE_SIZE_DEFAULT,
      step = consts$SAMPLE_SIZE_STEP
    ),

    shiny::sliderInput(
      "dropout",
      label = shiny::div(
        "Dropout Rate",
        create_tooltip(text = "Percentage of participants expected to withdraw before study completion")
      ),
      min = consts$DROPOUT_MIN,
      max = consts$DROPOUT_MAX,
      value = consts$DROPOUT_DEFAULT,
      step = consts$DROPOUT_STEP
    ),

    shiny::htmlOutput("sample_size_display")
  )
}

#' Create Effect Size Input Components
#'
#' Generates interactive controls for specifying effect size using one of four methods.
#' This is the core parametrization for power analysis calculations.
#'
#' @keywords internal
#' @details
#' Supports four different effect size specification methods:
#' \itemize{
#'   \item **Standard Deviation Units** (Cohen's d): Direct effect size in SD units
#'   \item **Percent Reduction**: Effect as percentage reduction from placebo baseline
#'   \item **Difference in Change Scores**: Absolute difference in outcome units
#'   \item **Change in Active Group**: Direct specification of treatment group change
#' }
#'
#' Conditional panels show method-specific inputs only when that method is selected.
#' Dependent parameters (e.g., placebo SD, placebo change) are shown as needed.
#'
#' @return A Shiny tag list containing effect size input controls and conditional panels
#'
#' @seealso [create_sample_size_inputs()], [create_advanced_settings()]
create_effect_size_inputs <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::radioButtons(
      "dmeth",
      "Effect Size Calculation Method",
      choices = c(
        "Standard Deviation Units" = "std",
        "Percent Reduction" = "pct",
        "Difference in Change Scores" = "diff",
        "Change in Active Group" = "active"
      ),
      selected = "std"
    ),

    # Conditional panels for different effect size methods
    shiny::conditionalPanel(
      condition = "input.dmeth == 'std'",
      shiny::sliderInput("del", "Effect Size in Standard Deviations",
                        min = consts$COHENS_D_MIN,
                        max = consts$COHENS_D_MAX,
                        value = c(consts$COHENS_D_DEFAULT_MIN, consts$COHENS_D_DEFAULT_MAX),
                        step = 0.1)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'diff'",
      shiny::sliderInput("dff", "Difference in Scores",
                        min = consts$DIFFERENCE_MIN,
                        max = consts$DIFFERENCE_MAX,
                        value = c(consts$DIFFERENCE_DEFAULT_MIN, consts$DIFFERENCE_DEFAULT_MAX),
                        step = 0.5),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = consts$PLACEBO_SD_DEFAULT)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'pct'",
      shiny::sliderInput("pct", "Percent Reduction",
                        min = consts$PERCENT_REDUCTION_MIN,
                        max = consts$PERCENT_REDUCTION_MAX,
                        value = c(consts$PERCENT_REDUCTION_DEFAULT_MIN, consts$PERCENT_REDUCTION_DEFAULT_MAX),
                        step = 0.05),
      shiny::numericInput("d0", "Placebo Change", value = consts$PLACEBO_CHANGE_DEFAULT),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = consts$PLACEBO_SD_DEFAULT)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'active'",
      shiny::sliderInput("active", "Treatment Group Change",
                        min = consts$TREATMENT_CHANGE_MIN,
                        max = consts$TREATMENT_CHANGE_MAX,
                        value = c(consts$TREATMENT_CHANGE_DEFAULT_MIN, consts$TREATMENT_CHANGE_DEFAULT_MAX),
                        step = 0.5),
      shiny::numericInput("d0", "Placebo Change", value = consts$PLACEBO_CHANGE_DEFAULT),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = consts$PLACEBO_SD_DEFAULT)
    )
  )
}

#' Create Advanced Settings Components
#'
#' Generates optional input controls for advanced trial design parameters.
#' These settings are initially hidden and revealed via a checkbox.
#'
#' @keywords internal
#' @details
#' Advanced parameters include:
#' \itemize{
#'   \item **Allocation Ratio**: Active to control group ratio (0.5-5)
#'   \item **Drop-in Rate**: Proportion of control participants crossing to active treatment
#'   \item **Type I Error Rate**: Significance level (alpha) for statistical testing
#'   \item **Test Type**: One-sided vs two-sided test specification
#' }
#'
#' These parameters are hidden by default in a collapsible section to maintain
#' UI simplicity for basic use cases while providing flexibility for advanced users.
#'
#' @return A Shiny tag list containing advanced settings with conditional visibility
#'
#' @seealso [create_sample_size_inputs()], [create_effect_size_inputs()]
create_advanced_settings <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::checkboxInput("show_advanced", "Show Advanced Settings"),
    shiny::conditionalPanel(
      condition = "input.show_advanced == true",
      shiny::fluidRow(
        shiny::column(6,
          shiny::numericInput("ratio", "Active to Control Group Ratio",
                             value = consts$RATIO_DEFAULT,
                             min = consts$RATIO_MIN,
                             max = consts$RATIO_MAX,
                             step = consts$RATIO_STEP),
          shiny::sliderInput("dropin", "Drop-in Rate",
                            min = consts$DROPIN_MIN,
                            max = consts$DROPIN_MAX,
                            value = consts$DROPIN_DEFAULT,
                            step = consts$DROPIN_STEP)
        ),
        shiny::column(6,
          shiny::numericInput("type1", "Type I Error Rate",
                             value = consts$TYPE1_DEFAULT,
                             min = consts$TYPE1_MIN,
                             max = consts$TYPE1_MAX,
                             step = consts$TYPE1_STEP),
          shiny::checkboxInput("onesided", "One-sided Test")
        )
      )
    )
  )
}