#' Create UI Components for Two Proportions Analysis
#'
#' Internal functions to create the user interface components for
#' two-proportion comparison power analysis (pwr.2p2n.test).
#'
#' @keywords internal
#' @importFrom bslib card card_header
#' @importFrom shiny sliderInput numericInput radioButtons conditionalPanel checkboxInput fluidRow column tagList div htmlOutput hr verbatimTextOutput plotOutput downloadButton
#' @importFrom DT dataTableOutput

#' Create Main UI for Two Proportions
#'
#' Assembles the complete user interface for proportion comparison power analysis.
#'
#' @keywords internal
#' @return A Shiny UI object with sidebar and main content
create_proportions_ui <- function() {

  bslib::page_sidebar(
    title = "Power Analysis Calculator for Two Proportions",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),

    sidebar = bslib::sidebar(
      shiny::uiOutput("prop_validation_messages"),
      create_prop_sample_size_inputs(),
      shiny::hr(),
      create_prop_effect_size_inputs(),
      shiny::hr(),
      create_prop_advanced_settings()
    ),

    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Power Curve"),
          shiny::plotOutput("prop_power_plot")
        )
      ),
      shiny::column(6,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Detailed Results"),
          DT::dataTableOutput("prop_results_table")
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(6,
        bslib::card(
          bslib::card_header("Study Design Summary"),
          shiny::verbatimTextOutput("prop_summary_text")
        )
      ),
      shiny::column(6,
        bslib::card(
          bslib::card_header("Generate Report"),
          shiny::radioButtons(
            "prop_report_format",
            "Report Format",
            choices = c("Text" = "text", "HTML" = "html", "PDF" = "pdf"),
            inline = TRUE
          ),
          shiny::downloadButton("prop_download_report", "Download Report")
        )
      )
    )
  )
}

#' Create Sample Size Input Components for Proportions
#'
#' Generates interactive controls for sample size specification in proportion studies.
#' Supports both equal and unequal allocation ratios.
#'
#' @keywords internal
#' @return A Shiny tag list containing sample size input controls
create_prop_sample_size_inputs <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::h6("Sample Size Strategy"),

    shiny::radioButtons(
      "prop_allocation",
      "Sample Size Allocation",
      choices = c(
        "Equal groups (n1 = n2)" = "equal",
        "Unequal groups (specify ratio)" = "unequal"
      ),
      selected = "equal"
    ),

    shiny::sliderInput(
      "prop_total_n",
      label = shiny::div(
        "Total Sample Size",
        create_tooltip(text = "Total number of participants across both groups")
      ),
      min = consts$SAMPLE_SIZE_MIN,
      max = consts$SAMPLE_SIZE_MAX_PROP,
      value = consts$SAMPLE_SIZE_DEFAULT_PROP,
      step = 10
    ),

    shiny::conditionalPanel(
      condition = "input.prop_allocation == 'unequal'",
      shiny::numericInput(
        "prop_ratio",
        label = shiny::div(
          "Group 1 to Group 2 Ratio",
          create_tooltip(text = "Ratio of sample sizes: n1/n2")
        ),
        value = consts$RATIO_DEFAULT,
        min = consts$RATIO_MIN,
        max = consts$RATIO_MAX,
        step = 0.1
      )
    ),

    shiny::htmlOutput("prop_sample_size_display")
  )
}

#' Create Effect Size Input Components for Proportions
#'
#' Generates interactive controls for specifying effect size using one of four methods:
#' two proportions, proportion difference, odds ratio, or relative risk.
#'
#' @keywords internal
#' @return A Shiny tag list containing effect size input controls
create_prop_effect_size_inputs <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::h6("Effect Size Specification"),

    shiny::radioButtons(
      "prop_dmeth",
      "Effect Size Method",
      choices = c(
        "Two Proportions (p1, p2)" = "proportions",
        "Proportion Difference (p1 - p2)" = "difference",
        "Odds Ratio (OR)" = "or",
        "Relative Risk (RR)" = "rr"
      ),
      selected = "proportions"
    ),

    # Two Proportions Method
    shiny::conditionalPanel(
      condition = "input.prop_dmeth == 'proportions'",
      shiny::sliderInput(
        "prop_p1",
        label = shiny::div(
          "Proportion in Group 1 (p1)",
          create_tooltip(text = "Expected proportion in treatment/group 1")
        ),
        min = consts$PROPORTION_MIN,
        max = consts$PROPORTION_MAX,
        value = consts$PROPORTION_DEFAULT_1,
        step = 0.05
      ),
      shiny::sliderInput(
        "prop_p2",
        label = shiny::div(
          "Proportion in Group 2 (p2)",
          create_tooltip(text = "Expected proportion in control/group 2")
        ),
        min = consts$PROPORTION_MIN,
        max = consts$PROPORTION_MAX,
        value = consts$PROPORTION_DEFAULT_2,
        step = 0.05
      )
    ),

    # Proportion Difference Method
    shiny::conditionalPanel(
      condition = "input.prop_dmeth == 'difference'",
      shiny::sliderInput(
        "prop_diff",
        label = shiny::div(
          "Proportion Difference (p1 - p2)",
          create_tooltip(text = "Difference between proportions")
        ),
        min = consts$PROP_DIFF_MIN,
        max = consts$PROP_DIFF_MAX,
        value = c(consts$PROP_DIFF_DEFAULT_MIN, consts$PROP_DIFF_DEFAULT_MAX),
        step = 0.05
      ),
      shiny::sliderInput(
        "prop_baseline_diff",
        label = shiny::div(
          "Baseline Proportion (p2)",
          create_tooltip(text = "Control group proportion")
        ),
        min = consts$PROPORTION_MIN,
        max = consts$PROPORTION_MAX,
        value = consts$BASELINE_PROPORTION_DEFAULT,
        step = 0.05
      )
    ),

    # Odds Ratio Method
    shiny::conditionalPanel(
      condition = "input.prop_dmeth == 'or'",
      shiny::sliderInput(
        "prop_or",
        label = shiny::div(
          "Odds Ratio (OR)",
          create_tooltip(text = "Ratio of odds: (p1/(1-p1)) / (p2/(1-p2))")
        ),
        min = consts$ODDS_RATIO_MIN,
        max = consts$ODDS_RATIO_MAX,
        value = c(consts$ODDS_RATIO_DEFAULT_MIN, consts$ODDS_RATIO_DEFAULT_MAX),
        step = 0.1
      ),
      shiny::sliderInput(
        "prop_baseline_or",
        label = shiny::div(
          "Baseline Proportion (p2)",
          create_tooltip(text = "Control group proportion")
        ),
        min = consts$PROPORTION_MIN,
        max = consts$PROPORTION_MAX,
        value = consts$BASELINE_PROPORTION_DEFAULT,
        step = 0.05
      )
    ),

    # Relative Risk Method
    shiny::conditionalPanel(
      condition = "input.prop_dmeth == 'rr'",
      shiny::sliderInput(
        "prop_rr",
        label = shiny::div(
          "Relative Risk (RR)",
          create_tooltip(text = "Ratio of risks: p1 / p2")
        ),
        min = consts$RELATIVE_RISK_MIN,
        max = consts$RELATIVE_RISK_MAX,
        value = c(consts$RELATIVE_RISK_DEFAULT_MIN, consts$RELATIVE_RISK_DEFAULT_MAX),
        step = 0.1
      ),
      shiny::sliderInput(
        "prop_baseline_rr",
        label = shiny::div(
          "Baseline Proportion (p2)",
          create_tooltip(text = "Control group proportion")
        ),
        min = consts$PROPORTION_MIN,
        max = consts$PROPORTION_MAX,
        value = consts$BASELINE_PROPORTION_DEFAULT,
        step = 0.05
      )
    )
  )
}

#' Create Advanced Settings Components for Proportions
#'
#' Generates optional input controls for advanced trial design parameters
#' specific to proportion studies.
#'
#' @keywords internal
#' @return A Shiny tag list containing advanced settings
create_prop_advanced_settings <- function() {
  consts <- ZZPOWER_CONSTANTS

  shiny::tagList(
    shiny::checkboxInput(
      "prop_show_advanced",
      "Show Advanced Settings"
    ),

    shiny::conditionalPanel(
      condition = "input.prop_show_advanced == true",
      shiny::fluidRow(
        shiny::column(6,
          shiny::numericInput(
            "prop_type1",
            label = shiny::div(
              "Type I Error Rate (alpha)",
              create_tooltip(text = "Significance level for statistical testing")
            ),
            value = consts$TYPE1_DEFAULT,
            min = consts$TYPE1_MIN,
            max = consts$TYPE1_MAX,
            step = consts$TYPE1_STEP
          )
        ),
        shiny::column(6,
          shiny::checkboxInput(
            "prop_onesided",
            label = shiny::div(
              "One-sided Test",
              create_tooltip(text = "One-sided vs two-sided hypothesis test")
            ),
            value = FALSE
          )
        )
      )
    )
  )
}
