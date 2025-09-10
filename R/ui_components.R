#' Create UI Components for the Shiny Application
#' 
#' Internal functions to create the user interface components.
#' 
#' @keywords internal
#' @importFrom bslib bs_theme page_sidebar sidebar card card_header tooltip
#' @importFrom bsicons bs_icon  
#' @importFrom shiny sliderInput numericInput radioButtons conditionalPanel checkboxInput fluidRow column tagList div htmlOutput hr verbatimTextOutput plotOutput downloadButton
#' @importFrom DT dataTableOutput
create_ui <- function() {
  
  # Tooltip helper function  
  create_tooltip <- function(icon = bsicons::bs_icon("info-circle"), text) {
    # Using bslib tooltip instead of bsicons
    bslib::tooltip(icon, text, placement = "right")
  }
  
  bslib::page_sidebar(
    title = "Power Analysis Calculator for Two-Group Designs",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    
    sidebar = bslib::sidebar(
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
#' @keywords internal
create_sample_size_inputs <- function() {
  create_tooltip <- function(icon = bsicons::bs_icon("info-circle"), text) {
    bslib::tooltip(icon, text, placement = "right")
  }
  
  shiny::tagList(
    shiny::sliderInput(
      "N", 
      label = shiny::div(
        "Total Sample Size", 
        create_tooltip(text = "Total number of participants in both groups combined")
      ), 
      min = 20, 
      max = 500, 
      value = 100, 
      step = 10
    ),
    
    shiny::sliderInput(
      "dropout", 
      label = shiny::div(
        "Dropout Rate", 
        create_tooltip(text = "Percentage of participants expected to withdraw before study completion")
      ), 
      min = 0, 
      max = 0.5, 
      value = 0.1, 
      step = 0.05
    ),
    
    shiny::htmlOutput("sample_size_display")
  )
}

#' Create Effect Size Input Components  
#' @keywords internal
create_effect_size_inputs <- function() {
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
                        min = 0, max = 2, value = c(0.2, 1.0), step = 0.1)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'diff'",
      shiny::sliderInput("dff", "Difference in Scores", 
                        min = 0, max = 10, value = c(1, 5), step = 0.5),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = 10)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'pct'", 
      shiny::sliderInput("pct", "Percent Reduction", 
                        min = 0, max = 1, value = c(0.1, 0.5), step = 0.05),
      shiny::numericInput("d0", "Placebo Change", value = 10),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = 10)
    ),
    shiny::conditionalPanel(
      condition = "input.dmeth == 'active'",
      shiny::sliderInput("active", "Treatment Group Change", 
                        min = 0, max = 10, value = c(0, 6), step = 0.5),
      shiny::numericInput("d0", "Placebo Change", value = 10),
      shiny::numericInput("sd0", "Placebo Standard Deviation", value = 10)
    )
  )
}

#' Create Advanced Settings Components
#' @keywords internal  
create_advanced_settings <- function() {
  shiny::tagList(
    shiny::checkboxInput("show_advanced", "Show Advanced Settings"),
    shiny::conditionalPanel(
      condition = "input.show_advanced == true",
      shiny::fluidRow(
        shiny::column(6, 
          shiny::numericInput("ratio", "Active to Control Group Ratio", 
                             value = 1, min = 0.5, max = 5, step = 0.5),
          shiny::sliderInput("dropin", "Drop-in Rate", 
                            min = 0, max = 0.4, value = 0, step = 0.05)
        ),
        shiny::column(6,
          shiny::numericInput("type1", "Type I Error Rate", 
                             value = 0.05, min = 0.01, max = 0.2, step = 0.005),
          shiny::checkboxInput("onesided", "One-sided Test")
        )
      )
    )
  )
}