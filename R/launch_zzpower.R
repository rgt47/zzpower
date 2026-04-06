#' Launch the zzpower Shiny Application
#'
#' This function launches the interactive 'Shiny' application for power
#' analysis and sample size calculations using a registry of statistical
#' tests. Each test runs as an independent Shiny module.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @param launch.browser Logical, whether to launch the app in browser.
#'   Default is \code{TRUE}.
#' @param host Character string of IP address to listen on.
#'   Default is "127.0.0.1".
#' @param port Integer specifying the port to listen on.
#'   Default is \code{NULL} (random port).
#'
#' @return No return value, launches the Shiny application
#'
#' @details
#' The application provides interactive power analysis for multiple
#' statistical tests:
#' \itemize{
#'   \item Two-group t-tests (independent samples)
#'   \item Paired t-tests
#'   \item One-sample t-tests
#'   \item Two proportions (binomial comparison)
#'   \item Correlation tests
#'   \item Survival log-rank test
#'   \item Fisher's exact test
#'   \item Cochran-Armitage trend in proportions
#'   \item Multiple effect size specifications per test
#'   \item Interactive power curves and detailed results tables
#'   \item Downloadable reports in text or HTML formats
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the application
#' launch_zzpower()
#'
#' # Launch on specific port
#' launch_zzpower(port = 3838)
#' }
#'
#' @export
#' @importFrom shiny shinyApp req actionLink observeEvent tags
#' @importFrom bslib bs_theme page_fillable navset_hidden nav_panel
#'   nav_panel_hidden layout_column_wrap nav_select
launch_zzpower <- function(..., launch.browser = TRUE,
                           host = "127.0.0.1", port = NULL) {

  registry <- get_power_test_registry()
  test_ids <- names(registry)

  card_colors <- c(
    ttest_2groups  = "#2c3e50",
    ttest_paired   = "#2980b9",
    ttest_one_sample = "#16a085",
    prop_2groups   = "#8e44ad",
    fisher_exact   = "#c0392b",
    trend_prop     = "#d35400",
    correlation    = "#27ae60",
    logrank        = "#e67e22",
    anova_oneway   = "#1abc9c",
    mcnemar        = "#9b59b6",
    mixed_model    = "#34495e"
  )

  test_categories <- list(
    "Continuous Outcomes" = c(
      "ttest_2groups", "ttest_paired", "ttest_one_sample", "anova_oneway"
    ),
    "Binary Outcomes" = c(
      "prop_2groups", "fisher_exact", "mcnemar", "trend_prop"
    ),
    "Longitudinal & Survival" = c(
      "mixed_model", "logrank"
    ),
    "Association" = c(
      "correlation"
    )
  )

  hero_panel <- bslib::nav_panel(
    value = "home",
    title = NULL,
    shiny::div(
      class = "container-fluid py-4",
      shiny::div(
        class = "text-center mb-5",
        shiny::h1(class = "fw-bold", "zzpower"),
        shiny::p(
          class = "lead text-muted",
          "Interactive power analysis and sample size",
          "calculations for clinical trial designs"
        )
      ),

      lapply(names(test_categories), function(category) {
        cat_ids <- intersect(test_categories[[category]], test_ids)
        if (length(cat_ids) == 0) return(NULL)

        shiny::tagList(
          shiny::h5(
            class = "mt-4 mb-3",
            style = "font-weight: 600; color: #555;",
            category
          ),
          bslib::layout_column_wrap(
            width = 1 / min(length(cat_ids), 3),
            heights_equal = "row",
            !!!lapply(cat_ids, function(tid) {
              spec <- registry[[tid]]
              clr <- card_colors[[tid]] %||% "#2c3e50"
              bslib::card(
                style = paste0(
                  "cursor: pointer;",
                  "border-left: 4px solid ", clr, ";",
                  "transition: transform 0.15s, box-shadow 0.15s;"
                ),
                class = "shadow-sm",
                id = paste0("card_", tid),
                onclick = sprintf(
                  "Shiny.setInputValue('nav_to', '%s', {priority: 'event'})",
                  spec$name
                ),
                onmouseenter = paste0(
                  "this.style.transform='translateY(-3px)';",
                  "this.style.boxShadow='0 6px 20px rgba(0,0,0,.12)';"
                ),
                onmouseleave = paste0(
                  "this.style.transform='';",
                  "this.style.boxShadow='';"
                ),
                bslib::card_body(
                  class = "text-center py-4",
                  shiny::div(
                    class = "mb-3",
                    bsicons::bs_icon(
                      spec$icon %||% "calculator",
                      size = "2em",
                      style = paste0("color: ", clr, ";")
                    )
                  ),
                  shiny::h6(class = "fw-bold mb-2", spec$name),
                  shiny::p(
                    class = "text-muted small mb-0",
                    spec$description
                  )
                )
              )
            })
          )
        )
      }),

      shiny::div(
        class = "text-center mt-5 text-muted small",
        shiny::p(
          paste(length(test_ids), "statistical tests"),
          " | Multiple effect size methods",
          " | Downloadable reports"
        )
      )
    )
  )

  back_link <- function(clr) {
    shiny::div(
      class = "mb-3",
      shiny::actionLink(
        "back_home", NULL,
        icon = bsicons::bs_icon("arrow-left"),
        class = "text-decoration-none",
        style = paste0("color: ", clr, "; font-weight: 600;"),
        onclick = paste0(
          "Shiny.setInputValue('nav_to', 'home', {priority: 'event'});",
          "return false;"
        ),
        "Back to all tests"
      )
    )
  }

  test_panels <- lapply(test_ids, function(test_id) {
    test_spec <- registry[[test_id]]
    clr <- card_colors[[test_id]] %||% "#2c3e50"
    bslib::nav_panel(
      value = test_spec$name,
      title = NULL,
      back_link(clr),
      shiny::h4(
        class = "mb-3",
        style = paste0("color: ", clr, "; font-weight: 700;"),
        bsicons::bs_icon(test_spec$icon %||% "calculator"),
        paste0(" ", test_spec$name)
      ),
      create_generic_test_ui(test_id)
    )
  })

  ui <- bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    title = "zzpower - Statistical Power Analysis Calculator",
    bslib::navset_hidden(
      id = "main_nav",
      hero_panel,
      !!!test_panels
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$nav_to, {
      bslib::nav_select("main_nav", selected = input$nav_to)
    })

    lapply(test_ids, function(test_id) {
      test_spec <- registry[[test_id]]
      create_generic_test_server(test_id, test_spec)
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(
      launch.browser = launch.browser,
      host = host,
      port = port,
      ...
    )
  )
}
