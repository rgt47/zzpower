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
#' @importFrom bslib bs_theme page_fillable navset_tab nav_panel
#'   nav_panel_hidden value_box layout_column_wrap nav_select
launch_zzpower <- function(..., launch.browser = TRUE,
                           host = "127.0.0.1", port = NULL) {

  registry <- get_power_test_registry()
  test_ids <- names(registry)

  test_categories <- list(
    "Continuous Outcomes" = c(
      "ttest_2groups", "ttest_paired", "ttest_one_sample"
    ),
    "Binary Outcomes" = c(
      "prop_2groups", "fisher_exact", "trend_prop"
    ),
    "Other Designs" = c(
      "correlation", "logrank"
    )
  )

  hero_panel <- bslib::nav_panel(
    title = "Home",
    icon = bsicons::bs_icon("house"),
    shiny::div(
      class = "container-fluid py-4",
      shiny::div(
        class = "text-center mb-4",
        shiny::h2("zzpower"),
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
          shiny::h5(class = "mt-4 mb-3 text-muted", category),
          bslib::layout_column_wrap(
            width = 1 / min(length(cat_ids), 3),
            heights_equal = "row",
            !!!lapply(cat_ids, function(tid) {
              spec <- registry[[tid]]
              bslib::card(
                class = "border-0 shadow-sm",
                style = "cursor: pointer;",
                id = paste0("card_", tid),
                onclick = sprintf(
                  "Shiny.setInputValue('nav_to', '%s', {priority: 'event'})",
                  spec$name
                ),
                bslib::card_body(
                  class = "text-center py-4",
                  shiny::div(
                    class = "mb-3",
                    bsicons::bs_icon(
                      spec$icon %||% "calculator",
                      size = "2em",
                      class = "text-primary"
                    )
                  ),
                  shiny::h6(class = "fw-bold", spec$name),
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

  test_panels <- lapply(test_ids, function(test_id) {
    test_spec <- registry[[test_id]]
    bslib::nav_panel(
      title = test_spec$name,
      icon = bsicons::bs_icon(test_spec$icon %||% "calculator"),
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
    bslib::navset_tab(
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
