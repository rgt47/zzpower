#' Launch the zzpower Shiny Application
#'
#' This function launches the interactive 'Shiny' application for power analysis
#' and sample size calculations using a registry of statistical tests.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @param launch.browser Logical, whether to launch the app in browser.
#'   Default is \code{TRUE}.
#' @param host Character string of IP address to listen on. Default is "127.0.0.1".
#' @param port Integer specifying the port to listen on. Default is \code{NULL}
#'   (random port).
#'
#' @return No return value, launches the Shiny application
#'
#' @details
#' The application provides interactive power analysis for multiple statistical tests:
#' \itemize{
#'   \item Two-group t-tests (independent samples)
#'   \item Paired t-tests
#'   \item One-sample t-tests
#'   \item Two proportions (binomial comparison)
#'   \item Correlation tests
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
#' @importFrom shiny shinyApp navset_tab nav_panel
#' @importFrom bslib bs_theme page_fillable
launch_zzpower <- function(..., launch.browser = TRUE, host = "127.0.0.1", port = NULL) {

  # Get the registry of all available tests
  registry <- get_power_test_registry()
  test_ids <- names(registry)

  # Create UI with tabbed interface
  ui <- bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    title = "zzpower - Statistical Power Analysis Calculator",

    # Create tabbed navigation
    shiny::navset_tab(
      !!!lapply(test_ids, function(test_id) {
        test_spec <- registry[[test_id]]
        shiny::nav_panel(
          title = test_spec$name,
          icon = bsicons::bs_icon(test_spec$icon %||% "calculator"),
          create_generic_test_ui(test_id)
        )
      })
    )
  )

  # Create server function that handles all tests
  server <- function(input, output, session) {
    # Create server logic for each test in the registry
    for (test_id in test_ids) {
      test_spec <- registry[[test_id]]
      create_generic_test_server(test_id, test_spec, get_power_test_registry)
    }
  }

  # Run the app
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