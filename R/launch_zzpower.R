#' Launch the zzpower Shiny Application
#'
#' This function launches the interactive 'Shiny' application for power analysis
#' and sample size calculations for two-group parallel designs.
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
#' The application provides interactive power analysis for two-group parallel 
#' clinical trial designs with the following features:
#' \itemize{
#'   \item Multiple effect size specifications (Cohen's d, percentage reduction, 
#'     difference in change scores, treatment group change)
#'   \item Accounts for dropout rates and unequal group allocation ratios
#'   \item Interactive power curves and detailed results tables
#'   \item Downloadable reports in PDF, HTML, or Word formats
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
#' @importFrom shiny shinyApp
#' @importFrom bslib bs_theme page_sidebar sidebar card card_header
#' @importFrom bsicons bs_icon
#' @importFrom bslib tooltip
#' @importFrom ggplot2 ggplot aes geom_line geom_hline scale_y_continuous scale_x_continuous theme_bw
#' @importFrom pwr pwr.t2n.test
launch_zzpower <- function(..., launch.browser = TRUE, host = "127.0.0.1", port = NULL) {
  
  # Create the UI
  ui <- create_ui()
  
  # Create the server
  server <- create_server()
  
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