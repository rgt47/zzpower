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
    ttest_2groups    = "#182B49",
    ttest_paired     = "#00629B",
    ttest_one_sample = "#00C6D7",
    anova_oneway     = "#6E963B",
    prop_2groups     = "#C69214",
    fisher_exact     = "#FC8900",
    mcnemar          = "#D462AD",
    trend_prop       = "#C69214",
    mixed_model      = "#00C6D7",
    logrank          = "#747678",
    correlation      = "#6E963B"
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

  category_bg <- c(
    "Continuous Outcomes"     = "#cdd8e8",
    "Binary Outcomes"         = "#fef3c7",
    "Longitudinal & Survival" = "#c0ddd6",
    "Association"             = "#d8cce0"
  )

  hero_panel <- bslib::nav_panel(
    value = "home",
    title = NULL,
    shiny::div(
      style = paste0(
        "background: linear-gradient(135deg, #182B49 0%, #00629B 100%);",
        "padding: 2rem 1rem 2.5rem 1rem; text-align: center;",
        "margin: -1rem -1rem 0 -1rem; position: relative;"
      ),
      shiny::img(
        src = "zzpower-assets/ucsd-logo-white.png",
        style = paste0(
          "position: absolute; top: 1.2rem; right: 1.5rem;",
          "height: 30px; opacity: 0.9;"
        ),
        alt = "UC San Diego"
      ),
      shiny::img(
        src = "zzpower-assets/hex-zzpower.png",
        style = "height: 120px; margin-bottom: 1rem;",
        alt = "zzpower hex sticker"
      ),
      shiny::h1(
        style = "color: #FFCD00; font-weight: 700; margin-bottom: 0.5rem;",
        "zzpower"
      ),
      shiny::p(
        style = "color: #F5F0E6; font-size: 1.1rem; margin-bottom: 0;",
        "Interactive power analysis and sample size",
        "calculations for clinical trial designs"
      )
    ),

    shiny::div(
      class = "container-fluid py-4",

      lapply(names(test_categories), function(category) {
        cat_ids <- intersect(test_categories[[category]], test_ids)
        if (length(cat_ids) == 0) return(NULL)
        bg <- category_bg[[category]] %||% "#f0f0f0"

        shiny::tagList(
          shiny::h6(
            class = "mt-4 mb-3",
            style = "font-weight: 700; color: #182B49; text-transform: uppercase;
                     letter-spacing: 0.05em; font-size: 0.8rem;",
            category
          ),
          bslib::layout_columns(
            col_widths = rep(3, length(cat_ids)),
            !!!lapply(cat_ids, function(tid) {
              spec <- registry[[tid]]
              clr <- card_colors[[tid]] %||% "#182B49"
              bslib::card(
                style = paste0(
                  "cursor: pointer;",
                  "border-left: 4px solid ", clr, ";",
                  "border-top: none; border-right: none; border-bottom: none;",
                  "background-color: ", bg, ";",
                  "transition: transform 0.15s, box-shadow 0.15s;",
                  "min-height: 130px;"
                ),
                class = "shadow-sm",
                id = paste0("card_", tid),
                onclick = sprintf(
                  "Shiny.setInputValue('nav_to', '%s', {priority: 'event'})",
                  spec$name
                ),
                onmouseenter = paste0(
                  "this.style.transform='translateY(-3px)';",
                  "this.style.boxShadow='0 6px 20px rgba(0,0,0,.15)';"
                ),
                onmouseleave = paste0(
                  "this.style.transform='';",
                  "this.style.boxShadow='';"
                ),
                bslib::card_body(
                  class = "text-center d-flex flex-column justify-content-center py-3",
                  shiny::div(
                    class = "mb-2",
                    bsicons::bs_icon(
                      spec$icon %||% "calculator",
                      size = "1.5em",
                      style = paste0("color: ", clr, ";")
                    )
                  ),
                  shiny::p(
                    class = "fw-bold mb-1",
                    style = "font-size: 0.85rem; color: #182B49;",
                    spec$name
                  ),
                  shiny::p(
                    class = "mb-0",
                    style = "font-size: 0.75rem; color: #747678;",
                    spec$description
                  )
                )
              )
            })
          )
        )
      }),

      shiny::div(
        class = "text-center mt-4",
        style = "color: #747678; font-size: 0.8rem;",
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
      primary = "#182B49",
      secondary = "#747678",
      success = "#6E963B",
      info = "#00629B",
      warning = "#C69214",
      danger = "#FC8900",
      bg = "#FFFFFF",
      fg = "#182B49",
      "navbar-bg" = "#182B49"
    ),
    title = "zzpower - Statistical Power Analysis Calculator",
    bslib::navset_hidden(
      id = "main_nav",
      hero_panel,
      !!!test_panels
    )
  )

  www_dir <- system.file("www", package = "zzpower")
  if (www_dir == "") {
    www_dir <- file.path(getwd(), "inst", "www")
  }
  shiny::addResourcePath("zzpower-assets", www_dir)

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
