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
    correlation      = "#6E963B",
    cluster_rct      = "#1B5E20",
    cluster_prop     = "#33691E",
    cluster_logrank  = "#827717"
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
    ),
    "Cluster-Randomized Designs" = c(
      "cluster_rct", "cluster_prop", "cluster_logrank"
    )
  )

  category_bg <- c(
    "Continuous Outcomes"        = "#cdd8e8",
    "Binary Outcomes"            = "#fef3c7",
    "Longitudinal & Survival"    = "#c0ddd6",
    "Association"                = "#d8cce0",
    "Cluster-Randomized Designs" = "#d4e8d4"
  )

  hero_panel <- bslib::nav_panel(
    value = "home",
    title = NULL,
    # Hero strip: lighter, modern slate gradient with a soft
    # diagonal sheen. The amber wordmark and warm-cream tagline
    # carry forward but on a less institutional ground.
    shiny::div(
      style = paste0(
        "background: linear-gradient(135deg, #1e293b 0%, #334155 60%, #475569 100%);",
        "padding: 2.5rem 1.5rem 3rem 1.5rem; text-align: center;",
        "margin: -1rem -1rem 0 -1rem; position: relative;",
        "border-bottom: 1px solid #e2e8f0;"
      ),
      shiny::img(
        src = "zzpower-assets/ucsd-logo-white.png",
        style = paste0(
          "position: absolute; top: 1.5rem; right: 1.75rem;",
          "height: 28px; opacity: 0.85;"
        ),
        alt = "UC San Diego"
      ),
      shiny::img(
        src = "zzpower-assets/hex-zzpower.png",
        style = "height: 96px; margin-bottom: 1rem;",
        alt = "zzpower hex sticker"
      ),
      shiny::h1(
        style = paste0(
          "color: #FFCD00; font-weight: 700; margin-bottom: 0.75rem; ",
          "font-size: 2.5rem; letter-spacing: -0.02em;"
        ),
        "zzpower"
      ),
      shiny::p(
        style = paste0(
          "color: #f1f5f9; font-size: 1.05rem; ",
          "margin-bottom: 0.75rem; max-width: 720px; ",
          "margin-left: auto; margin-right: auto; ",
          "line-height: 1.5;"
        ),
        "Interactive power analysis and sample size",
        "calculations for clinical trial designs"
      ),
      # Stats tagline lives in the hero now (was clipped at the
      # very bottom of the page before).
      shiny::p(
        style = paste0(
          "color: #cbd5e1; font-size: 0.85rem; ",
          "letter-spacing: 0.04em; margin-bottom: 0;"
        ),
        sprintf("%d statistical tests", length(test_ids)),
        " · Multiple effect-size methods · Downloadable reports"
      )
    ),

    shiny::div(
      class = "container py-4",
      style = "max-width: 1200px;",

      lapply(names(test_categories), function(category) {
        cat_ids <- intersect(test_categories[[category]], test_ids)
        if (length(cat_ids) == 0) return(NULL)

        shiny::tagList(
          shiny::h6(
            class = "zzpower-section-label",
            category
          ),
          bslib::layout_columns(
            # Grid of three columns at lg breakpoint regardless of
            # category size, so a one-card category does not
            # stretch its lone card across the full width.
            col_widths = bslib::breakpoints(
              sm = rep(12, length(cat_ids)),
              md = rep(6, length(cat_ids)),
              lg = rep(4, length(cat_ids))
            ),
            !!!lapply(cat_ids, function(tid) {
              spec <- registry[[tid]]
              clr <- card_colors[tid] %||% "#334155"
              bslib::card(
                style = paste0(
                  "cursor: pointer; ",
                  "border-left: 4px solid ", clr, " !important; ",
                  "min-height: 132px;"
                ),
                class = "zzpower-launcher-card",
                id = paste0("card_", tid),
                role = "button",
                tabindex = "0",
                `aria-label` = paste("Open", spec$name),
                onclick = sprintf(
                  "Shiny.setInputValue('nav_to', '%s', {priority: 'event'})",
                  spec$name
                ),
                onkeydown = sprintf(
                  paste0(
                    "if (event.key === 'Enter' || event.key === ' ') ",
                    "{ event.preventDefault(); ",
                    "Shiny.setInputValue('nav_to', '%s', ",
                    "{priority: 'event'}); }"
                  ),
                  spec$name
                ),
                onmouseenter = paste0(
                  "this.style.transform='translateY(-2px)';",
                  "this.style.boxShadow='0 8px 24px rgba(15,23,42,0.10)';"
                ),
                onmouseleave = paste0(
                  "this.style.transform='';",
                  "this.style.boxShadow='';"
                ),
                bslib::card_body(
                  class = "py-3 px-4",
                  shiny::div(
                    style = paste0(
                      "display: flex; align-items: flex-start; ",
                      "gap: 0.85rem;"
                    ),
                    shiny::div(
                      style = paste0(
                        "flex-shrink: 0; width: 40px; height: 40px; ",
                        "border-radius: 10px; ",
                        "background: ", clr, "14; ",
                        "display: flex; align-items: center; ",
                        "justify-content: center;"
                      ),
                      bsicons::bs_icon(
                        spec$icon %||% "calculator",
                        size = "1.25em",
                        style = paste0("color: ", clr, ";")
                      )
                    ),
                    shiny::div(
                      style = "flex: 1; min-width: 0;",
                      shiny::p(
                        class = "fw-semibold mb-1",
                        style = paste0(
                          "font-size: 0.95rem; color: #0f172a; ",
                          "line-height: 1.3;"
                        ),
                        spec$name
                      ),
                      shiny::p(
                        class = "mb-0 text-muted",
                        style = paste0(
                          "font-size: 0.78rem; line-height: 1.45;"
                        ),
                        spec$description
                      )
                    )
                  )
                )
              )
            })
          )
        )
      }),

      # Tagline moved into the hero strip; bottom of the hero
      # panel just leaves breathing room.
      shiny::div(class = "mb-4")
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
    clr <- card_colors[test_id] %||% "#2c3e50"
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
      # Refined palette: warm slate over institutional navy,
      # softer neutrals, body sans = Inter for a contemporary feel.
      primary   = "#334155",   # slate-700
      secondary = "#64748b",   # slate-500
      success   = "#15803d",   # green-700
      info      = "#0891b2",   # cyan-600
      warning   = "#b45309",   # amber-700
      danger    = "#b91c1c",   # red-700
      bg        = "#fafaf9",   # warm off-white
      fg        = "#1e293b",   # slate-800
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Inter"),
      "navbar-bg" = "#1e293b",
      "card-border-color"  = "#e2e8f0",
      "card-cap-bg"        = "#ffffff",
      "card-bg"            = "#ffffff",
      "border-radius"      = "10px",
      "border-radius-sm"   = "8px",
      "border-radius-lg"   = "14px"
    ),
    title = "zzpower - Statistical Power Analysis Calculator",
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        # Soft page background + breathing room.
        "body { ",
        "  background: #fafaf9; ",
        "  color: #1e293b; ",
        "}",
        # Cards: lighter borders, soft shadow, rounded corners,
        # a smooth hover lift on launcher cards.
        ".card { ",
        "  border: 1px solid #e2e8f0 !important; ",
        "  box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04); ",
        "  border-radius: 12px; ",
        "}",
        ".card-header { ",
        "  background: #ffffff !important; ",
        "  border-bottom: 1px solid #f1f5f9 !important; ",
        "  font-weight: 600; ",
        "  letter-spacing: 0.01em; ",
        "}",
        # Launcher cards on the hero panel.
        ".zzpower-launcher-card { ",
        "  border-radius: 12px !important; ",
        "  transition: transform 0.18s ease, ",
        "              box-shadow 0.18s ease; ",
        "}",
        ".zzpower-launcher-card:focus, ",
        ".zzpower-launcher-card:focus-visible { ",
        "  outline: 3px solid #0891b2; ",
        "  outline-offset: 2px; ",
        "}",
        # Value boxes: lighter typography, less aggressive
        # backgrounds, larger value font.
        ".bslib-value-box { ",
        "  height: 120px !important; ",
        "  min-height: 120px !important; ",
        "  max-height: 120px !important; ",
        "  overflow: hidden; ",
        "  border-radius: 12px !important; ",
        "}",
        ".bslib-value-box .value-box-title { ",
        "  font-weight: 500; ",
        "  font-size: 0.85rem; ",
        "  letter-spacing: 0.02em; ",
        "  opacity: 0.95; ",
        "}",
        ".bslib-value-box .value-box-value { ",
        "  font-weight: 700; ",
        "  font-size: 1.7rem; ",
        "  letter-spacing: -0.01em; ",
        "}",
        ".bslib-value-box .shiny-text-output:empty::before { ",
        "  content: '\\00a0'; ",
        "}",
        # DT table: cleaner typographic rhythm.
        "table.dataTable { ",
        "  font-size: 0.92rem; ",
        "  border-spacing: 0 !important; ",
        "}",
        "table.dataTable thead th { ",
        "  background: #f8fafc; ",
        "  border-bottom: 2px solid #cbd5e1 !important; ",
        "  font-weight: 600; ",
        "  font-size: 0.82rem; ",
        "  letter-spacing: 0.03em; ",
        "  text-transform: uppercase; ",
        "  color: #475569; ",
        "}",
        "table.dataTable tbody td { ",
        "  border-top: 1px solid #f1f5f9 !important; ",
        "  padding: 0.6rem 0.75rem !important; ",
        "}",
        # Numeric columns get a tabular-nums treatment so digits
        # align vertically across rows.
        "table.dataTable tbody td.dt-right { ",
        "  font-variant-numeric: tabular-nums; ",
        "  color: #334155; ",
        "}",
        # Sidebar: subtler border + accordion typography.
        ".bslib-sidebar-layout > .sidebar { ",
        "  background: #ffffff; ",
        "  border-right: 1px solid #e2e8f0 !important; ",
        "}",
        ".accordion-button { ",
        "  font-weight: 600; ",
        "  font-size: 0.95rem; ",
        "  letter-spacing: 0.01em; ",
        "}",
        # Section dividers in the test panel.
        ".zzpower-section-label { ",
        "  font-size: 0.78rem; ",
        "  font-weight: 600; ",
        "  text-transform: uppercase; ",
        "  letter-spacing: 0.07em; ",
        "  color: #64748b; ",
        "  margin: 1.25rem 0 0.5rem 0; ",
        "}",
        # Lock the busy-pulse bar to the top of the viewport.
        ".shiny-busy-indicator-pulse, .shiny-busy-indicator { ",
        "  position: fixed !important; ",
        "  top: 0; left: 0; right: 0; ",
        "  z-index: 10000; ",
        "}",
        # Buttons: tighter, slightly softer.
        ".btn { ",
        "  border-radius: 8px; ",
        "  font-weight: 500; ",
        "}",
        ".btn-sm { ",
        "  font-size: 0.82rem; ",
        "  padding: 0.3rem 0.65rem; ",
        "}"
      ))
    ),
    shiny::useBusyIndicators(spinners = FALSE, pulse = TRUE),
    bslib::navset_hidden(
      id = "main_nav",
      hero_panel,
      !!!test_panels
    ),
    shiny::tags$footer(
      class = "mt-4 pt-3 border-top text-muted small",
      style = "text-align: center;",
      sprintf(
        "zzpower v%s",
        as.character(utils::packageVersion("zzpower"))
      ),
      " | ",
      shiny::tags$a(
        href = "https://github.com/rgt47/zzpower",
        target = "_blank", rel = "noopener noreferrer",
        "Source"
      ),
      " | ",
      shiny::tags$a(
        href = "https://github.com/rgt47/zzpower/issues",
        target = "_blank", rel = "noopener noreferrer",
        "Issues"
      ),
      " | ",
      shiny::actionLink("show_citation", "How to cite",
                          style = "color: inherit;"),
      " | ",
      shiny::tags$a(
        href = "https://www.gnu.org/licenses/gpl-3.0.html",
        target = "_blank", rel = "noopener noreferrer",
        "GPL-3"
      )
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

    shiny::observeEvent(input$show_citation, {
      cit <- tryCatch(
        utils::citation("zzpower"),
        error = function(e) NULL
      )
      cit_text <- if (!is.null(cit)) {
        paste(format(cit, style = "text"), collapse = "\n\n")
      } else {
        paste(
          "Thomas, R. G. (2026). zzpower: Interactive Power",
          "Analysis Calculator for Clinical Trial Designs.",
          sprintf("R package version %s.",
                   as.character(utils::packageVersion("zzpower"))),
          "https://github.com/rgt47/zzpower"
        )
      }
      bib_text <- if (!is.null(cit)) {
        paste(format(cit, style = "bibtex"), collapse = "\n")
      } else {
        ""
      }
      shiny::showModal(shiny::modalDialog(
        title = "How to cite zzpower",
        size = "l",
        easyClose = TRUE,
        shiny::tags$p(class = "text-muted small",
                       "Suggested citation:"),
        shiny::tags$pre(
          style = "white-space: pre-wrap; font-size: 0.85rem;",
          cit_text
        ),
        if (nzchar(bib_text)) {
          shiny::tagList(
            shiny::tags$p(class = "text-muted small mt-3",
                           "BibTeX:"),
            shiny::tags$pre(
              style = "font-size: 0.8rem;",
              bib_text
            )
          )
        },
        footer = shiny::modalButton("Close")
      ))
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
