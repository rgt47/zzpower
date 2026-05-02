#' zzpower: Interactive Power Analysis Calculator for Clinical Trials
#'
#' A 'Shiny' application for power analysis and sample size calculations
#' across a catalogue of statistical tests common in clinical trial design.
#'
#' @description
#' The \pkg{zzpower} package provides an interactive 'Shiny' application
#' covering eleven statistical tests via a registry/plugin architecture:
#' two-group, paired, and one-sample t-tests; two-proportion comparisons;
#' Pearson correlation; log-rank survival; Fisher's exact test;
#' Cochran-Armitage trend; one-way ANOVA; McNemar's test for paired
#' proportions; and a basic linear mixed-model power calculation. Each
#' test supports multiple effect-size specifications, design parameters
#' (alpha, allocation, dropout), and downloadable reports.
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Eleven statistical tests} via the
#'     \code{\link{get_power_test_registry}} registry, each rendered through
#'     a generic UI/server factory pair
#'     (\code{\link{create_generic_test_ui}},
#'     \code{\link{create_generic_test_server}})
#'   \item \strong{Multiple effect-size methods} per test (Cohen's d, h, f,
#'     hazard ratio, odds ratio, percent reduction, etc.)
#'   \item \strong{Sample-size and power solve modes:} compute power at a
#'     given N or N required for target power
#'   \item \strong{Interactive power curves} with reference lines and
#'     formatted result tables (\code{DT})
#'   \item \strong{Downloadable reports} (text and HTML) summarising study
#'     design and power calculation
#'   \item \strong{Design considerations:} dropout, drop-in, allocation
#'     ratio, one-sided vs two-sided testing
#' }
#'
#' @section Getting Started:
#' To launch the interactive application, run:
#' \preformatted{
#' library(zzpower)
#' launch_zzpower()
#' }
#'
#' @author Ronald "Ryy" G. Thomas
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral
#' Sciences (2nd ed.). Lawrence Erlbaum Associates.
#'
#' Schoenfeld, D. A. (1983). Sample-size formula for the proportional-
#' hazards regression model. Biometrics, 39(2), 499-503.
#'
#' Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{launch_zzpower}} for the interactive application
#'   \item \code{\link{get_power_test_registry}} for the test registry
#'   \item The \pkg{pwr} package for the underlying power-calculation
#'     primitives
#' }
#'
#' @docType package
#' @name zzpower-package
#' @aliases zzpower
"_PACKAGE"
