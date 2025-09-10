#' zzpower: Power Analysis Calculator for Two-Group Parallel Designs
#'
#' A comprehensive 'Shiny' application for conducting power analysis and sample 
#' size calculations for two-group parallel clinical trial designs.
#'
#' @description
#' The \pkg{zzpower} package provides an interactive 'Shiny' application for 
#' power analysis and sample size calculations specifically designed for 
#' two-group parallel clinical trial designs. The application supports multiple
#' effect size specifications, accounts for dropout rates and unequal group 
#' allocation ratios, and provides interactive visualizations with downloadable
#' reports.
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Multiple Effect Size Methods:} Supports Cohen's d, percentage 
#'     reductions, difference in change scores, and treatment group changes
#'   \item \strong{Comprehensive Design Considerations:} Accounts for dropout 
#'     rates, drop-in rates, and unequal group allocation ratios
#'   \item \strong{Interactive Visualizations:} Real-time power curves and 
#'     detailed results tables
#'   \item \strong{Report Generation:} Downloadable reports in PDF, HTML, or 
#'     Word formats
#'   \item \strong{Advanced Settings:} Configurable Type I error rates and 
#'     one-sided vs two-sided testing
#' }
#'
#' @section Getting Started:
#' To launch the interactive application, simply run:
#' \preformatted{
#' library(zzpower)
#' launch_zzpower()
#' }
#'
#' @section Effect Size Methods:
#' The application supports four different approaches to specifying effect sizes:
#' \describe{
#'   \item{\strong{Standard Deviation Units (Cohen's d):}}{Direct specification 
#'     of standardized effect sizes}
#'   \item{\strong{Percent Reduction:}}{Effect size specified as percentage 
#'     reduction from placebo}
#'   \item{\strong{Difference in Change Scores:}}{Absolute difference between 
#'     treatment groups in outcome units}
#'   \item{\strong{Change in Active Group:}}{Specifying the treatment group 
#'     change directly}
#' }
#'
#' @section Sample Size Calculations:
#' The application calculates both intention-to-treat (ITT) and completer sample
#' sizes, accounting for:
#' \itemize{
#'   \item Expected dropout rates
#'   \item Drop-in rates (control group receiving treatment)
#'   \item Unequal randomization ratios between groups
#' }
#'
#' @author Ronald "Ryy" G. Thomas
#' 
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences 
#' (2nd ed.). Lawrence Erlbaum Associates.
#' 
#' @seealso
#' \itemize{
#'   \item \code{\link{launch_zzpower}} - Launch the interactive application
#'   \item \code{\link[pwr]{pwr.t2n.test}} - Underlying power calculation function
#' }
#'
#' @docType package
#' @name zzpower-package
#' @aliases zzpower
"_PACKAGE"