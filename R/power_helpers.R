#' Log-rank Test Power (Schoenfeld Formula)
#'
#' Computes power for the log-rank test comparing two survival curves.
#' Uses the Schoenfeld (1981) formula based on total expected events.
#' Parameters n1 and n2 represent expected events per group (not raw
#' sample sizes), as the event probability is applied in
#' sample_size_calc before these values reach this function.
#'
#' @param h Standardized effect size: log(hazard ratio)
#' @param n1 Expected events in group 1
#' @param n2 Expected events in group 2
#' @param sig.level Significance level
#' @param alternative "two.sided" or "one.sided"
#'
#' @return List with \code{power} element
#'
#' @references
#' Schoenfeld DA (1981). The asymptotic properties of nonparametric
#' tests for comparing survival distributions. Biometrika, 68(1),
#' 316-319.
#'
#' @export
#' @keywords internal
logrank_power <- function(h, n1, n2, sig.level, alternative = "two.sided") {
  total_events <- n1 + n2
  if (total_events <= 0) return(list(power = NA))

  prop1 <- n1 / total_events
  prop2 <- n2 / total_events

  z_alpha <- if (alternative == "two.sided") {
    stats::qnorm(1 - sig.level / 2)
  } else {
    stats::qnorm(1 - sig.level)
  }

  z_power <- sqrt(total_events * prop1 * prop2) * abs(h) - z_alpha
  list(power = stats::pnorm(z_power))
}

#' Cochran-Armitage Trend Test Power
#'
#' Computes power for the Cochran-Armitage test for linear trend in
#' proportions across ordered groups. Uses the chi-square approximation
#' with 1 degree of freedom. The standardized effect size d encodes
#' the trend strength such that the non-centrality parameter is
#' n * d^2.
#'
#' @param n Total sample size across all dose groups
#' @param d Standardized trend effect size
#' @param sig.level Significance level
#' @param alternative "two.sided" or "one.sided"
#'
#' @return List with \code{power} element
#'
#' @references
#' Cochran WG (1954). Some methods for strengthening the common
#' chi-squared tests. Biometrics, 10(4), 417-451.
#'
#' Armitage P (1955). Tests for linear trends in proportions and
#' frequencies. Biometrics, 11(3), 375-386.
#'
#' @export
#' @keywords internal
trend_power <- function(n, d, sig.level, alternative = "two.sided") {
  ncp <- n * d^2
  if (ncp <= 0) return(list(power = NA))

  if (alternative == "two.sided") {
    crit <- stats::qchisq(1 - sig.level, df = 1)
    power <- 1 - stats::pchisq(crit, df = 1, ncp = ncp)
  } else {
    z_alpha <- stats::qnorm(1 - sig.level)
    power <- stats::pnorm(sqrt(ncp) - z_alpha)
  }

  list(power = power)
}

#' McNemar Test Power (Connor 1987)
#'
#' Computes power for McNemar's test comparing paired binary outcomes
#' using the normal approximation from Connor (1987). The effect size
#' h represents the difference in discordant proportions (p10 - p01),
#' and n is the total number of pairs. The overall discordant rate
#' (p10 + p01) is folded into the standardized effect size by the
#' spec's standardize function.
#'
#' @param n Number of pairs
#' @param d Standardized effect size: |p10 - p01| / sqrt(p_disc - (p10-p01)^2)
#' @param sig.level Significance level
#' @param alternative "two.sided" or "one.sided"
#'
#' @return List with \code{power} element
#'
#' @references
#' Connor RJ (1987). Sample size for testing differences in
#' proportions for the paired-sample design. Biometrics, 43(1),
#' 207-211.
#'
#' @export
#' @keywords internal
mcnemar_power <- function(n, d, sig.level, alternative = "two.sided") {
  if (n <= 0 || d <= 0) return(list(power = NA))

  z_alpha <- if (alternative == "two.sided") {
    stats::qnorm(1 - sig.level / 2)
  } else {
    stats::qnorm(1 - sig.level)
  }

  z_power <- abs(d) * sqrt(n) - z_alpha
  list(power = stats::pnorm(z_power))
}

#' Longitudinal Mixed Model Power (Diggle et al. 2002)
#'
#' Computes power for a two-group comparison of slopes in a linear
#' mixed model with equally spaced time points and compound symmetry
#' correlation. Based on the formula from Diggle et al. (2002, p. 29).
#'
#' The standardized effect size d incorporates the slope difference,
#' residual variance, within-subject correlation, and number of time
#' points. The parameter n represents the per-group sample size.
#'
#' @param n Per-group sample size
#' @param d Standardized slope effect size:
#'   delta * sqrt(S_tt / (2 * sigma^2 * (1 - rho)))
#' @param sig.level Significance level
#' @param alternative "two.sided" or "one.sided"
#'
#' @return List with \code{power} element
#'
#' @references
#' Diggle PJ, Heagerty P, Liang K-Y, Zeger SL (2002). Analysis of
#' Longitudinal Data, 2nd ed. Oxford University Press, p. 29.
#'
#' @export
#' @keywords internal
mixed_model_power <- function(n, d, sig.level, alternative = "two.sided") {
  if (n <= 0 || d <= 0) return(list(power = NA))

  z_alpha <- if (alternative == "two.sided") {
    stats::qnorm(1 - sig.level / 2)
  } else {
    stats::qnorm(1 - sig.level)
  }

  z_power <- abs(d) * sqrt(n) - z_alpha
  list(power = stats::pnorm(z_power))
}
