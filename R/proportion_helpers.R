#' Proportion Effect Size Conversions
#'
#' Helper functions to convert between different effect size representations
#' for two-proportion comparisons (pwr.2p2n.test).
#'
#' @keywords internal

#' Calculate Cohen's h from Two Proportions
#'
#' Cohen's h is the standardized difference between two proportions:
#' h = 2 * (arcsin(sqrt(p1)) - arcsin(sqrt(p2)))
#'
#' @param p1 Proportion in group 1 (0 to 1)
#' @param p2 Proportion in group 2 (0 to 1)
#'
#' @return Cohen's h effect size
#'
#' @keywords internal
#' @examples
#' prop_to_cohens_h(0.6, 0.4)  # Returns ~0.41
prop_to_cohens_h <- function(p1, p2) {
  2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
}

#' Calculate Two Proportions from Cohen's h
#'
#' Inverse transformation: given p2 and Cohen's h, calculate p1
#'
#' @param h Cohen's h effect size
#' @param p2 Proportion in group 2 (0 to 1)
#'
#' @return Proportion in group 1
#'
#' @keywords internal
cohens_h_to_prop <- function(h, p2) {
  # h = 2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
  # Solve for p1:
  asin_p2 <- asin(sqrt(p2))
  asin_p1 <- h / 2 + asin_p2
  # Constrain to valid range
  asin_p1 <- pmin(pmax(asin_p1, 0), pi / 2)
  p1 <- sin(asin_p1) ^ 2
  return(p1)
}

#' Calculate Cohen's h from Proportion Difference
#'
#' Convert proportion difference to Cohen's h given a baseline proportion
#'
#' @param diff Proportion difference (p1 - p2), range -1 to 1
#' @param baseline Baseline proportion (p2), range 0 to 1
#'
#' @return Cohen's h effect size
#'
#' @keywords internal
#' @examples
#' # If baseline is 50% and difference is -20%, p1=30%, p2=50%
#' diff_to_cohens_h(-0.2, 0.5)  # Returns h for (0.3, 0.5)
diff_to_cohens_h <- function(diff, baseline) {
  p2 <- baseline
  p1 <- baseline + diff
  # Constrain to valid range
  p1 <- pmin(pmax(p1, 0.001), 0.999)
  prop_to_cohens_h(p1, p2)
}

#' Calculate Cohen's h from Odds Ratio
#'
#' Convert odds ratio to Cohen's h given a baseline proportion
#'
#' @param or Odds ratio (OR), range 0.1 to 10
#' @param baseline Baseline proportion (p2), range 0 to 1
#'
#' @return Cohen's h effect size
#'
#' @keywords internal
#' @examples
#' # If baseline is 50% and OR is 2
#' or_to_cohens_h(2, 0.5)
or_to_cohens_h <- function(or, baseline) {
  p2 <- baseline
  # OR = (p1/(1-p1)) / (p2/(1-p2))
  # Solve for p1:
  odds_2 <- p2 / (1 - p2)
  odds_1 <- or * odds_2
  p1 <- odds_1 / (1 + odds_1)
  # Constrain to valid range
  p1 <- pmin(pmax(p1, 0.001), 0.999)
  prop_to_cohens_h(p1, p2)
}

#' Calculate Cohen's h from Relative Risk
#'
#' Convert relative risk to Cohen's h given a baseline proportion
#'
#' @param rr Relative risk (RR), range 0.1 to 10
#' @param baseline Baseline proportion (p2), range 0 to 1
#'
#' @return Cohen's h effect size
#'
#' @keywords internal
#' @examples
#' # If baseline is 50% and RR is 1.5
#' rr_to_cohens_h(1.5, 0.5)
rr_to_cohens_h <- function(rr, baseline) {
  p2 <- baseline
  # RR = p1 / p2
  # Solve for p1:
  p1 <- rr * p2
  # Constrain to valid range
  p1 <- pmin(pmax(p1, 0.001), 0.999)
  prop_to_cohens_h(p1, p2)
}

#' Generate Effect Size Range for Proportion Studies
#'
#' Creates a sequence of Cohen's h values for power curve visualization
#' based on the specified effect size method.
#'
#' @param method Effect size method: "proportions", "difference", "or", or "rr"
#' @param p1 Proportion in group 1 (for "proportions" method)
#' @param p2 Proportion in group 2 (for "proportions" method)
#' @param diff_min Minimum proportion difference (for "difference" method)
#' @param diff_max Maximum proportion difference (for "difference" method)
#' @param or_min Minimum odds ratio (for "or" method)
#' @param or_max Maximum odds ratio (for "or" method)
#' @param rr_min Minimum relative risk (for "rr" method)
#' @param rr_max Maximum relative risk (for "rr" method)
#' @param baseline Baseline proportion (for "difference", "or", "rr" methods)
#' @param n_points Number of points in the sequence (default: 16)
#'
#' @return List with components:
#'   - effect_sizes: Original effect size scale values
#'   - cohens_h: Cohen's h equivalents for pwr.2p2n.test
#'
#' @keywords internal
generate_prop_effect_sizes <- function(method = "proportions",
                                       p1 = NULL, p2 = NULL,
                                       diff_min = NULL, diff_max = NULL,
                                       or_min = NULL, or_max = NULL,
                                       rr_min = NULL, rr_max = NULL,
                                       baseline = 0.5,
                                       n_points = 16) {

  if (method == "proportions") {
    # For proportions method, vary p1 while keeping p2 constant
    if (is.null(p1) || is.null(p2)) {
      p1 <- 0.5
      p2 <- 0.3
    }

    # Create range around p1
    p1_min <- max(0.01, p1 - 0.25)
    p1_max <- min(0.99, p1 + 0.25)
    p1_seq <- seq(p1_min, p1_max, length.out = n_points)

    effect_sizes <- p1_seq
    cohens_h <- sapply(p1_seq, function(p) prop_to_cohens_h(p, p2))

  } else if (method == "difference") {
    if (is.null(diff_min) || is.null(diff_max)) {
      diff_min <- -0.3
      diff_max <- -0.05
    }

    effect_sizes <- seq(diff_min, diff_max, length.out = n_points)
    cohens_h <- sapply(effect_sizes, function(d) {
      diff_to_cohens_h(d, baseline)
    })

  } else if (method == "or") {
    if (is.null(or_min) || is.null(or_max)) {
      or_min <- 1.2
      or_max <- 3
    }

    effect_sizes <- seq(or_min, or_max, length.out = n_points)
    cohens_h <- sapply(effect_sizes, function(o) {
      or_to_cohens_h(o, baseline)
    })

  } else if (method == "rr") {
    if (is.null(rr_min) || is.null(rr_max)) {
      rr_min <- 1.2
      rr_max <- 3
    }

    effect_sizes <- seq(rr_min, rr_max, length.out = n_points)
    cohens_h <- sapply(effect_sizes, function(r) {
      rr_to_cohens_h(r, baseline)
    })

  } else {
    stop("Unknown method: ", method)
  }

  list(
    effect_sizes = effect_sizes,
    cohens_h = cohens_h
  )
}

#' Get Method-Specific Effect Size Label
#'
#' Returns a formatted label for displaying effect size based on method
#'
#' @param method Effect size method
#' @param value Effect size value
#'
#' @return Formatted label
#'
#' @keywords internal
get_prop_effect_label <- function(method, value) {
  switch(method,
    "proportions" = sprintf("p1=%.3f", value),
    "difference" = sprintf("Difference=%.3f", value),
    "or" = sprintf("OR=%.3f", value),
    "rr" = sprintf("RR=%.3f", value),
    sprintf("ES=%.3f", value)
  )
}

#' Get X-axis Label for Proportion Power Curves
#'
#' Returns a formatted axis label for the power curve plot
#'
#' @param method Effect size method
#'
#' @return Axis label
#'
#' @keywords internal
get_prop_axis_label <- function(method) {
  switch(method,
    "proportions" = "Group 1 Proportion",
    "difference" = "Proportion Difference (p1 - p2)",
    "or" = "Odds Ratio (OR)",
    "rr" = "Relative Risk (RR)",
    "Effect Size"
  )
}
