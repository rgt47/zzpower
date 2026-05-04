#' Power Test Registry
#'
#' Centralized registry defining all available power analysis tests.
#' Each entry defines the parameters, effect size methods, and power calculation
#' for a specific statistical test.
#'
#' @keywords internal

#' Canonicalize a sample-size calculation result
#'
#' Converts per-arm enrolled counts plus a dropout rate into the
#' canonical four-layer record (`n_per_arm_evaluable`,
#' `n_per_arm_enrolled`, `n_total_evaluable`, `n_total_enrolled`)
#' that every grant-writing artifact (Gaps 1, 2, 3, 9) reads from.
#' Back-compat scalars (`n`, `n1`, `n2`, `n1_itt`, `n2_itt`, `k`)
#' are populated so consumers that read the legacy keys keep
#' working unchanged.
#'
#' @param per_arm_enrolled Numeric vector of enrolled (pre-dropout)
#'   counts per arm. Length 1 for single-sample tests, 2 for
#'   two-arm tests, k for ANOVA-style designs.
#' @param dropout Expected dropout rate in [0, 1).
#' @param arm_labels Optional character vector of arm names for
#'   display. Defaults to "Sample" for one arm, "Group i" otherwise.
#'
#' @return Named list with the canonical fields plus legacy shims.
#'
#' @keywords internal
.canonicalize_sample_sizes <- function(per_arm_enrolled,
                                       dropout = 0,
                                       arm_labels = NULL) {
  per_arm_enrolled  <- as.numeric(per_arm_enrolled)
  per_arm_evaluable <- per_arm_enrolled * (1 - dropout)
  n_arms <- length(per_arm_enrolled)

  if (is.null(arm_labels)) {
    arm_labels <- if (n_arms == 1L) {
      "Sample"
    } else {
      paste("Group", seq_len(n_arms))
    }
  }

  list(
    n_per_arm_evaluable = per_arm_evaluable,
    n_per_arm_enrolled  = per_arm_enrolled,
    n_total_evaluable   = sum(per_arm_evaluable),
    n_total_enrolled    = sum(per_arm_enrolled),
    n_arms              = n_arms,
    arm_labels          = arm_labels,
    dropout             = dropout,

    # Back-compat scalars consumed by generic_server_factory.R
    # and the report builders. Evaluable values (post-dropout)
    # are what the power-calc functions expect.
    n  = if (n_arms == 1L) per_arm_evaluable else per_arm_evaluable[1L],
    n1 = if (n_arms >= 1L) per_arm_evaluable[1L] else NULL,
    n2 = if (n_arms >= 2L) per_arm_evaluable[2L] else NULL,
    n1_itt = if (n_arms >= 1L) per_arm_enrolled[1L] else NULL,
    n2_itt = if (n_arms >= 2L) per_arm_enrolled[2L] else NULL,
    k  = if (n_arms >= 2L) n_arms else NULL
  )
}

#' Build a calc_context for a single power calculation
#'
#' Bundles inputs, outputs, provenance, and metadata for one
#' power/sample-size calculation into a single named list. Every
#' grant-writing artifact (Gap 1 methods paragraph, Gap 2 table
#' builder, Gap 9 reproducibility script, Gap 3 multi-aim aggregator)
#' reads its values from a calc_context. Building the context once
#' from inputs and consuming it from many places replaces what
#' would otherwise be eleven specs × five output-paste sites.
#'
#' This is the structural shape consumed by the programmatic API
#' (`power_calc()`, Gap 10) and the Wave 2 generators. Field
#' values populated in earlier waves are treated as ground truth;
#' fields populated in later waves default to NULL until then.
#'
#' @param test_spec A test spec (one of the elements of
#'   `get_power_test_registry()`).
#' @param sample_sizes Output of the spec's `sample_size_calc()`,
#'   already canonicalized via `.canonicalize_sample_sizes()`.
#' @param effect_size Numeric scalar effect size on the native
#'   scale of the chosen `effect_method`.
#' @param effect_size_std Numeric scalar effect size on the
#'   standardized scale (Cohen's d, h, f, or analogous).
#' @param effect_method Character string identifying which entry
#'   of `test_spec$effect_size_methods` was selected.
#' @param effect_params Named list of method-dependent parameters
#'   (e.g. `sd0`, `p2`, `baseline`).
#' @param target_power Target power threshold (sample-size mode)
#'   or NULL (power mode).
#' @param achieved_power Achieved power at the proposed N (power
#'   mode) or NULL (sample-size mode).
#' @param alpha Type I error rate.
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @param effect_source,effect_doi Provenance strings for the
#'   effect-size assumption (Gap 5). Default empty.
#' @param sensitivity_factor Numeric multiplier for the
#'   conservative-effect sentence in the methods paragraph (Gap 6).
#' @param include_sex_paragraph Logical; whether the sex-as-bio
#'   paragraph is appended (Gap 12).
#'
#' @return Named list with the calc_context shape.
#'
#' @keywords internal
.build_calc_context <- function(test_spec,
                                sample_sizes,
                                effect_size,
                                effect_size_std    = NA_real_,
                                effect_method      = NULL,
                                effect_params      = list(),
                                target_power       = NULL,
                                achieved_power     = NULL,
                                alpha              = 0.05,
                                alternative        = "two.sided",
                                effect_source      = "",
                                effect_doi         = "",
                                sensitivity_factor = NULL,
                                include_sex_paragraph = TRUE) {

  list(
    # Identity
    test_id          = test_spec$id,
    test_name        = test_spec$name,
    test_description = test_spec$description,

    # Inputs (the user knobs)
    alpha              = alpha,
    alternative        = alternative,
    target_power       = target_power,
    effect_size        = effect_size,
    effect_size_std    = effect_size_std,
    effect_method      = effect_method,
    effect_params      = effect_params,

    # Provenance (Gaps 5, 6, 12)
    effect_source         = effect_source,
    effect_doi            = effect_doi,
    sensitivity_factor    = sensitivity_factor,
    include_sex_paragraph = include_sex_paragraph,

    # Outputs
    sample_sizes   = sample_sizes,
    achieved_power = achieved_power,

    # Metadata for downstream artifacts
    formula_citation     = test_spec$formula_citation %||% "",
    paragraph_template   = test_spec$paragraph_template,  # may be NULL
    repro_call_template  = test_spec$repro_call,          # may be NULL
    default_effect_grid  = test_spec$default_effect_grid %||% list()
  )
}

#' Get the complete test registry
#'
#' @return List of all available power tests with their configurations
#'
#' @keywords internal
#' @export
get_power_test_registry <- function() {
  list(
    ttest_2groups = create_ttest_2groups_spec(),
    ttest_paired = create_ttest_paired_spec(),
    ttest_one_sample = create_ttest_one_sample_spec(),
    prop_2groups = create_prop_2groups_spec(),
    correlation = create_correlation_spec(),
    logrank = create_logrank_spec(),
    fisher_exact = create_fisher_exact_spec(),
    trend_prop = create_trend_prop_spec(),
    anova_oneway = create_anova_oneway_spec(),
    mcnemar = create_mcnemar_spec(),
    mixed_model = create_mixed_model_spec()
  )
}

#' Two-Group Independent t-test Specification
#'
#' @keywords internal
create_ttest_2groups_spec <- function() {
  list(
    id = "ttest_2groups",
    name = "Two-Group t-test",
    description = "Independent samples t-test for continuous outcomes",
    icon = "bar-chart-line",
    power_function = pwr::pwr.t2n.test,
    effect_size_methods = c("cohens_d", "percent_reduction", "difference", "active_change"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, sec. 2.3 (p. 19).",
    default_effect_grid = list(
      cohens_d  = c(0.2, 0.5, 0.8),
      difference = c(2, 5, 8),
      percent_reduction = c(0.10, 0.25, 0.40)
    ),
    paragraph_template = NULL,  # populated in Wave 2 (Gap 1)
    repro_call         = NULL,  # populated in Wave 3 (Gap 9)

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 500, default = 100, step = 10,
        description = "Total number of participants in both groups"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      ),
      allocation = list(
        type = "radio",
        label = "Sample Size Allocation",
        options = c("equal", "unequal"),
        default = "equal",
        description = "Equal vs unequal group sizes"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio (n1/n2)",
        min = 0.5, max = 5, default = 1, step = 0.1,
        condition = "allocation == 'unequal'",
        description = "Ratio of group 1 to group 2 sample sizes"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size in SD Units"
      ),
      percent_reduction = list(
        min = 0, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Percent Reduction",
        requires = list(sd0 = list(
          type = "numeric", label = "Placebo SD", default = 10
        ))
      ),
      difference = list(
        min = 0, max = 10, default_min = 1, default_max = 5,
        label = "Difference in Scores",
        requires = list(sd0 = list(
          type = "numeric", label = "SD", default = 10
        ))
      ),
      active_change = list(
        min = 0, max = 10, default_min = 0, default_max = 6,
        label = "Treatment Change",
        requires = list(
          d0 = list(type = "numeric", label = "Placebo Change", default = 10),
          sd0 = list(type = "numeric", label = "SD", default = 10)
        )
      )
    ),

    standardize = function(effect_sizes, method, params) {
      # Convert all effect size methods to Cohen's d
      switch(method,
        "cohens_d" = effect_sizes,
        "percent_reduction" = effect_sizes / (params$sd0 %||% 10),
        "difference" = effect_sizes / (params$sd0 %||% 10),
        "active_change" = ((params$d0 %||% 10) - effect_sizes) / (params$sd0 %||% 10),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 100
      allocation <- input$allocation %||% "equal"
      dropout <- input$dropout %||% 0.1

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm, dropout = dropout)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Paired t-test Specification
#'
#' @keywords internal
create_ttest_paired_spec <- function() {
  list(
    id = "ttest_paired",
    name = "Paired t-test",
    description = "Paired samples t-test for before-after designs",
    icon = "arrow-repeat",
    power_function = pwr::pwr.t.test,
    test_type = "paired",
    effect_size_methods = c("cohens_d"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, sec. 2.5 (p. 48).",
    default_effect_grid = list(cohens_d = c(0.2, 0.5, 0.8)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of paired observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size (Cohen's d)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 50
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Pairs"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' One-Sample t-test Specification
#'
#' @keywords internal
create_ttest_one_sample_spec <- function() {
  list(
    id = "ttest_one_sample",
    name = "One-Sample t-test",
    description = "One-sample t-test comparing to a fixed value",
    icon = "bullseye",
    power_function = pwr::pwr.t.test,
    test_type = "one.sample",
    effect_size_methods = c("cohens_d"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, sec. 2.4 (p. 40).",
    default_effect_grid = list(cohens_d = c(0.2, 0.5, 0.8)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size (Cohen's d)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 50
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Sample"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Two Proportions Specification
#'
#' @keywords internal
create_prop_2groups_spec <- function() {
  list(
    id = "prop_2groups",
    name = "Two Proportions",
    description = "Comparison of two binary outcomes (proportions)",
    icon = "percent",
    power_function = pwr::pwr.2p2n.test,
    effect_size_methods = c("proportions", "difference", "odds_ratio", "relative_risk"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 6 (Cohen's h).",
    default_effect_grid = list(
      difference    = c(0.05, 0.10, 0.15, 0.20),
      odds_ratio    = c(1.25, 1.50, 2.00, 3.00),
      relative_risk = c(1.25, 1.50, 2.00, 3.00),
      proportions   = c(0.40, 0.50, 0.60)
    ),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 200, step = 10,
        description = "Total number of participants"
      ),
      allocation = list(
        type = "radio",
        label = "Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio",
        min = 0.5, max = 5, default = 1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      proportions = list(
        min = 0.01, max = 0.99, default_min = 0.3, default_max = 0.5,
        label = "Group Proportions (p1, p2)"
      ),
      difference = list(
        min = -0.5, max = 0.5, default_min = -0.3, default_max = -0.05,
        label = "Proportion Difference"
      ),
      odds_ratio = list(
        min = 0.1, max = 10, default_min = 1.2, default_max = 3,
        label = "Odds Ratio (OR)"
      ),
      relative_risk = list(
        min = 0.1, max = 10, default_min = 1.2, default_max = 3,
        label = "Relative Risk (RR)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      baseline <- params$baseline %||% 0.5
      switch(method,
        "proportions" = sapply(effect_sizes, function(p1) {
          prop_to_cohens_h(p1, params$p2 %||% 0.3)
        }),
        "difference" = sapply(effect_sizes, function(d) {
          diff_to_cohens_h(d, baseline)
        }),
        "odds_ratio" = sapply(effect_sizes, function(or) {
          or_to_cohens_h(or, baseline)
        }),
        "relative_risk" = sapply(effect_sizes, function(rr) {
          rr_to_cohens_h(rr, baseline)
        }),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 200
      allocation <- input$allocation %||% "equal"

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Correlation Test Specification
#'
#' @keywords internal
create_correlation_spec <- function() {
  list(
    id = "correlation",
    name = "Correlation Test",
    description = "Test for correlation between two variables",
    icon = "diagram-2",
    power_function = pwr::pwr.r.test,
    effect_size_methods = c("correlation"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 3 (Pearson r).",
    default_effect_grid = list(correlation = c(0.10, 0.30, 0.50)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      correlation = list(
        min = 0, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Correlation Coefficient (r)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 100
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Sample"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Survival Log-rank Test Specification
#'
#' Power analysis for comparing two survival curves using the log-rank
#' test. Uses the Schoenfeld (1981) formula. The event probability
#' parameter converts total sample sizes to expected event counts
#' before power calculation.
#'
#' @keywords internal
create_logrank_spec <- function() {
  list(
    id = "logrank",
    name = "Survival Log-rank",
    description = "Log-rank test comparing two survival curves",
    icon = "hourglass-split",
    power_function = logrank_power,
    effect_size_methods = c("hazard_ratio"),
    formula_citation = "Schoenfeld DA (1981). The asymptotic properties of nonparametric tests for comparing survival distributions. Biometrika, 68(1), 316-319.",
    default_effect_grid = list(hazard_ratio = c(0.50, 0.65, 0.75)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 200, step = 10,
        description = "Total participants across both groups"
      ),
      event_prob = list(
        type = "slider",
        label = "Event Probability",
        min = 0.1, max = 1, default = 0.7, step = 0.05,
        description = "Probability of observing the event"
      ),
      allocation = list(
        type = "radio",
        label = "Sample Size Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio (n1/n2)",
        min = 0.5, max = 5, default = 1, step = 0.1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      hazard_ratio = list(
        min = 0.1, max = 5, default_min = 1.2, default_max = 3.0,
        label = "Hazard Ratio (HR)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      log(effect_sizes)
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 200
      event_prob <- input$event_prob %||% 0.7
      allocation <- input$allocation %||% "equal"
      dropout <- input$dropout %||% 0.1

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      result <- .canonicalize_sample_sizes(per_arm, dropout = dropout)

      # Schoenfeld's log-rank formula expects expected event counts,
      # not participants. Override the back-compat n1/n2 scalars
      # consumed by the power function with events; canonical
      # n_per_arm_* fields continue to report participants.
      result$n1 <- result$n_per_arm_evaluable[1] * event_prob
      result$n2 <- result$n_per_arm_evaluable[2] * event_prob
      result$expected_events_total <-
        result$n_total_evaluable * event_prob

      result
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      ep <- input$event_prob %||% 0.7
      if (ep <= 0 || ep > 1) {
        issues <- c(issues, "Event probability must be between 0 and 1")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Fisher's Exact Test Specification
#'
#' Power analysis for Fisher's exact test on 2x2 contingency tables.
#' Uses the normal approximation to the exact test (equivalent to
#' pwr.2p2n.test with Cohen's h). Appropriate for small-sample
#' studies with binary outcomes.
#'
#' @keywords internal
create_fisher_exact_spec <- function() {
  list(
    id = "fisher_exact",
    name = "Fisher's Exact Test",
    description = "Exact test for 2x2 contingency tables",
    icon = "grid-3x2",
    power_function = pwr::pwr.2p2n.test,
    effect_size_methods = c("proportions", "odds_ratio"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 6 (Cohen's h normal approximation).",
    default_effect_grid = list(
      proportions = c(0.20, 0.35, 0.50),
      odds_ratio  = c(2.0, 4.0, 8.0)
    ),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 10, max = 200, default = 60, step = 2,
        description = "Total participants in both groups"
      ),
      allocation = list(
        type = "radio",
        label = "Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio",
        min = 0.5, max = 5, default = 1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      proportions = list(
        min = 0.01, max = 0.99, default_min = 0.1, default_max = 0.5,
        label = "Treatment Proportion (p1)",
        requires = list(p2 = list(
          type = "numeric", label = "Control Proportion (p2)", default = 0.1
        ))
      ),
      odds_ratio = list(
        min = 0.1, max = 20, default_min = 2, default_max = 8,
        label = "Odds Ratio (OR)",
        requires = list(baseline = list(
          type = "numeric", label = "Baseline Proportion", default = 0.1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      switch(method,
        "proportions" = sapply(effect_sizes, function(p1) {
          prop_to_cohens_h(p1, params$p2 %||% 0.1)
        }),
        "odds_ratio" = sapply(effect_sizes, function(or) {
          or_to_cohens_h(or, params$baseline %||% 0.1)
        }),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 60
      allocation <- input$allocation %||% "equal"

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 60) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Trend in Proportions Specification (Cochran-Armitage)
#'
#' Power analysis for the Cochran-Armitage test for linear trend in
#' proportions across ordered dose groups. The standardized effect
#' size is derived from the expected proportions at the lowest and
#' highest dose levels assuming a linear dose-response relationship.
#'
#' @keywords internal
create_trend_prop_spec <- function() {
  list(
    id = "trend_prop",
    name = "Trend in Proportions",
    description = "Cochran-Armitage test for dose-response trend",
    icon = "graph-up-arrow",
    power_function = trend_power,
    effect_size_methods = c("prop_range"),
    formula_citation = "Cochran WG (1954). Some methods for strengthening the common chi-squared tests. Biometrics, 10(4), 417-451; Armitage P (1955). Tests for linear trends in proportions and frequencies. Biometrics, 11(3), 375-386.",
    default_effect_grid = list(prop_range = c(0.30, 0.45, 0.60)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 30, max = 1000, default = 150, step = 10,
        description = "Total participants across all dose groups"
      ),
      n_groups = list(
        type = "slider",
        label = "Number of Dose Groups",
        min = 3, max = 8, default = 3, step = 1,
        description = "Number of ordered dose levels"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      prop_range = list(
        min = 0.01, max = 0.99, default_min = 0.2, default_max = 0.6,
        label = "Proportion at Highest Dose",
        requires = list(p_low = list(
          type = "numeric",
          label = "Proportion at Lowest Dose",
          default = 0.1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      p_low <- params$p_low %||% 0.1
      k <- params$n_groups %||% 3

      sapply(effect_sizes, function(p_high) {
        if (k < 2) return(0)
        doses <- seq_len(k) - (k + 1) / 2
        props <- p_low + (seq_len(k) - 1) / (k - 1) * (p_high - p_low)
        p_bar <- mean(props)
        if (p_bar <= 0 || p_bar >= 1) return(0)

        num <- sum(doses * props)
        denom <- p_bar * (1 - p_bar) * sum(doses^2)
        if (denom <= 0) return(0)

        sqrt(num^2 / (k * denom))
      })
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 150
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Total"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 150) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 3) {
        issues <- c(issues, "At least 3 dose groups required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' One-Way ANOVA Specification
#'
#' Power analysis for one-way analysis of variance comparing means
#' across k independent groups. Uses Cohen's f as the effect size.
#'
#' @keywords internal
create_anova_oneway_spec <- function() {
  list(
    id = "anova_oneway",
    name = "One-Way ANOVA",
    description = "Compare means across multiple independent groups",
    icon = "bar-chart-steps",
    power_function = pwr::pwr.anova.test,
    power_args = list(k = "k"),
    effect_size_methods = c("cohens_f"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 8 (Cohen's f).",
    default_effect_grid = list(cohens_f = c(0.10, 0.25, 0.40)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 120, step = 10,
        description = "Total participants across all groups"
      ),
      n_groups = list(
        type = "slider",
        label = "Number of Groups",
        min = 2, max = 10, default = 3, step = 1,
        description = "Number of independent groups"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_f = list(
        min = 0.01, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Effect Size (Cohen's f)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 120
      k <- input$n_groups %||% 3
      per_arm <- rep(total_n / k, k)
      .canonicalize_sample_sizes(
        per_arm_enrolled = per_arm,
        dropout    = input$dropout %||% 0.1,
        arm_labels = paste("Group", seq_len(k))
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 120) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 2) {
        issues <- c(issues, "At least 2 groups required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' McNemar Test Specification (Paired Proportions)
#'
#' Power analysis for McNemar's test comparing paired binary outcomes.
#' Uses the normal approximation from Connor (1987). The user specifies
#' the two discordant cell probabilities (p10 and p01).
#'
#' @keywords internal
create_mcnemar_spec <- function() {
  list(
    id = "mcnemar",
    name = "McNemar Test",
    description = "Paired proportions (before-after binary outcomes)",
    icon = "arrow-left-right",
    power_function = mcnemar_power,
    effect_size_methods = c("discordant"),
    formula_citation = "Connor RJ (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics, 43(1), 207-211.",
    default_effect_grid = list(discordant = c(0.10, 0.20, 0.30)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of matched pairs"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      discordant = list(
        min = 0.01, max = 0.5, default_min = 0.05, default_max = 0.25,
        label = "P(+Treatment, -Control): p10",
        requires = list(p01 = list(
          type = "numeric",
          label = "P(-Treatment, +Control): p01",
          default = 0.05
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      p01 <- params$p01 %||% 0.05

      sapply(effect_sizes, function(p10) {
        p_disc <- p10 + p01
        p_diff <- p10 - p01
        denom <- p_disc - p_diff^2
        if (denom <= 0 || p_disc <= 0) return(0)
        abs(p_diff) / sqrt(denom)
      })
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 100
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Pairs"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Number of pairs must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Longitudinal Mixed Model Specification
#'
#' Power analysis for a two-group comparison of slopes in a linear
#' mixed model with equally spaced time points and compound symmetry
#' correlation structure. Based on Diggle et al. (2002, p. 29).
#'
#' @keywords internal
create_mixed_model_spec <- function() {
  list(
    id = "mixed_model",
    name = "Mixed Model (Slopes)",
    description = "Longitudinal two-group comparison of rates of change",
    icon = "graph-up",
    power_function = mixed_model_power,
    effect_size_methods = c("slope_diff"),
    formula_citation = "Diggle PJ, Heagerty P, Liang K-Y, Zeger SL (2002). Analysis of Longitudinal Data, 2nd ed., Oxford University Press, p. 29.",
    default_effect_grid = list(slope_diff = c(0.20, 0.50, 1.00)),
    paragraph_template = NULL,
    repro_call         = NULL,

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Total participants across both groups"
      ),
      n_timepoints = list(
        type = "slider",
        label = "Number of Time Points",
        min = 3, max = 20, default = 5, step = 1,
        description = "Measurement occasions per subject"
      ),
      correlation = list(
        type = "slider",
        label = "Within-Subject Correlation",
        min = 0.05, max = 0.95, default = 0.5, step = 0.05,
        description = "Compound symmetry correlation (rho)"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      slope_diff = list(
        min = 0.01, max = 5, default_min = 0.1, default_max = 1.0,
        label = "Slope Difference (delta)",
        requires = list(sigma = list(
          type = "numeric",
          label = "Residual SD (sigma)",
          default = 1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      sigma <- params$sigma %||% 1
      rho <- params$correlation %||% 0.5
      m <- params$n_timepoints %||% 5

      t_vals <- seq(0, 1, length.out = m)
      s_tt <- sum((t_vals - mean(t_vals))^2)

      effect_sizes * sqrt(s_tt / (2 * sigma^2 * (1 - rho)))
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 100
      per_arm <- c(total_n / 2, total_n / 2)
      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_timepoints %||% 5) < 3) {
        issues <- c(issues, "At least 3 time points required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}
