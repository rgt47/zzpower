#' Power Test Registry
#'
#' Centralized registry defining all available power analysis tests.
#' Each entry defines the parameters, effect size methods, and power calculation
#' for a specific statistical test.
#'
#' @keywords internal

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
        n1 <- total_n / 2
        n2 <- total_n / 2
      } else {
        ratio <- input$ratio %||% 1
        n1 <- ratio * total_n / (ratio + 1)
        n2 <- total_n / (ratio + 1)
      }

      n1_comp <- n1 * (1 - dropout)
      n2_comp <- n2 * (1 - dropout)

      list(
        n1 = n1_comp,
        n2 = n2_comp,
        n1_itt = n1,
        n2_itt = n2
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

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of paired observations"
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
      list(n = n)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
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

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of observations"
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
      list(n = n)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
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
        n1 <- total_n / 2
        n2 <- total_n / 2
      } else {
        ratio <- input$ratio %||% 1
        n1 <- ratio * total_n / (ratio + 1)
        n2 <- total_n / (ratio + 1)
      }

      list(n1 = n1, n2 = n2)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
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

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of observations"
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
      list(n = n)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
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

      if (allocation == "equal") {
        n1 <- total_n / 2
        n2 <- total_n / 2
      } else {
        ratio <- input$ratio %||% 1
        n1 <- ratio * total_n / (ratio + 1)
        n2 <- total_n / (ratio + 1)
      }

      list(
        n1 = n1 * event_prob,
        n2 = n2 * event_prob
      )
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
        n1 <- total_n / 2
        n2 <- total_n / 2
      } else {
        ratio <- input$ratio %||% 1
        n1 <- ratio * total_n / (ratio + 1)
        n2 <- total_n / (ratio + 1)
      }

      list(n1 = n1, n2 = n2)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 60) <= 0) {
        issues <- c(issues, "Sample size must be positive")
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
      list(n = n)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 150) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 3) {
        issues <- c(issues, "At least 3 dose groups required")
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
      list(n = total_n / k, k = k)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 120) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 2) {
        issues <- c(issues, "At least 2 groups required")
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

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of matched pairs"
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
      list(n = n)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Number of pairs must be positive")
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
      list(n = total_n / 2)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_timepoints %||% 5) < 3) {
        issues <- c(issues, "At least 3 time points required")
      }
      issues
    }
  )
}
