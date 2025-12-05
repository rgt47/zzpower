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
    correlation = create_correlation_spec()
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
    icon = "chart-line",
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
    icon = "arrows-repeat",
    power_function = pwr::pwr.t.test,  # Uses t.test with paired=TRUE
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
      effect_sizes  # Already in Cohen's d
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
    icon = "target",
    power_function = pwr::pwr.t.test,
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
      effect_sizes  # Already standardized
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
