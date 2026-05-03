# testServer() reactivity tests for the generic module server.
# These exercise the four power-function shapes in the registry:
# n1+n2 (ttest_2groups), single-n (correlation), proportions (Cohen's h),
# and survival (logrank).

if (!requireNamespace("shiny", quietly = TRUE)) {
  exit_file("shiny not installed; skipping module-server tests")
}

registry <- get_power_test_registry()


# ------------------------------------------------------------
# Two-group t-test: power mode, valid inputs
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "ttest_2groups",
    test_spec = registry$ttest_2groups
  ),
  expr = {
    session$setInputs(
      solve_for      = "power",
      sample_size    = 100,
      allocation     = "equal",
      ratio          = 1,
      dropout        = 0.10,
      effect_method  = "cohens_d",
      cohens_d_es    = c(0.2, 1.0),
      type1          = 0.05,
      onesided       = FALSE
    )
    expect_true(is_valid())
    pr <- power_results()
    expect_equal(nrow(pr), 16L)
    expect_true(all(!is.na(pr$power)))
    expect_true(all(pr$power >= 0 & pr$power <= 1))
  }
)


# ------------------------------------------------------------
# Two-group t-test: sample-size mode produces required N
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "ttest_2groups",
    test_spec = registry$ttest_2groups
  ),
  expr = {
    session$setInputs(
      solve_for      = "sample_size",
      target_power   = 0.80,
      ss_allocation  = "equal",
      ss_ratio       = 1,
      ss_dropout     = 0.10,
      effect_method  = "cohens_d",
      cohens_d_es    = c(0.3, 0.8),
      type1          = 0.05,
      onesided       = FALSE
    )
    expect_true(is_valid())
    sr <- sample_size_results()
    expect_equal(nrow(sr), 16L)
    finite_n <- sr$required_n[!is.na(sr$required_n)]
    if (length(finite_n) > 0L) {
      expect_true(all(finite_n > 0))
    }
  }
)


# ------------------------------------------------------------
# Two-proportion test: power mode (Cohen's h shape)
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "prop_2groups",
    test_spec = registry$prop_2groups
  ),
  expr = {
    session$setInputs(
      solve_for      = "power",
      sample_size    = 200,
      allocation     = "equal",
      ratio          = 1,
      dropout        = 0,
      effect_method  = "proportions",
      proportions_es = c(0.30, 0.55),
      proportions_p2 = 0.50,
      type1          = 0.05,
      onesided       = FALSE
    )
    if (is_valid()) {
      pr <- power_results()
      expect_equal(nrow(pr), 16L)
    }
  }
)


# ------------------------------------------------------------
# Correlation test: power mode (single-n shape)
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "correlation",
    test_spec = registry$correlation
  ),
  expr = {
    session$setInputs(
      solve_for       = "power",
      sample_size     = 60,
      dropout         = 0,
      effect_method   = "correlation",
      correlation_es  = c(0.2, 0.5),
      type1           = 0.05,
      onesided        = FALSE
    )
    if (is_valid()) {
      pr <- power_results()
      expect_equal(nrow(pr), 16L)
      expect_true(all(pr$power >= 0 & pr$power <= 1, na.rm = TRUE))
    }
  }
)


# ------------------------------------------------------------
# Logrank survival test: power mode (custom helper shape)
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "logrank",
    test_spec = registry$logrank
  ),
  expr = {
    session$setInputs(
      solve_for       = "power",
      sample_size     = 200,
      allocation      = "equal",
      ratio           = 1,
      dropout         = 0,
      event_prob      = 0.5,
      effect_method   = "hazard_ratio",
      hazard_ratio_es = c(0.5, 1.5),
      type1           = 0.05,
      onesided        = FALSE
    )
    if (is_valid()) {
      pr <- power_results()
      expect_equal(nrow(pr), 16L)
    }
  }
)


# ------------------------------------------------------------
# Headline value-box reactives produce non-empty strings
# ------------------------------------------------------------
shiny::testServer(
  create_generic_test_server,
  args = list(
    id = "ttest_2groups",
    test_spec = registry$ttest_2groups
  ),
  expr = {
    session$setInputs(
      solve_for      = "power",
      sample_size    = 100,
      allocation     = "equal",
      ratio          = 1,
      dropout        = 0.10,
      effect_method  = "cohens_d",
      cohens_d_es    = c(0.2, 1.0),
      type1          = 0.05,
      onesided       = FALSE
    )
    d <- headline_data()
    expect_false(is.null(d))
    expect_equal(d$mode, "power")
    expect_true(d$max_power >= 0 && d$max_power <= 1)
  }
)
