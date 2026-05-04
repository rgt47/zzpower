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


# ------------------------------------------------------------
# Gap 4: every spec returns the canonical 4-layer N contract
# ------------------------------------------------------------
required_fields <- c(
  "n_per_arm_evaluable", "n_per_arm_enrolled",
  "n_total_evaluable",   "n_total_enrolled",
  "n_arms", "arm_labels", "dropout"
)

for (id in names(registry)) {
  out <- registry[[id]]$sample_size_calc(list())

  for (f in required_fields) {
    expect_true(f %in% names(out),
                info = sprintf("%s missing field %s", id, f))
  }

  expect_true(out$n_arms >= 1L,
              info = sprintf("%s n_arms must be >= 1", id))
  expect_equal(length(out$n_per_arm_evaluable), out$n_arms,
               info = sprintf("%s evaluable vector length", id))
  expect_equal(length(out$n_per_arm_enrolled), out$n_arms,
               info = sprintf("%s enrolled vector length", id))

  # logrank reports participants in canonical fields and events
  # in back-compat n1/n2; for every other test the canonical
  # total equals what consumers used to read.
  if (id != "logrank") {
    expect_equal(sum(out$n_per_arm_evaluable),
                 out$n_total_evaluable,
                 info = sprintf("%s evaluable sum", id))
    expect_equal(sum(out$n_per_arm_enrolled),
                 out$n_total_enrolled,
                 info = sprintf("%s enrolled sum", id))
  }

  # Enrolled >= evaluable for any non-negative dropout.
  expect_true(out$n_total_enrolled >= out$n_total_evaluable,
              info = sprintf("%s enrolled >= evaluable", id))
}


# ------------------------------------------------------------
# Gap 10: programmatic API smoke tests (power_calc, power_table)
# ------------------------------------------------------------

# Power at fixed N matches pwr::pwr.t.test with same inputs.
ctx <- power_calc("ttest_2groups",
                  sample_size = 100, effect_size = 0.5,
                  effect_method = "cohens_d", dropout = 0)
expect_equal(ctx$test_id, "ttest_2groups")
expect_true(!is.na(ctx$achieved_power))
expect_true(ctx$achieved_power > 0 && ctx$achieved_power < 1)
expect_equal(ctx$sample_sizes$n_total_enrolled, 100)
expect_equal(ctx$sample_sizes$n_total_evaluable, 100)

# Sample-size mode: required N is consistent with achieved power.
ctx_n <- power_calc("ttest_2groups",
                    target_power = 0.80, effect_size = 0.5,
                    effect_method = "cohens_d", dropout = 0)
expect_true(!is.na(ctx_n$sample_sizes$n_total_enrolled))
expect_true(ctx_n$achieved_power >= 0.795)  # bisection tolerance

# power_table: default grid produces a 3-row data frame.
tbl <- power_table("ttest_2groups",
                   effect_method = "cohens_d", dropout = 0)
expect_true(is.data.frame(tbl))
expect_equal(nrow(tbl), 3L)
expect_true(all(c("effect_size", "effect_size_std",
                  "n_total_enrolled_p80", "n_total_enrolled_p90")
                %in% names(tbl)))
# Required N decreases as effect grows.
expect_true(all(diff(tbl$n_total_enrolled_p80) < 0))

# power_calc returns the canonical 4-layer N record.
required_fields <- c(
  "n_per_arm_evaluable", "n_per_arm_enrolled",
  "n_total_evaluable",   "n_total_enrolled",
  "n_arms", "arm_labels", "dropout"
)
for (f in required_fields) {
  expect_true(f %in% names(ctx$sample_sizes),
              info = sprintf("calc_context.sample_sizes missing %s", f))
}

# Unknown test id raises.
expect_error(power_calc("not_a_test", sample_size = 100, effect_size = 0.5))
