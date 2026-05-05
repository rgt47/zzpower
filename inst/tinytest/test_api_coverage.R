# API coverage + parameter-sweep tests for zzpower.
#
# Complements `test_module_server.R` (Shiny reactivity) by
# exercising the public programmatic API (`power_calc`,
# `power_table`, `multi_aim_study`/`add_aim`/`format_multi_aim_df`)
# across all 14 registry entries and the recent feature flags
# (non-inferiority, Bonferroni-aware ANOVA, cluster-RCT design
# effect). The forward (power-given-N) and inverse (N-given-power)
# directions are checked for every spec; mathematical relationships
# (monotonicity in N, design-effect inflation, alpha adjustment)
# are verified to catch regressions in the calculation engine.

library(tinytest)

if (!requireNamespace("shiny", quietly = TRUE)) {
  exit_file("shiny not installed; skipping API coverage tests")
}

registry <- get_power_test_registry()

# Method-specific extras each spec needs in addition to the
# top-level args. Centralised so the loops below stay readable.
spec_extras <- list(
  ttest_2groups    = list(dropout = 0.10, allocation = "equal"),
  ttest_paired     = list(dropout = 0.10),
  ttest_one_sample = list(dropout = 0.10),
  prop_2groups     = list(dropout = 0.10, allocation = "equal",
                          baseline = 0.30, p2 = 0.30),
  correlation      = list(dropout = 0.10),
  logrank          = list(dropout = 0.10, allocation = "equal",
                          event_prob = 0.7),
  fisher_exact     = list(dropout = 0.10, allocation = "equal",
                          baseline = 0.30, p2 = 0.30),
  trend_prop       = list(dropout = 0.10, p_low = 0.10,
                          n_groups = 3),
  anova_oneway     = list(dropout = 0.10, n_groups = 3,
                          n_pairwise_contrasts = 1),
  mcnemar          = list(dropout = 0.10, p01 = 0.05),
  mixed_model      = list(dropout = 0.10, sigma = 1,
                          n_timepoints = 5, correlation = 0.5),
  cluster_rct      = list(dropout = 0.10, m_cluster = 30,
                          icc = 0.05),
  cluster_prop     = list(dropout = 0.10, m_cluster = 30,
                          icc = 0.05, baseline = 0.30),
  cluster_logrank  = list(dropout = 0.10, m_cluster = 30,
                          icc = 0.05, event_prob = 0.7)
)

# Helper: pick a representative effect from each spec's default
# grid (median element if multiple, else the only value).
.rep_effect <- function(spec, method) {
  grid <- spec$default_effect_grid[[method]]
  if (is.null(grid)) return(0.5)
  grid[ceiling(length(grid) / 2)]
}


# ============================================================
# Section 1: power_calc() forward direction for all 14 specs
# ============================================================
for (id in names(registry)) {
  spec <- registry[[id]]
  m <- spec$effect_size_methods[1]
  eff <- .rep_effect(spec, m)
  extra <- spec_extras[[id]] %||% list()

  ctx <- do.call(power_calc, c(
    list(test = id, sample_size = 200, effect_size = eff,
         effect_method = m),
    extra
  ))

  expect_true(is.list(ctx),
              info = sprintf("%s: power_calc did not return a list",
                              id))
  expect_equal(ctx$test_id, id,
               info = sprintf("%s: test_id mismatch", id))
  expect_true(!is.na(ctx$achieved_power),
              info = sprintf("%s: achieved_power is NA", id))
  expect_true(ctx$achieved_power >= 0 && ctx$achieved_power <= 1,
              info = sprintf("%s: achieved_power out of [0,1]: %s",
                              id, ctx$achieved_power))
  expect_true(ctx$sample_sizes$n_total_evaluable > 0,
              info = sprintf("%s: n_total_evaluable not positive",
                              id))
}


# ============================================================
# Section 2: power_calc() inverse direction (target_power)
# ============================================================
for (id in names(registry)) {
  spec <- registry[[id]]
  m <- spec$effect_size_methods[1]
  eff <- .rep_effect(spec, m)
  extra <- spec_extras[[id]] %||% list()

  ctx <- do.call(power_calc, c(
    list(test = id, target_power = 0.80, effect_size = eff,
         effect_method = m),
    extra
  ))

  N_enrolled <- ctx$sample_sizes$n_total_enrolled
  expect_true(!is.na(N_enrolled),
              info = sprintf("%s: required N is NA", id))
  expect_true(N_enrolled > 0,
              info = sprintf("%s: required N not positive (%s)",
                              id, N_enrolled))
  # The achieved-power at the bisection solution should hit at
  # least the target -- bisection tolerance ~1e-3 in the engine.
  expect_true(ctx$achieved_power >= 0.795,
              info = sprintf("%s: achieved %.3f below 0.80 target",
                              id, ctx$achieved_power))
}


# ============================================================
# Section 3: power_table() default-grid for all 14 specs
# ============================================================
for (id in names(registry)) {
  spec <- registry[[id]]
  m <- spec$effect_size_methods[1]
  extra <- spec_extras[[id]] %||% list()

  tbl <- do.call(power_table, c(
    list(test = id, effect_method = m),
    extra
  ))

  expect_true(is.data.frame(tbl),
              info = sprintf("%s: power_table not a data frame",
                              id))
  expect_true(nrow(tbl) >= 2L,
              info = sprintf("%s: power_table has %d rows",
                              id, nrow(tbl)))
  expect_true(all(c("effect_size", "effect_size_std",
                    "n_total_enrolled_p80",
                    "n_total_enrolled_p90") %in% names(tbl)),
              info = sprintf("%s: missing power_table columns", id))
}


# ============================================================
# Section 4: power monotone in N (sweep sample sizes)
# Detects regressions in the call-assembly logic that would
# otherwise return non-monotonic power as N grows.
# ============================================================
for (id in names(registry)) {
  spec <- registry[[id]]
  m <- spec$effect_size_methods[1]
  eff <- .rep_effect(spec, m)
  extra <- spec_extras[[id]] %||% list()

  # logrank's input "sample_size" is participants; via event_prob
  # the sweep behaves consistently. For cluster designs we stay
  # well inside the slider's positive range to avoid edge cases.
  N_grid <- c(60, 200, 800)
  powers <- vapply(N_grid, function(N) {
    ctx <- do.call(power_calc, c(
      list(test = id, sample_size = N, effect_size = eff,
           effect_method = m),
      extra
    ))
    ctx$achieved_power
  }, numeric(1))

  # 1e-3 tolerance handles small bisection / numeric drift.
  expect_true(all(diff(powers) >= -1e-3),
              info = sprintf(
                "%s: power not monotone in N: %s",
                id, paste(sprintf("%.3f", powers), collapse = ", ")
              ))
  # As N grows we should approach 1 (or at least be non-trivial).
  expect_true(powers[length(powers)] > powers[1],
              info = sprintf("%s: power did not grow with N", id))
}


# ============================================================
# Section 5: required N monotone DECREASING in target power up
# to a knee, then increasing. (Trivial check: 0.90 needs at least
# as much N as 0.80 for the same effect.)
# ============================================================
for (id in names(registry)) {
  spec <- registry[[id]]
  m <- spec$effect_size_methods[1]
  eff <- .rep_effect(spec, m)
  extra <- spec_extras[[id]] %||% list()

  N80 <- do.call(power_calc, c(
    list(test = id, target_power = 0.80, effect_size = eff,
         effect_method = m),
    extra
  ))$sample_sizes$n_total_enrolled

  N90 <- do.call(power_calc, c(
    list(test = id, target_power = 0.90, effect_size = eff,
         effect_method = m),
    extra
  ))$sample_sizes$n_total_enrolled

  # 90% power needs more N than 80% power for the same effect.
  expect_true(N90 >= N80,
              info = sprintf(
                "%s: N(0.90) = %.0f < N(0.80) = %.0f",
                id, N90, N80
              ))
}


# ============================================================
# Section 6: non-inferiority for ttest_2groups
# Margin of 0 should equal superiority with effect_size 0;
# positive margin shifts the standardised effect by margin / sd.
# ============================================================
ctx_sup <- power_calc(
  "ttest_2groups",
  sample_size = 200, effect_size = 0.5,
  effect_method = "cohens_d", dropout = 0,
  hypothesis_type = "superiority"
)
ctx_ni0 <- power_calc(
  "ttest_2groups",
  sample_size = 200, effect_size = 0,
  effect_method = "cohens_d", dropout = 0,
  hypothesis_type = "non_inferiority", ni_margin = 0
)
ctx_ni  <- power_calc(
  "ttest_2groups",
  sample_size = 200, effect_size = 0,
  effect_method = "cohens_d", dropout = 0,
  alpha = 0.025, alternative = "one.sided",
  hypothesis_type = "non_inferiority", ni_margin = 0.5
)
expect_equal(ctx_ni0$effect_size_std, 0,
             info = "NI margin 0 with effect 0 => std = 0")
expect_equal(ctx_ni$effect_size_std, 0.5,
             info = "NI margin 0.5 shifts std to 0.5")
# One-sided alpha=0.025 has the same critical z as two-sided
# alpha=0.05, so power for ctx_ni should match the standard
# two-sided d=0.5 result within a small tolerance.
expect_true(abs(ctx_ni$achieved_power -
                  ctx_sup$achieved_power) < 0.01,
            info = "NI shift produces equivalent power to superiority")


# ============================================================
# Section 7: non-inferiority for prop_2groups
# Margin shifts the proportion difference; Cohen's h is computed
# from baseline + (effect + margin) vs baseline. The exact
# relationship: standardize(effect = 0, margin = m) for NI must
# equal standardize(effect = m, margin = 0) for superiority,
# because the standardize() function adds them before applying
# diff_to_cohens_h.
# ============================================================
ctx_pni  <- power_calc(
  "prop_2groups",
  sample_size = 400, effect_size = 0,
  effect_method = "difference",
  baseline = 0.30, dropout = 0,
  alpha = 0.025, alternative = "one.sided",
  hypothesis_type = "non_inferiority", ni_margin = 0.10
)
ctx_pmatch <- power_calc(
  "prop_2groups",
  sample_size = 400, effect_size = 0.10,
  effect_method = "difference",
  baseline = 0.30, dropout = 0,
  hypothesis_type = "superiority"
)
expect_equal(ctx_pni$effect_size_std,
             ctx_pmatch$effect_size_std,
             info = "NI prop (effect=0, margin=m) == sup (effect=m)")
# Sanity: NI with non-zero margin should produce non-zero
# standardised effect.
expect_true(abs(ctx_pni$effect_size_std) > 0.01,
            info = "NI prop standardised effect should be non-zero")


# ============================================================
# Section 8: Bonferroni-aware ANOVA
# alpha_effective = alpha / n_pairwise_contrasts; achieved power
# strictly decreases as the contrast count grows.
# ============================================================
ctx1 <- power_calc("anova_oneway",
                   sample_size = 200, effect_size = 0.25,
                   effect_method = "cohens_f", n_groups = 3,
                   n_pairwise_contrasts = 1, dropout = 0)
ctx3 <- power_calc("anova_oneway",
                   sample_size = 200, effect_size = 0.25,
                   effect_method = "cohens_f", n_groups = 3,
                   n_pairwise_contrasts = 3, dropout = 0)
ctx6 <- power_calc("anova_oneway",
                   sample_size = 200, effect_size = 0.25,
                   effect_method = "cohens_f", n_groups = 3,
                   n_pairwise_contrasts = 6, dropout = 0)
expect_equal(ctx1$alpha_effective, 0.05)
expect_equal(ctx3$alpha_effective, 0.05 / 3)
expect_equal(ctx6$alpha_effective, 0.05 / 6)
expect_true(ctx1$achieved_power > ctx3$achieved_power,
            info = "Bonferroni: power(1 contrast) > power(3 contrasts)")
expect_true(ctx3$achieved_power > ctx6$achieved_power,
            info = "Bonferroni: power(3 contrasts) > power(6 contrasts)")


# ============================================================
# Section 9: Cluster-RCT design effect
# DE = 1 + (m_bar - 1) * ICC.
# Power at fixed N decreases as ICC grows (variance inflated).
# ============================================================
de_check <- function(m_bar, icc) 1 + (m_bar - 1) * icc

for (icc in c(0.00, 0.05, 0.10, 0.20)) {
  ctx <- power_calc("cluster_rct",
                    sample_size = 400, effect_size = 0.5,
                    effect_method = "cohens_d",
                    m_cluster = 30, icc = icc, dropout = 0)
  expect_equal(ctx$sample_sizes$design_effect,
               de_check(30, icc),
               info = sprintf("cluster_rct DE wrong for ICC=%.2f",
                               icc))
}

# Power monotonically non-increasing as ICC grows
ctx_iccs <- vapply(c(0.00, 0.05, 0.10, 0.20), function(icc) {
  power_calc("cluster_rct",
             sample_size = 400, effect_size = 0.5,
             effect_method = "cohens_d",
             m_cluster = 30, icc = icc,
             dropout = 0)$achieved_power
}, numeric(1))
expect_true(all(diff(ctx_iccs) <= 1e-3),
            info = "cluster_rct power should not grow with ICC")


# ============================================================
# Section 10: multi_aim_study end-to-end with mixed test types
# ============================================================
study <- multi_aim_study(study_name = "Coverage check")
study <- add_aim(study,
  power_calc("ttest_2groups", target_power = 0.80,
             effect_size = 0.5, effect_method = "cohens_d",
             dropout = 0.10),
  name = "Aim 1", outcome = "Continuous outcome")
study <- add_aim(study,
  power_calc("logrank", target_power = 0.80,
             effect_size = 0.65, effect_method = "hazard_ratio",
             dropout = 0.10, event_prob = 0.7),
  name = "Aim 2", outcome = "Survival")
study <- add_aim(study,
  power_calc("cluster_prop", target_power = 0.80,
             effect_size = -0.10, effect_method = "difference",
             baseline = 0.30, dropout = 0.10,
             m_cluster = 30, icc = 0.05),
  name = "Aim 3 (cluster)", outcome = "Proportion (cluster RCT)")

df <- format_multi_aim_df(study)
expect_equal(nrow(df), 3L)
expect_equal(sum(df$Binding), 1L)
expect_true(all(df$`N enrolled` > 0))

md <- multi_aim_markdown(study)
expect_true(nchar(md) > 200)
expect_true(grepl("Coverage check", md, fixed = TRUE))
expect_true(grepl("binding aim", md, fixed = TRUE))


# ============================================================
# Section 11: power_calc validates effect_method
# ============================================================
expect_error(
  power_calc("ttest_2groups", sample_size = 100, effect_size = 0.5,
             effect_method = "not_a_real_method")
)


# ============================================================
# Section 12: power_table accepts custom power_thresholds
# ============================================================
tbl <- power_table("ttest_2groups",
                   effect_grid = c(0.2, 0.5, 0.8),
                   power_thresholds = c(0.70, 0.80, 0.90),
                   effect_method = "cohens_d", dropout = 0)
expect_true(all(c("n_total_enrolled_p70",
                  "n_total_enrolled_p80",
                  "n_total_enrolled_p90") %in% names(tbl)))
expect_equal(nrow(tbl), 3L)
# N(0.90) >= N(0.80) >= N(0.70) for every effect-size row
expect_true(all(tbl$n_total_enrolled_p80 >=
                  tbl$n_total_enrolled_p70))
expect_true(all(tbl$n_total_enrolled_p90 >=
                  tbl$n_total_enrolled_p80))
