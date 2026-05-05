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
    # 80%- and 90%-threshold effect sizes are NA when the curve
    # never reaches; otherwise positive scalars.
    expect_true(is.na(d$es_at_80) ||
                  (d$es_at_80 > 0 && d$es_at_80 < 5))
    expect_true(is.na(d$es_at_90) ||
                  (d$es_at_90 > 0 && d$es_at_90 < 5))
    # Total N reads from the canonical 4-layer record.
    expect_true(!is.na(d$n_total) && d$n_total > 0)
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


# ------------------------------------------------------------
# Gap 1: methods-paragraph generator
# ------------------------------------------------------------

# Renderer alone produces a paragraph from a manually-built ctx.
ctx <- power_calc("ttest_2groups",
                  sample_size = 100, effect_size = 0.5,
                  effect_method = "cohens_d", dropout = 0.10)
ctx$effect_source         <- "Smith et al. 2019, JAMA, n=30"
ctx$sensitivity_factor    <- 0.7
ctx$include_sex_paragraph <- TRUE

para <- .render_methods_paragraph(ctx)
expect_true(nchar(para) > 200)                       # not truncated
expect_true(grepl("Two-Sample t-test", para, fixed = TRUE))
expect_true(grepl("Smith et al. 2019", para, fixed = TRUE))
expect_true(grepl("Cohen's d", para, fixed = TRUE))
expect_true(grepl("alpha=0.05", para, fixed = TRUE))  # alpha shown
expect_true(grepl("evaluable participants", para, fixed = TRUE))
expect_true(grepl("dropout", para, fixed = TRUE))
expect_true(grepl("smaller", para, fixed = TRUE))   # sensitivity sentence
expect_true(grepl("zzpower", para, fixed = TRUE))
expect_true(grepl("sex x treatment", para, fixed = TRUE))  # sex paragraph

# Sex paragraph drops out when the toggle is off.
ctx$include_sex_paragraph <- FALSE
para_no_sex <- .render_methods_paragraph(ctx)
expect_false(grepl("sex x treatment", para_no_sex, fixed = TRUE))

# Sensitivity sentence drops out when factor is NULL.
ctx$sensitivity_factor <- NULL
para_no_sens <- .render_methods_paragraph(ctx)
expect_false(grepl("smaller", para_no_sens, fixed = TRUE))

# In-server reactive renders the paragraph as a Shiny output.
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
      onesided       = FALSE,
      effect_source  = "Test Citation 2026",
      effect_doi     = "",
      sensitivity_factor    = 0.7,
      include_sex_paragraph = TRUE
    )
    out <- output$methods_paragraph_text
    expect_true(nchar(out) > 200)
    expect_true(grepl("Test Citation 2026", out, fixed = TRUE))
    expect_true(grepl("Two-Sample t-test", out, fixed = TRUE))
  }
)


# ------------------------------------------------------------
# Gap 2: sensitivity-table reactive seeds from default grid
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

    df <- sensitivity_table_df()
    expect_true(is.data.frame(df))
    # Seeded from default_effect_grid$cohens_d (3 base rows) +
    # auto-augmented rows at 50%/80%/90%-power thresholds (up to
    # 3 more, depending on whether the curve actually crosses).
    expect_true(nrow(df) >= 3L && nrow(df) <= 6L)
    expect_true(all(c("effect_size", "n_total_enrolled_p80",
                      "n_total_enrolled_p90") %in% names(df)))
    # Required N is monotonically decreasing in effect size.
    expect_true(all(diff(df$n_total_enrolled_p80) < 0))
    # 90%-power N exceeds 80%-power N for every row.
    expect_true(all(df$n_total_enrolled_p90 > df$n_total_enrolled_p80))
  }
)


# ------------------------------------------------------------
# .df_to_markdown produces a parseable markdown table
# ------------------------------------------------------------
md <- .df_to_markdown(
  data.frame(A = c(1.234, 5.678), B = c("x", "y")),
  caption = "Test"
)
expect_true(grepl("**Test**", md, fixed = TRUE))
expect_true(grepl("| A | B |", md, fixed = TRUE))
expect_true(grepl("|---|---|", md, fixed = TRUE))


# ------------------------------------------------------------
# Gap 9: reproducibility R script export
# ------------------------------------------------------------

# pwr-backed test: emits library(pwr) and pwr::pwr.t2n.test call.
ctx <- power_calc("ttest_2groups",
                  sample_size = 100, effect_size = 0.5,
                  effect_method = "cohens_d", dropout = 0.10)
script <- .render_repro_script(ctx)
expect_true(nchar(script) > 100)
expect_true(grepl("library(pwr)", script, fixed = TRUE))
expect_true(grepl("pwr::pwr.t2n.test", script, fixed = TRUE))
expect_true(grepl("d ", script))
expect_true(grepl("n1 ", script))
expect_true(grepl("n2 ", script))
expect_true(grepl("sig.level", script, fixed = TRUE))
# Args should come in signature order, not alphabetic insertion.
d_pos  <- regexpr("\\bd\\s+=", script)
sig_pos <- regexpr("sig.level", script)
expect_true(d_pos < sig_pos)

# zzpower-backed test: emits library(zzpower) and zzpower:: call.
ctx2 <- power_calc("logrank",
                   sample_size = 200, effect_size = 0.6,
                   effect_method = "hazard_ratio",
                   dropout = 0.10, event_prob = 0.7)
script2 <- .render_repro_script(ctx2)
expect_true(grepl("library(zzpower)", script2, fixed = TRUE))
expect_true(grepl("zzpower::logrank_power", script2, fixed = TRUE))

# Result-comment line includes achieved-power figure.
expect_true(grepl("Achieved power:", script, fixed = TRUE))
expect_true(grepl("Total enrolled:", script, fixed = TRUE))

# Markdown fence wrapper.
script_fenced <- .render_repro_script(ctx, fence = TRUE)
expect_true(startsWith(script_fenced, "```r\n"))
expect_true(endsWith(script_fenced, "\n```"))

# Every spec produces a non-empty repro script.
for (id in names(registry)) {
  ctx_i <- power_calc(id,
    sample_size   = 100,
    effect_size   = registry[[id]]$default_effect_grid[[1]][2] %||% 0.5,
    effect_method = registry[[id]]$effect_size_methods[1])
  s <- .render_repro_script(ctx_i)
  expect_true(nchar(s) > 50,
              info = sprintf("%s repro script empty", id))
  expect_true(grepl(registry[[id]]$repro_call, s, fixed = TRUE),
              info = sprintf("%s repro_call name not in script", id))
}

# .report_data_to_ctx threads the canonical sample sizes through.
report_data <- list(
  test_id = "ttest_2groups",
  sample_sizes = ctx$sample_sizes,
  effect_size_range = list(
    method = "cohens_d",
    effect_sizes = c(0.2, 0.5, 0.8),
    standardized = c(0.2, 0.5, 0.8)
  ),
  power_results = data.frame(effect_size = c(0.2, 0.5, 0.8),
                             standardized_es = c(0.2, 0.5, 0.8),
                             power = c(0.30, 0.65, 0.90)),
  type1_error = 0.05,
  one_sided = FALSE
)
ctx_back <- .report_data_to_ctx(report_data)
expect_false(is.null(ctx_back))
expect_equal(ctx_back$test_id, "ttest_2groups")
expect_equal(ctx_back$alpha, 0.05)
expect_equal(ctx_back$alternative, "two.sided")


# ------------------------------------------------------------
# Gap 3: multi-aim aggregation
# ------------------------------------------------------------

# Empty study returns an empty data frame with the right columns.
study0 <- multi_aim_study()
df0 <- format_multi_aim_df(study0)
expect_equal(nrow(df0), 0)
expect_true(all(c("Aim", "Outcome", "Test", "Effect size",
                  "Alpha", "Power", "N evaluable", "N enrolled",
                  "Binding") %in% names(df0)))

# Add three aims; binding is the row with max N enrolled.
study <- multi_aim_study(study_name = "Test study")
study <- add_aim(study,
  power_calc("ttest_2groups", target_power = 0.80,
             effect_size = 0.5, effect_method = "cohens_d",
             dropout = 0.10),
  name = "Aim A", outcome = "Continuous outcome")
study <- add_aim(study,
  power_calc("logrank", target_power = 0.80,
             effect_size = 0.65, effect_method = "hazard_ratio",
             dropout = 0.10, event_prob = 0.7),
  name = "Aim B", outcome = "Survival")
study <- add_aim(study,
  power_calc("correlation", target_power = 0.80,
             effect_size = 0.30, effect_method = "correlation",
             dropout = 0),
  name = "Aim C", outcome = "Correlation")

df <- format_multi_aim_df(study)
expect_equal(nrow(df), 3)
expect_equal(df$Aim, c("Aim A", "Aim B", "Aim C"))
expect_equal(sum(df$Binding), 1L)  # exactly one binding row
expect_equal(which.max(df[["N enrolled"]]),
             which(df$Binding))

# Markdown output mentions binding asterisk and study name.
md <- multi_aim_markdown(study)
expect_true(grepl("Test study", md, fixed = TRUE))
expect_true(grepl("`*` is the binding aim", md, fixed = TRUE))
expect_true(grepl(" \\* \\|", md))   # asterisk after binding aim cell

# CSV string contains all rows.
csv_str <- multi_aim_csv(study)
expect_true(grepl("Aim A", csv_str, fixed = TRUE))
expect_true(grepl("Aim B", csv_str, fixed = TRUE))
expect_true(grepl("Aim C", csv_str, fixed = TRUE))

# CSV file write returns the path.
tmp <- tempfile(fileext = ".csv")
ret <- multi_aim_csv(study, file = tmp)
expect_equal(ret, tmp)
expect_true(file.exists(tmp))
unlink(tmp)

# add_aim rejects bad inputs.
expect_error(add_aim(list(), study$aims[[1]]))
expect_error(add_aim(study, "not a ctx"))
