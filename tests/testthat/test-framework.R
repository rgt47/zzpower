test_that("power test registry exists and contains 8 tests", {
  registry <- get_power_test_registry()

  expect_true(is.list(registry))
  expect_equal(length(registry), 8)

  expected_tests <- c(
    "ttest_2groups",
    "ttest_paired",
    "ttest_one_sample",
    "prop_2groups",
    "correlation",
    "logrank",
    "fisher_exact",
    "trend_prop"
  )

  expect_equal(sort(names(registry)), sort(expected_tests))
})

test_that("each test specification has required fields", {
  registry <- get_power_test_registry()

  required_fields <- c(
    "id", "name", "description", "icon",
    "power_function", "effect_size_methods",
    "parameters", "effect_size_params",
    "standardize", "sample_size_calc", "validation"
  )

  for (test_id in names(registry)) {
    spec <- registry[[test_id]]

    # Check all required fields exist
    for (field in required_fields) {
      expect_true(
        field %in% names(spec),
        info = paste("Test", test_id, "missing field:", field)
      )
    }

    # Check functions are callable
    expect_true(is.function(spec$standardize))
    expect_true(is.function(spec$sample_size_calc))
    expect_true(is.function(spec$validation))
  }
})

test_that("ttest_2groups specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups

  expect_equal(spec$id, "ttest_2groups")
  expect_equal(spec$name, "Two-Group t-test")
  expect_true("cohens_d" %in% spec$effect_size_methods)
  expect_true("sample_size" %in% names(spec$parameters))
  expect_true("dropout" %in% names(spec$parameters))

  # Test validation
  validation <- spec$validation(list(sample_size = 100, dropout = 0.1))
  expect_true(is.character(validation))
})

test_that("ttest_paired specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_paired

  expect_equal(spec$id, "ttest_paired")
  expect_equal(spec$name, "Paired t-test")
  expect_true("cohens_d" %in% spec$effect_size_methods)
})

test_that("ttest_one_sample specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_one_sample

  expect_equal(spec$id, "ttest_one_sample")
  expect_equal(spec$name, "One-Sample t-test")
  expect_true("cohens_d" %in% spec$effect_size_methods)
})

test_that("prop_2groups specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$prop_2groups

  expect_equal(spec$id, "prop_2groups")
  expect_equal(spec$name, "Two Proportions")
  expect_true("proportions" %in% spec$effect_size_methods)
  expect_true("difference" %in% spec$effect_size_methods)
  expect_true("odds_ratio" %in% spec$effect_size_methods)
  expect_true("relative_risk" %in% spec$effect_size_methods)
})

test_that("correlation specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$correlation

  expect_equal(spec$id, "correlation")
  expect_equal(spec$name, "Correlation Test")
  expect_true("correlation" %in% spec$effect_size_methods)
})

test_that("standardize function converts effect sizes correctly for ttest", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups

  # Cohen's d should pass through unchanged
  effect_sizes <- c(0.2, 0.5, 0.8)
  result <- spec$standardize(effect_sizes, "cohens_d", list())
  expect_equal(result, effect_sizes)
})

test_that("sample_size_calc produces valid results", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups

  params <- spec$sample_size_calc(list(
    sample_size = 100,
    allocation = "equal",
    dropout = 0.1
  ))

  expect_true(!is.null(params$n1))
  expect_true(!is.null(params$n2))
  expect_true(params$n1 > 0)
  expect_true(params$n2 > 0)
})

test_that("generic UI builder creates valid UI for all tests", {
  registry <- get_power_test_registry()

  for (test_id in names(registry)) {
    ui <- create_generic_test_ui(test_id)

    # Check that UI is a Shiny tag object (page_sidebar returns a tag)
    # Allow for either shiny.tag or shiny.tag.list
    expect_true(
      inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui),
      info = paste("Test", test_id, "did not produce valid UI")
    )
  }
})

test_that("render_sample_size_inputs generates correct controls", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups
  test_id <- "ttest_2groups"

  # Create mock input object with all expected fields
  input <- list(
    ttest_2groups_allocation = "equal",
    ttest_2groups_sample_size = 100,
    ttest_2groups_dropout = 0.1,
    ttest_2groups_ratio = 1
  )

  controls <- render_sample_size_inputs(test_id, input)

  # Should return a tag list or single tag
  expect_true(
    inherits(controls, "shiny.tag.list") ||
    inherits(controls, "shiny.tag") ||
    is.null(controls)
  )
})

test_that("render_effect_size_inputs generates correct controls", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups
  test_id <- "ttest_2groups"

  input <- list(
    ttest_2groups_effect_method = "cohens_d",
    ttest_2groups_cohens_d_es = c(0.2, 0.8),
    ttest_2groups_sd0 = 10
  )

  controls <- render_effect_size_inputs(test_id, input)

  expect_true(
    inherits(controls, "shiny.tag.list") ||
    inherits(controls, "shiny.tag") ||
    is.null(controls)
  )
})

test_that("render_advanced_settings generates control panel", {
  test_id <- "ttest_2groups"

  settings <- render_advanced_settings(test_id)

  expect_true(
    inherits(settings, "shiny.tag.list") ||
    inherits(settings, "shiny.tag") ||
    is.null(settings)
  )
})

test_that("get_effect_size_range generates correct sequences", {
  registry <- get_power_test_registry()
  test_id <- "ttest_2groups"
  spec <- registry[[test_id]]

  input <- list(
    ttest_2groups_effect_method = "cohens_d",
    ttest_2groups_cohens_d_es = c(0.2, 0.8)
  )

  result <- get_effect_size_range(test_id, input)

  expect_true(is.list(result))
  expect_true("effect_sizes" %in% names(result))
  expect_true("standardized" %in% names(result))
  expect_true("method" %in% names(result))

  expect_equal(length(result$effect_sizes), ZZPOWER_CONSTANTS$EFFECT_SIZE_SEQ_LENGTH)
  expect_equal(length(result$standardized), ZZPOWER_CONSTANTS$EFFECT_SIZE_SEQ_LENGTH)
})

test_that("generic server factory has correct signature", {
  expect_true(is.function(create_generic_test_server))
  args <- names(formals(create_generic_test_server))
  expect_true("id" %in% args)
  expect_true("test_spec" %in% args)
  expect_true("input" %in% args)
  expect_true("output" %in% args)
  expect_true("session" %in% args)
})

test_that("launch_zzpower function exists and is exported", {
  expect_true(exists("launch_zzpower"))
  expect_true(is.function(launch_zzpower))
})

test_that("generic report formatter produces valid output", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups

  report_data <- list(
    test_id = "ttest_2groups",
    test_name = "Two-Group t-test",
    format = "text",
    timestamp = Sys.time(),
    r_version = "4.0.0",
    parameters = list(),
    sample_sizes = list(n1 = 100, n2 = 100),
    effect_size_range = list(
      effect_sizes = seq(0.2, 0.8, length.out = 16),
      standardized = seq(0.2, 0.8, length.out = 16),
      method = "cohens_d"
    ),
    power_results = data.frame(
      effect_size = seq(0.2, 0.8, length.out = 16),
      standardized_es = seq(0.2, 0.8, length.out = 16),
      power = seq(0.1, 0.95, length.out = 16)
    ),
    type1_error = 0.05,
    one_sided = FALSE
  )

  # Test text report
  report_text <- .generate_generic_report(report_data, spec)

  expect_true(is.character(report_text))
  expect_true(length(report_text) > 0)
  # Flatten the list to check content
  report_content <- paste(report_text, collapse = "\n")
  expect_true(grepl("Two-Group t-test", report_content))
})

test_that("generic HTML report formatter produces valid HTML", {
  registry <- get_power_test_registry()
  spec <- registry$ttest_2groups

  report_data <- list(
    test_id = "ttest_2groups",
    test_name = "Two-Group t-test",
    format = "html",
    timestamp = Sys.time(),
    r_version = "4.0.0",
    parameters = list(),
    sample_sizes = list(n1 = 100, n2 = 100),
    effect_size_range = list(
      effect_sizes = seq(0.2, 0.8, length.out = 16),
      standardized = seq(0.2, 0.8, length.out = 16),
      method = "cohens_d"
    ),
    power_results = data.frame(
      effect_size = seq(0.2, 0.8, length.out = 16),
      standardized_es = seq(0.2, 0.8, length.out = 16),
      power = seq(0.1, 0.95, length.out = 16)
    ),
    type1_error = 0.05,
    one_sided = FALSE
  )

  report_html <- .generate_generic_report(report_data, spec)

  expect_true(is.character(report_html))
  # Flatten if it's a list
  if (is.list(report_html)) {
    report_html <- paste(report_html, collapse = "\n")
  }
  expect_true(grepl("<!DOCTYPE html>", report_html))
  expect_true(grepl("</html>", report_html))
})

test_that("framework properly handles all effect size methods", {
  registry <- get_power_test_registry()

  # Test ttest with multiple methods
  spec <- registry$ttest_2groups

  for (method in spec$effect_size_methods) {
    method_spec <- spec$effect_size_params[[method]]

    expect_true(!is.null(method_spec))
    expect_true(!is.null(method_spec$min))
    expect_true(!is.null(method_spec$max))
    expect_true(!is.null(method_spec$default_min))
    expect_true(!is.null(method_spec$default_max))
  }
})

test_that("framework validates inputs for each test type", {
  registry <- get_power_test_registry()

  # Valid inputs should produce empty validation list
  spec <- registry$ttest_2groups

  valid_inputs <- list(
    sample_size = 100,
    dropout = 0.1,
    allocation = "equal"
  )

  issues <- spec$validation(valid_inputs)
  expect_equal(length(issues), 0)

  # Invalid inputs should produce warnings
  invalid_inputs <- list(
    sample_size = -100,
    dropout = 0.1
  )

  issues <- spec$validation(invalid_inputs)
  expect_true(length(issues) > 0)
})

test_that("logrank specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$logrank

  expect_equal(spec$id, "logrank")
  expect_equal(spec$name, "Survival Log-rank")
  expect_true("hazard_ratio" %in% spec$effect_size_methods)
  expect_true("sample_size" %in% names(spec$parameters))
  expect_true("event_prob" %in% names(spec$parameters))
  expect_true("allocation" %in% names(spec$parameters))
})

test_that("logrank standardize converts HR to log(HR)", {
  registry <- get_power_test_registry()
  spec <- registry$logrank

  hrs <- c(1.5, 2.0, 3.0)
  result <- spec$standardize(hrs, "hazard_ratio", list())
  expect_equal(result, log(hrs))
})

test_that("logrank sample_size_calc applies event probability", {
  registry <- get_power_test_registry()
  spec <- registry$logrank

  params <- spec$sample_size_calc(list(
    sample_size = 200,
    event_prob = 0.5,
    allocation = "equal"
  ))

  expect_equal(params$n1, 50)
  expect_equal(params$n2, 50)
})

test_that("logrank power function returns valid power", {
  result <- logrank_power(
    h = log(2), n1 = 50, n2 = 50,
    sig.level = 0.05, alternative = "two.sided"
  )

  expect_true(!is.null(result$power))
  expect_true(result$power > 0 && result$power < 1)
})

test_that("logrank power increases with more events", {
  power_small <- logrank_power(
    h = log(2), n1 = 25, n2 = 25,
    sig.level = 0.05, alternative = "two.sided"
  )$power

  power_large <- logrank_power(
    h = log(2), n1 = 100, n2 = 100,
    sig.level = 0.05, alternative = "two.sided"
  )$power

  expect_true(power_large > power_small)
})

test_that("logrank validation catches invalid inputs", {
  registry <- get_power_test_registry()
  spec <- registry$logrank

  issues <- spec$validation(list(sample_size = 200, event_prob = 0.7))
  expect_equal(length(issues), 0)

  issues <- spec$validation(list(sample_size = -10, event_prob = 0.7))
  expect_true(length(issues) > 0)

  issues <- spec$validation(list(sample_size = 200, event_prob = 1.5))
  expect_true(length(issues) > 0)
})

test_that("fisher_exact specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$fisher_exact

  expect_equal(spec$id, "fisher_exact")
  expect_equal(spec$name, "Fisher's Exact Test")
  expect_true("proportions" %in% spec$effect_size_methods)
  expect_true("odds_ratio" %in% spec$effect_size_methods)
  expect_true("sample_size" %in% names(spec$parameters))
})

test_that("fisher_exact standardize uses Cohen's h", {
  registry <- get_power_test_registry()
  spec <- registry$fisher_exact

  result <- spec$standardize(c(0.3, 0.5), "proportions", list(p2 = 0.1))
  expect_true(all(result > 0))

  result_or <- spec$standardize(c(2, 4), "odds_ratio", list(baseline = 0.1))
  expect_true(all(result_or > 0))
})

test_that("fisher_exact sample_size_calc handles equal allocation", {
  registry <- get_power_test_registry()
  spec <- registry$fisher_exact

  params <- spec$sample_size_calc(list(
    sample_size = 60, allocation = "equal"
  ))

  expect_equal(params$n1, 30)
  expect_equal(params$n2, 30)
})

test_that("trend_prop specification is correct", {
  registry <- get_power_test_registry()
  spec <- registry$trend_prop

  expect_equal(spec$id, "trend_prop")
  expect_equal(spec$name, "Trend in Proportions")
  expect_true("prop_range" %in% spec$effect_size_methods)
  expect_true("sample_size" %in% names(spec$parameters))
  expect_true("n_groups" %in% names(spec$parameters))
})

test_that("trend_prop standardize computes valid effect size", {
  registry <- get_power_test_registry()
  spec <- registry$trend_prop

  result <- spec$standardize(
    c(0.3, 0.5, 0.7), "prop_range",
    list(p_low = 0.1, n_groups = 3)
  )

  expect_equal(length(result), 3)
  expect_true(all(result >= 0))
  expect_true(result[3] > result[1])
})

test_that("trend_prop power function returns valid power", {
  d <- 0.15
  result <- trend_power(n = 150, d = d, sig.level = 0.05,
                        alternative = "two.sided")

  expect_true(!is.null(result$power))
  expect_true(result$power > 0 && result$power < 1)
})

test_that("trend_prop power increases with sample size", {
  d <- 0.15
  power_small <- trend_power(n = 50, d = d, sig.level = 0.05,
                             alternative = "two.sided")$power
  power_large <- trend_power(n = 300, d = d, sig.level = 0.05,
                             alternative = "two.sided")$power

  expect_true(power_large > power_small)
})

test_that("trend_prop validation catches too few groups", {
  registry <- get_power_test_registry()
  spec <- registry$trend_prop

  issues <- spec$validation(list(sample_size = 150, n_groups = 3))
  expect_equal(length(issues), 0)

  issues <- spec$validation(list(sample_size = 150, n_groups = 2))
  expect_true(length(issues) > 0)
})
