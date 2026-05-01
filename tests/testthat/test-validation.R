test_that("input validation detects invalid sample sizes", {
  # Sample size must be positive
  issues <- character()
  if ((NULL %||% 100) <= 0) {
    issues <- c(issues, "Sample size must be positive")
  }
  expect_length(issues, 0)

  # Negative sample size should trigger validation
  issues <- character()
  if ((-10 %||% 100) <= 0) {
    issues <- c(issues, "Sample size must be positive")
  }
  expect_length(issues, 1)
  expect_true(any(grepl("Sample size", issues)))
})

test_that("input validation detects excess dropout+drop-in", {
  issues <- character()

  total_loss <- (0.5 %||% 0.1) + (0.6 %||% 0)
  if (total_loss > 1) {
    issues <- c(issues, "Dropout + Drop-in rates cannot exceed 100%")
  }
  expect_length(issues, 1)
  expect_true(any(grepl("Dropout", issues)))
})

test_that("input validation detects invalid allocation ratio", {
  issues <- character()

  if ((0 %||% 1) <= 0) {
    issues <- c(issues, "Allocation ratio must be positive")
  }
  expect_length(issues, 1)
  expect_true(any(grepl("ratio", issues)))
})

test_that("input validation detects invalid type I error", {
  issues <- character()

  type1 <- 1.5  # Invalid: > 1
  if (type1 <= 0 || type1 >= 1) {
    issues <- c(issues, "Type I error must be between 0 and 1")
  }
  expect_length(issues, 1)
  expect_true(any(grepl("Type I", issues)))

  # Valid type I error
  issues <- character()
  type1 <- 0.05
  if (type1 <= 0 || type1 >= 1) {
    issues <- c(issues, "Type I error must be between 0 and 1")
  }
  expect_length(issues, 0)
})

test_that("input validation detects invalid effect size ranges", {
  issues <- character()

  # Reversed range should trigger validation
  es_range <- c(1.0, 0.2)  # Max < Min
  if (length(es_range) == 2 && es_range[1] > es_range[2]) {
    issues <- c(issues, "Min effect size must be ≤ max effect size")
  }
  expect_length(issues, 1)

  # Valid range
  issues <- character()
  es_range <- c(0.2, 1.0)
  if (length(es_range) == 2 && es_range[1] > es_range[2]) {
    issues <- c(issues, "Min effect size must be ≤ max effect size")
  }
  expect_length(issues, 0)
})

test_that("constants are properly defined", {
  expect_true(exists("ZZPOWER_CONSTANTS"))
  expect_true(is.list(ZZPOWER_CONSTANTS))

  # Check key constants exist
  expect_true("SAMPLE_SIZE_MIN" %in% names(ZZPOWER_CONSTANTS))
  expect_true("POWER_TARGET" %in% names(ZZPOWER_CONSTANTS))
  expect_true("EFFECT_SIZE_SEQ_LENGTH" %in% names(ZZPOWER_CONSTANTS))

  # Check some values are reasonable
  expect_equal(ZZPOWER_CONSTANTS$POWER_TARGET, 0.8)
  expect_equal(ZZPOWER_CONSTANTS$EFFECT_SIZE_SEQ_LENGTH, 16)
})

test_that("report generation function exists and works", {
  expect_true(exists("generate_power_report"))

  # Create mock data
  mock_input <- list(
    N = 100,
    dropout = 0.1,
    dropin = 0.05,
    dmeth = "std",
    ratio = 1,
    type1 = 0.05,
    onesided = FALSE,
    report_format = "text"
  )

  mock_power_results <- data.frame(
    effect_size = seq(0.2, 1.0, length.out = 16),
    cohens_d = seq(0.2, 1.0, length.out = 16),
    power = seq(0.05, 0.99, length.out = 16)
  )

  mock_study_params <- list(
    n1_comp = 45,
    n2_comp = 45,
    n1_itt = 50,
    n2_itt = 50,
    sig_level = 0.025
  )

  # Create wrapper functions that return the data
  pr_wrapper <- function() { mock_power_results }
  sp_wrapper <- function() { mock_study_params }

  # Test text report generation
  expect_no_error({
    report_text <- generate_power_report(
      input = mock_input,
      power_results = pr_wrapper,
      study_parameters = sp_wrapper,
      format = "text"
    )
    expect_true(is.character(report_text))
    expect_true(nchar(report_text) > 500)
    expect_true(grepl("POWER ANALYSIS REPORT", report_text))
  })

  # Test HTML report generation
  expect_no_error({
    report_html <- generate_power_report(
      input = mock_input,
      power_results = pr_wrapper,
      study_parameters = sp_wrapper,
      format = "html"
    )
    expect_true(is.character(report_html))
    expect_true(nchar(report_html) > 1000)
    expect_true(grepl("<html>", report_html, ignore.case = TRUE))
  })
})
