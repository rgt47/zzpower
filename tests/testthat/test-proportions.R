test_that("proportion constants are properly defined", {
  expect_true(exists("ZZPOWER_CONSTANTS"))
  consts <- ZZPOWER_CONSTANTS

  # Check proportion-specific constants
  expect_true("PROPORTION_MIN" %in% names(consts))
  expect_true("PROPORTION_MAX" %in% names(consts))
  expect_true("PROPORTION_DEFAULT_1" %in% names(consts))
  expect_true("PROPORTION_DEFAULT_2" %in% names(consts))

  # Check bounds
  expect_equal(consts$PROPORTION_MIN, 0.01)
  expect_equal(consts$PROPORTION_MAX, 0.99)
  expect_equal(consts$PROPORTION_DEFAULT_1, 0.5)
  expect_equal(consts$PROPORTION_DEFAULT_2, 0.3)
})

test_that("Cohen's h conversion from proportions is correct", {
  # Test basic conversion
  h <- prop_to_cohens_h(0.6, 0.4)
  expect_true(is.numeric(h))
  expect_true(h > 0)

  # Test identity
  h_same <- prop_to_cohens_h(0.5, 0.5)
  expect_equal(h_same, 0, tolerance = 0.001)

  # Test symmetry of transformation
  h1 <- prop_to_cohens_h(0.7, 0.3)
  h2 <- prop_to_cohens_h(0.3, 0.7)
  expect_equal(abs(h1), abs(h2), tolerance = 0.001)
})

test_that("Cohen's h to proportions inverse transformation works", {
  # Test conversion and inverse
  p1_original <- 0.6
  p2 <- 0.4
  h <- prop_to_cohens_h(p1_original, p2)
  p1_recovered <- cohens_h_to_prop(h, p2)

  expect_equal(p1_recovered, p1_original, tolerance = 0.01)
})

test_that("proportion difference to Cohen's h conversion works", {
  # Test difference conversion
  baseline <- 0.5
  diff <- -0.2

  h <- diff_to_cohens_h(diff, baseline)
  expect_true(is.numeric(h))
  # h can be negative if p1 < p2
  expect_true(is.numeric(h) && !is.na(h))

  # If p1 = baseline + diff and p2 = baseline
  # Then p1 = 0.3, p2 = 0.5
  h_direct <- prop_to_cohens_h(0.3, 0.5)
  expect_equal(h, h_direct, tolerance = 0.001)
})

test_that("odds ratio to Cohen's h conversion works", {
  # Test odds ratio conversion
  baseline <- 0.5
  or <- 2

  h <- or_to_cohens_h(or, baseline)
  expect_true(is.numeric(h))
  expect_true(h > 0)

  # OR = (p1/(1-p1)) / (p2/(1-p2))
  # With p2 = 0.5 and OR = 2:
  # odds_1 = 2 * 1 = 2
  # p1 = 2/3
  expected_p1 <- 2/3
  h_direct <- prop_to_cohens_h(expected_p1, 0.5)
  expect_equal(h, h_direct, tolerance = 0.001)
})

test_that("relative risk to Cohen's h conversion works", {
  # Test relative risk conversion
  baseline <- 0.5
  rr <- 1.5

  h <- rr_to_cohens_h(rr, baseline)
  expect_true(is.numeric(h))
  expect_true(h > 0)

  # RR = p1 / p2
  # With p2 = 0.5 and RR = 1.5:
  # p1 = 1.5 * 0.5 = 0.75
  expected_p1 <- 0.75
  h_direct <- prop_to_cohens_h(expected_p1, 0.5)
  expect_equal(h, h_direct, tolerance = 0.001)
})

test_that("effect size range generation works for proportions method", {
  result <- generate_prop_effect_sizes(
    method = "proportions",
    p1 = 0.6,
    p2 = 0.4,
    n_points = 16
  )

  expect_true(is.list(result))
  expect_true("effect_sizes" %in% names(result))
  expect_true("cohens_h" %in% names(result))
  expect_length(result$effect_sizes, 16)
  expect_length(result$cohens_h, 16)
})

test_that("effect size range generation works for difference method", {
  result <- generate_prop_effect_sizes(
    method = "difference",
    diff_min = -0.3,
    diff_max = -0.05,
    baseline = 0.5,
    n_points = 16
  )

  expect_true(is.list(result))
  expect_length(result$effect_sizes, 16)
  expect_length(result$cohens_h, 16)
  expect_equal(result$effect_sizes[1], -0.3, tolerance = 0.01)
  expect_equal(result$effect_sizes[16], -0.05, tolerance = 0.01)
})

test_that("effect size range generation works for OR method", {
  result <- generate_prop_effect_sizes(
    method = "or",
    or_min = 1.2,
    or_max = 3,
    baseline = 0.5,
    n_points = 16
  )

  expect_true(is.list(result))
  expect_length(result$effect_sizes, 16)
  expect_length(result$cohens_h, 16)
})

test_that("effect size range generation works for RR method", {
  result <- generate_prop_effect_sizes(
    method = "rr",
    rr_min = 1.2,
    rr_max = 3,
    baseline = 0.5,
    n_points = 16
  )

  expect_true(is.list(result))
  expect_length(result$effect_sizes, 16)
  expect_length(result$cohens_h, 16)
})

test_that("pwr.2p2n.test calculates power correctly for proportions", {
  # Test basic power calculation
  # Two groups: p1 = 0.6, p2 = 0.4
  h <- prop_to_cohens_h(0.6, 0.4)
  power_result <- pwr::pwr.2p2n.test(h = h, n1 = 100, n2 = 100, sig.level = 0.05)

  expect_true(is.numeric(power_result$power))
  expect_true(power_result$power > 0 && power_result$power < 1)
})

test_that("sample size calculation handles equal allocation", {
  # Test equal allocation
  total_n <- 200
  n1 <- total_n / 2
  n2 <- total_n / 2

  expect_equal(n1, 100)
  expect_equal(n2, 100)
  expect_equal(n1 + n2, 200)
})

test_that("sample size calculation handles unequal allocation", {
  # Test unequal allocation with ratio 2:1
  total_n <- 300
  ratio <- 2
  n1 <- ratio * total_n / (ratio + 1)
  n2 <- total_n / (ratio + 1)

  expect_equal(n1, 200)
  expect_equal(n2, 100)
  expect_equal(n1 + n2, 300)
})

test_that("axis labels are correct for each method", {
  label_prop <- get_prop_axis_label("proportions")
  expect_true(grepl("Proportion", label_prop))

  label_diff <- get_prop_axis_label("difference")
  expect_true(grepl("Difference", label_diff))

  label_or <- get_prop_axis_label("or")
  expect_true(grepl("Odds", label_or))

  label_rr <- get_prop_axis_label("rr")
  expect_true(grepl("Risk", label_rr))
})

test_that("effect size labels are formatted correctly", {
  label_prop <- get_prop_effect_label("proportions", 0.6)
  expect_true(grepl("0.600", label_prop))

  label_diff <- get_prop_effect_label("difference", -0.2)
  expect_true(grepl("-0.200", label_diff))

  label_or <- get_prop_effect_label("or", 2.5)
  expect_true(grepl("2.500", label_or))

  label_rr <- get_prop_effect_label("rr", 1.8)
  expect_true(grepl("1.800", label_rr))
})

test_that("proportion report generation functions exist", {
  expect_true(exists("generate_proportions_report"))
  expect_true(is.function(generate_proportions_report))
})

test_that("UI creation functions for proportions exist", {
  expect_true(exists("create_proportions_ui"))
  expect_true(exists("create_prop_sample_size_inputs"))
  expect_true(exists("create_prop_effect_size_inputs"))
  expect_true(exists("create_prop_advanced_settings"))
})

test_that("server function for proportions exists", {
  expect_true(exists("create_proportions_server"))
  expect_true(is.function(create_proportions_server))
})

test_that("proportion conversion helpers are robust to boundary values", {
  # Test with extreme but valid values
  p1_high <- prop_to_cohens_h(0.99, 0.01)
  expect_true(is.numeric(p1_high))
  expect_true(!is.na(p1_high))

  p1_low <- prop_to_cohens_h(0.01, 0.99)
  expect_true(is.numeric(p1_low))
  expect_true(!is.na(p1_low))

  # Both should have similar magnitude (symmetry)
  # p1_high should be positive, p1_low should be negative
  expect_equal(abs(p1_high), abs(p1_low), tolerance = 0.1)
})

test_that("proportions with small effects can be calculated", {
  # Small effect size
  h_small <- prop_to_cohens_h(0.52, 0.48)
  expect_true(h_small > 0 && h_small < 0.2)

  # Power with small effect should be lower
  power_small <- pwr::pwr.2p2n.test(
    h = h_small, n1 = 100, n2 = 100, sig.level = 0.05
  )$power

  h_large <- prop_to_cohens_h(0.7, 0.3)
  power_large <- pwr::pwr.2p2n.test(
    h = h_large, n1 = 100, n2 = 100, sig.level = 0.05
  )$power

  expect_true(power_small < power_large)
})
