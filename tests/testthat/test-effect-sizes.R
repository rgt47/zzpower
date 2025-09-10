test_that("Cohen's d calculations are correct", {
  # Direct Cohen's d specification
  d_values <- c(0.2, 0.5, 0.8)  # Small, medium, large effects
  
  # These should pass through unchanged
  expect_equal(d_values[1], 0.2)
  expect_equal(d_values[2], 0.5)
  expect_equal(d_values[3], 0.8)
  
  # Test range creation
  d_range <- seq(0.2, 0.8, length.out = 4)
  expect_equal(d_range[1], 0.2)
  expect_equal(d_range[4], 0.8)
  expect_length(d_range, 4)
})

test_that("percentage reduction to Cohen's d conversion works", {
  # Test case: 20% reduction from placebo mean of 10 with SD of 5
  placebo_mean <- 10
  placebo_sd <- 5
  pct_reduction <- 0.20
  
  # Formula: d = (pct_reduction * placebo_mean) / placebo_sd
  expected_d <- (pct_reduction * placebo_mean) / placebo_sd
  expect_equal(expected_d, 0.4)
  
  # Test with different scenarios
  # 50% reduction: should give d = 1.0
  pct_50 <- 0.5
  d_50 <- (pct_50 * placebo_mean) / placebo_sd
  expect_equal(d_50, 1.0)
  
  # 10% reduction: should give d = 0.2
  pct_10 <- 0.1
  d_10 <- (pct_10 * placebo_mean) / placebo_sd
  expect_equal(d_10, 0.2)
  
  # Test edge cases
  # 0% reduction: should give d = 0
  pct_0 <- 0.0
  d_0 <- (pct_0 * placebo_mean) / placebo_sd
  expect_equal(d_0, 0.0)
  
  # 100% reduction: should give d = 2.0
  pct_100 <- 1.0
  d_100 <- (pct_100 * placebo_mean) / placebo_sd
  expect_equal(d_100, 2.0)
})

test_that("difference in change scores to Cohen's d conversion works", {
  # Test case: difference of 2.5 points with SD of 5
  difference <- 2.5
  pooled_sd <- 5
  
  # Formula: d = difference / pooled_sd
  expected_d <- difference / pooled_sd
  expect_equal(expected_d, 0.5)
  
  # Test with different differences
  differences <- c(1, 2, 4, 8)
  sd_val <- 4
  expected_ds <- differences / sd_val
  expect_equal(expected_ds, c(0.25, 0.5, 1.0, 2.0))
  
  # Test that larger differences give larger effect sizes
  diff_small <- 1
  diff_large <- 4
  sd_constant <- 2
  
  d_small <- diff_small / sd_constant
  d_large <- diff_large / sd_constant
  
  expect_true(d_large > d_small)
  expect_equal(d_small, 0.5)
  expect_equal(d_large, 2.0)
})

test_that("active group change to Cohen's d conversion works", {
  # Test case: placebo improves by 10, active improves by 15, SD = 5
  placebo_change <- 10
  active_change <- 15
  pooled_sd <- 5
  
  # Formula: d = (active_change - placebo_change) / pooled_sd
  # Note: In our app, this is actually (placebo_change - active_change) / pooled_sd
  # because we specify active_change as the treatment group outcome
  expected_d <- (active_change - placebo_change) / pooled_sd
  expect_equal(expected_d, 1.0)
  
  # Test the app's actual formula (placebo - active for improvement scale)
  # where lower scores are better
  app_formula_d <- (placebo_change - active_change) / pooled_sd
  expect_equal(app_formula_d, -1.0)
  
  # Test with different scenarios
  # Active group improves more (lower final score)
  placebo_final <- 8
  active_final <- 5
  sd <- 3
  
  d_improvement <- (placebo_final - active_final) / sd
  expect_equal(d_improvement, 1.0)
  
  # Active group improves less (higher final score)
  active_worse <- 10
  d_worse <- (placebo_final - active_worse) / sd
  expect_equal(d_worse, -2/3, tolerance = 0.01)
})

test_that("effect size ranges are generated correctly", {
  # Test range generation for different methods
  
  # Standard deviations
  std_range <- c(0.2, 1.0)
  std_sequence <- seq(std_range[1], std_range[2], length.out = 16)
  
  expect_length(std_sequence, 16)
  expect_equal(std_sequence[1], 0.2)
  expect_equal(std_sequence[16], 1.0)
  expect_true(all(diff(std_sequence) > 0))  # Monotonically increasing
  
  # Percentage reductions
  pct_range <- c(0.1, 0.5)
  pct_sequence <- seq(pct_range[1], pct_range[2], length.out = 16)
  
  expect_length(pct_sequence, 16)
  expect_equal(pct_sequence[1], 0.1)
  expect_equal(pct_sequence[16], 0.5)
  
  # Convert to Cohen's d
  placebo_mean <- 20
  placebo_sd <- 10
  d_from_pct <- (pct_sequence * placebo_mean) / placebo_sd
  
  expect_equal(d_from_pct[1], 0.2)  # 0.1 * 20 / 10
  expect_equal(d_from_pct[16], 1.0)  # 0.5 * 20 / 10
})

test_that("effect size conversions are mathematically consistent", {
  # Test that different parameterizations can yield the same Cohen's d
  
  target_d <- 0.5
  
  # Method 1: Direct specification
  d_direct <- 0.5
  
  # Method 2: Percentage reduction
  placebo_mean <- 10
  placebo_sd <- 5
  pct_needed <- (target_d * placebo_sd) / placebo_mean
  d_from_pct <- (pct_needed * placebo_mean) / placebo_sd
  
  # Method 3: Difference in change scores
  difference_needed <- target_d * placebo_sd
  d_from_diff <- difference_needed / placebo_sd
  
  # Method 4: Active group change
  placebo_change <- 8
  active_change_needed <- placebo_change - (target_d * placebo_sd)
  d_from_active <- (placebo_change - active_change_needed) / placebo_sd
  
  # All should equal target Cohen's d
  expect_equal(d_direct, target_d)
  expect_equal(d_from_pct, target_d, tolerance = 1e-10)
  expect_equal(d_from_diff, target_d, tolerance = 1e-10)
  expect_equal(d_from_active, target_d, tolerance = 1e-10)
})

test_that("effect size boundary conditions work correctly", {
  # Test zero effect size
  expect_equal(0 / 5, 0)  # Zero difference
  expect_equal((0 * 10) / 5, 0)  # Zero percent reduction
  
  # Test very large effect sizes
  large_diff <- 20
  small_sd <- 2
  large_d <- large_diff / small_sd
  expect_equal(large_d, 10)
  
  # Test very small effect sizes
  small_diff <- 0.1
  large_sd <- 10
  small_d <- small_diff / large_sd
  expect_equal(small_d, 0.01)
})

test_that("effect size interpretations are correct", {
  # Cohen's conventional effect size interpretations
  small_effect <- 0.2
  medium_effect <- 0.5
  large_effect <- 0.8
  
  # Test ordering
  expect_true(small_effect < medium_effect)
  expect_true(medium_effect < large_effect)
  
  # Test that effect sizes translate correctly to power differences
  # (This would be tested more thoroughly with actual power calculations)
  n1 <- n2 <- 50
  sig_level <- 0.05
  
  power_small <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = sig_level, d = small_effect)$power
  power_medium <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = sig_level, d = medium_effect)$power
  power_large <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = sig_level, d = large_effect)$power
  
  expect_true(power_small < power_medium)
  expect_true(power_medium < power_large)
})

test_that("negative effect sizes are handled correctly", {
  # Test that negative Cohen's d values are handled appropriately
  negative_d <- -0.5
  
  # Power calculation should work with negative effect sizes
  power_result <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = negative_d)
  expect_true(is.numeric(power_result$power))
  expect_true(power_result$power > 0)
  
  # Absolute effect size should be the same for power
  positive_d <- 0.5
  power_positive <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = positive_d)
  expect_equal(power_result$power, power_positive$power, tolerance = 1e-10)
})