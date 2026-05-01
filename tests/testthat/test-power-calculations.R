test_that("power calculations work correctly", {
  # Test basic power calculation using pwr package directly
  # This serves as a baseline comparison for our app calculations
  
  # Basic two-sample t-test power calculation
  result <- pwr::pwr.t2n.test(n1 = 25, n2 = 25, sig.level = 0.05, d = 0.5)
  expect_true(result$power > 0.4 && result$power < 0.6)  # Should be around 0.5
  
  # Test with different effect sizes
  small_effect <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.2)
  large_effect <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.8)
  
  expect_true(small_effect$power < large_effect$power)
  
  # Test unequal sample sizes
  unequal <- pwr::pwr.t2n.test(n1 = 30, n2 = 20, sig.level = 0.05, d = 0.5)
  expect_true(is.numeric(unequal$power))
  expect_true(unequal$power > 0 && unequal$power < 1)
})

test_that("effect size conversions work correctly", {
  # These test the logic that would be used in the server functions
  
  # Test Cohen's d to Cohen's d (should be identity)
  cohens_d <- 0.5
  expect_equal(cohens_d, 0.5)
  
  # Test percentage reduction to Cohen's d conversion
  # If placebo change is 10 and SD is 5, 50% reduction means effect size of 1.0
  placebo_change <- 10
  placebo_sd <- 5
  pct_reduction <- 0.5
  expected_d <- (pct_reduction * placebo_change) / placebo_sd
  expect_equal(expected_d, 1.0)
  
  # Test difference in change scores to Cohen's d
  difference <- 2.5
  sd <- 5
  expected_d_diff <- difference / sd
  expect_equal(expected_d_diff, 0.5)
  
  # Test active group change to Cohen's d
  active_change <- 5
  placebo_change <- 10
  sd <- 5
  expected_d_active <- (placebo_change - active_change) / sd
  expect_equal(expected_d_active, 1.0)
})

test_that("sample size calculations handle dropout correctly", {
  # Test dropout rate calculations
  total_n <- 100
  dropout_rate <- 0.1
  ratio <- 1  # 1:1 allocation
  
  # Expected completer sample sizes
  expected_n1_comp <- (ratio * total_n) / (ratio + 1) * (1 - dropout_rate)
  expected_n2_comp <- total_n / (ratio + 1) * (1 - dropout_rate)
  
  expect_equal(expected_n1_comp, 45)  # 50 * 0.9
  expect_equal(expected_n2_comp, 45)  # 50 * 0.9
  
  # Test with unequal allocation (2:1)
  ratio_2to1 <- 2
  expected_n1_comp_unequal <- (ratio_2to1 * total_n) / (ratio_2to1 + 1) * (1 - dropout_rate)
  expected_n2_comp_unequal <- total_n / (ratio_2to1 + 1) * (1 - dropout_rate)
  
  expect_equal(round(expected_n1_comp_unequal, 1), 60)  # 66.67 * 0.9 ≈ 60
  expect_equal(expected_n2_comp_unequal, 30)  # 33.33 * 0.9 = 30
  
  # Test edge case: 100% dropout
  extreme_dropout <- 1.0
  n_with_extreme_dropout <- total_n / 2 * (1 - extreme_dropout)
  expect_equal(n_with_extreme_dropout, 0)
})

test_that("power ranges are sensible across parameter ranges", {
  # Test that power increases with sample size
  small_n <- pwr::pwr.t2n.test(n1 = 10, n2 = 10, sig.level = 0.05, d = 0.5)
  large_n <- pwr::pwr.t2n.test(n1 = 100, n2 = 100, sig.level = 0.05, d = 0.5)
  
  expect_true(small_n$power < large_n$power)
  
  # Test that power increases with effect size
  small_effect <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.2)
  large_effect <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.8)
  
  expect_true(small_effect$power < large_effect$power)
  
  # Test that power decreases with more stringent alpha
  alpha_05 <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.5)
  alpha_01 <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.01, d = 0.5)
  
  expect_true(alpha_01$power < alpha_05$power)
})

test_that("one-sided vs two-sided tests work correctly", {
  # Two-sided test (standard alpha = 0.05)
  two_sided <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.5)
  
  # One-sided test - to get equivalent power, we use the same overall alpha
  # but pwr package expects the full alpha level for one-sided tests
  # In our app logic: sig_level = type1 / sided, where sided = 1 for one-sided, 2 for two-sided
  # So for two-sided with type1 = 0.05: sig_level = 0.05 / 2 = 0.025
  # For one-sided with type1 = 0.05: sig_level = 0.05 / 1 = 0.05
  # But pwr package itself doesn't distinguish, so we simulate by using different alpha
  two_sided_corrected <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.025, d = 0.5)
  one_sided_corrected <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0.5)
  
  # One-sided test should have higher power when using same overall Type I error rate
  expect_true(one_sided_corrected$power > two_sided_corrected$power)
})

test_that("error handling in power calculations", {
  # Test invalid inputs
  expect_error(pwr::pwr.t2n.test(n1 = -1, n2 = 50, sig.level = 0.05, d = 0.5))
  expect_error(pwr::pwr.t2n.test(n1 = 50, n2 = -1, sig.level = 0.05, d = 0.5))
  expect_error(pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = -0.05, d = 0.5))
  expect_error(pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 1.05, d = 0.5))
  
  # Test zero sample sizes
  expect_error(pwr::pwr.t2n.test(n1 = 0, n2 = 50, sig.level = 0.05, d = 0.5))
  expect_error(pwr::pwr.t2n.test(n1 = 50, n2 = 0, sig.level = 0.05, d = 0.5))
})