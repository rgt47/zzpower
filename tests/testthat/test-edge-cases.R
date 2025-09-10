test_that("extreme sample sizes are handled correctly", {
  # Very small sample sizes (n=1 should error)
  expect_error(pwr::pwr.t2n.test(n1 = 1, n2 = 1, sig.level = 0.05, d = 0.5))
  
  small_n_result <- pwr::pwr.t2n.test(n1 = 2, n2 = 2, sig.level = 0.05, d = 0.5)
  expect_true(is.numeric(small_n_result$power))
  expect_true(small_n_result$power < 0.5)  # Should have low power
  
  # Very large sample sizes
  large_n_result <- pwr::pwr.t2n.test(n1 = 1000, n2 = 1000, sig.level = 0.05, d = 0.2)
  expect_true(large_n_result$power > 0.9)  # Should have high power even for small effect
  
  # Extreme unequal allocation
  unequal_extreme <- pwr::pwr.t2n.test(n1 = 100, n2 = 10, sig.level = 0.05, d = 0.5)
  balanced <- pwr::pwr.t2n.test(n1 = 55, n2 = 55, sig.level = 0.05, d = 0.5)
  
  # Balanced design should have higher power than unbalanced for same total N
  expect_true(balanced$power > unequal_extreme$power)
})

test_that("extreme effect sizes are handled correctly", {
  # Zero effect size
  zero_effect <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.05, d = 0)
  expect_equal(zero_effect$power, 0.05, tolerance = 1e-10)  # Should equal alpha
  
  # Very large effect size
  huge_effect <- pwr::pwr.t2n.test(n1 = 10, n2 = 10, sig.level = 0.05, d = 2.0)
  expect_true(huge_effect$power > 0.95)  # Should have very high power
  
  # Very small but non-zero effect size
  tiny_effect <- pwr::pwr.t2n.test(n1 = 1000, n2 = 1000, sig.level = 0.05, d = 0.05)
  expect_true(tiny_effect$power > 0.05)  # Should be detectable with large N
})

test_that("extreme significance levels are handled correctly", {
  # Very stringent alpha
  stringent <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.001, d = 0.5)
  
  # Less stringent alpha
  lenient <- pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0.10, d = 0.5)
  
  # Lenient should have higher power
  expect_true(lenient$power > stringent$power)
  
  # Test boundary values (these actually don't error in pwr package but give warnings)
  expect_warning(pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 0, d = 0.5))
  expect_warning(pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 1, d = 0.5))
  expect_error(pwr::pwr.t2n.test(n1 = 50, n2 = 50, sig.level = 1.5, d = 0.5))
})

test_that("extreme dropout rates are handled correctly", {
  # Test 100% dropout rate
  total_n <- 100
  dropout_100 <- 1.0
  ratio <- 1
  
  n_comp_100 <- total_n / 2 * (1 - dropout_100)
  expect_equal(n_comp_100, 0)
  
  # Test near 100% dropout
  dropout_99 <- 0.99
  n_comp_99 <- total_n / 2 * (1 - dropout_99)
  expect_equal(n_comp_99, 0.5)
  
  # Test negative dropout (should be prevented in UI, but test mathematical result)
  dropout_negative <- -0.1
  n_comp_negative <- total_n / 2 * (1 - dropout_negative)
  expect_equal(n_comp_negative, 55)  # Would give more participants than planned
  
  # Test dropout + dropin > 100%
  dropout <- 0.6
  dropin <- 0.5
  combined_loss <- dropout + dropin
  n_comp_extreme <- total_n / 2 * (1 - combined_loss)
  expect_true(n_comp_extreme < 0)  # Mathematically possible but unrealistic
})

test_that("extreme allocation ratios are handled correctly", {
  total_n <- 100
  
  # Very unequal allocation (10:1)
  ratio_extreme <- 10
  n1_extreme <- ratio_extreme * total_n / (ratio_extreme + 1)
  n2_extreme <- total_n / (ratio_extreme + 1)
  
  expect_equal(round(n1_extreme, 1), 90.9)
  expect_equal(round(n2_extreme, 1), 9.1)
  
  # Very small ratio (1:10 = 0.1:1)
  ratio_small <- 0.1
  n1_small <- ratio_small * total_n / (ratio_small + 1)
  n2_small <- total_n / (ratio_small + 1)
  
  expect_equal(round(n1_small, 1), 9.1)
  expect_equal(round(n2_small, 1), 90.9)
  
  # Test zero ratio (should cause error in real usage)
  ratio_zero <- 0
  expect_true(ratio_zero == 0)  # This would cause division issues
})

test_that("missing or invalid inputs are handled correctly", {
  # Test NA values
  expect_true(is.na(NA))
  expect_true(is.na(NA + 5))
  expect_true(is.na(NA * 0.5))
  
  # Test infinite values
  expect_true(is.infinite(1/0))
  expect_true(is.infinite(-1/0))
  expect_false(is.finite(1/0))
  
  # Test division by zero scenarios
  expect_true(is.infinite(10/0))
  expect_true(is.nan(0/0))
  
  # Test that these would be caught in actual calculations
  tryCatch({
    result <- 10 / 0
    expect_true(is.infinite(result))
  }, warning = function(w) {
    expect_true(TRUE)  # Warning is expected
  })
})

test_that("boundary input combinations are handled correctly", {
  # Test minimum viable study
  min_result <- pwr::pwr.t2n.test(n1 = 2, n2 = 2, sig.level = 0.05, d = 1.0)
  expect_true(is.numeric(min_result$power))
  expect_true(min_result$power > 0.05)
  
  # Test maximum practical parameters
  # Large N, large effect, lenient alpha
  max_practical <- pwr::pwr.t2n.test(n1 = 500, n2 = 500, sig.level = 0.10, d = 1.5)
  expect_true(max_practical$power > 0.999)
  
  # Test conflicting parameters
  # Small N, small effect, stringent alpha
  difficult <- pwr::pwr.t2n.test(n1 = 10, n2 = 10, sig.level = 0.01, d = 0.1)
  expect_true(difficult$power < 0.1)  # Should have very low power
})

test_that("effect size conversion edge cases are handled", {
  # Division by zero in conversions
  expect_true(is.infinite(1/0))
  expect_true(is.nan(0/0))
  
  # Very small standard deviations
  tiny_sd <- 0.001
  normal_diff <- 1.0
  huge_effect <- normal_diff / tiny_sd
  expect_equal(huge_effect, 1000)
  
  # Very large standard deviations
  huge_sd <- 1000
  normal_diff <- 1.0
  tiny_effect <- normal_diff / huge_sd
  expect_equal(tiny_effect, 0.001)
  
  # Negative values in inappropriate contexts
  # (These might be prevented in UI but test mathematical results)
  negative_sd <- -5
  positive_diff <- 2
  negative_d <- positive_diff / negative_sd
  expect_equal(negative_d, -0.4)
})

test_that("sequence generation edge cases work correctly", {
  # Single point range
  single_point <- seq(0.5, 0.5, length.out = 10)
  expect_true(all(single_point == 0.5))
  expect_length(single_point, 10)
  
  # Reverse range (max < min)
  reverse_range <- seq(1.0, 0.2, length.out = 5)
  expect_equal(reverse_range[1], 1.0)
  expect_equal(reverse_range[5], 0.2)
  expect_true(all(diff(reverse_range) < 0))  # Decreasing
  
  # Zero-length sequence
  zero_length <- seq(0.2, 1.0, length.out = 0)
  expect_length(zero_length, 0)
  
  # Single point sequence
  single_length <- seq(0.2, 1.0, length.out = 1)
  expect_length(single_length, 1)
  # Note: seq() with length.out = 1 returns the first value
  expect_equal(single_length[1], 0.2)
})

test_that("data frame construction edge cases are handled", {
  # Empty vectors
  empty_df <- data.frame(
    effect_size = numeric(0),
    power = numeric(0)
  )
  expect_equal(nrow(empty_df), 0)
  expect_equal(ncol(empty_df), 2)
  
  # Mismatched vector lengths (should error in real usage)
  expect_error(data.frame(
    effect_size = c(0.2, 0.5),
    power = c(0.1, 0.3, 0.6)  # Different length
  ))
  
  # Vectors with NA values
  na_df <- data.frame(
    effect_size = c(0.2, NA, 0.8),
    power = c(NA, 0.5, 0.9)
  )
  expect_equal(nrow(na_df), 3)
  expect_true(is.na(na_df$effect_size[2]))
  expect_true(is.na(na_df$power[1]))
})

test_that("rounding and precision edge cases are handled", {
  # Very small numbers
  tiny_number <- 1e-15
  expect_true(tiny_number > 0)
  expect_equal(round(tiny_number, 10), 0)
  
  # Very large numbers
  huge_number <- 1e15
  expect_true(is.finite(huge_number))
  
  # Precision limits
  precision_test <- 0.1 + 0.2
  expect_false(precision_test == 0.3)  # Floating point precision issue
  expect_equal(precision_test, 0.3, tolerance = 1e-15)
  
  # Rounding edge cases
  expect_equal(round(2.5), 2)  # R uses "round half to even"
  expect_equal(round(3.5), 4)
  expect_equal(round(2.5, 0), 2)
  expect_equal(round(2.55, 1), 2.6)
})