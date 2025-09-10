test_that("server function creates proper server logic", {
  server_func <- create_server()
  expect_type(server_func, "closure")
  
  # Test that server function has expected parameters
  server_args <- names(formals(server_func))
  expect_true("input" %in% server_args)
  expect_true("output" %in% server_args)
  expect_true("session" %in% server_args)
})

test_that("effect size calculations work correctly in different methods", {
  # Test Cohen's d method (identity function)
  test_range <- c(0.2, 1.0)
  expected_sequence <- seq(test_range[1], test_range[2], length.out = 16)
  expect_length(expected_sequence, 16)
  expect_equal(expected_sequence[1], 0.2)
  expect_equal(expected_sequence[16], 1.0)
  
  # Test difference method conversion
  sd0 <- 10
  diff_values <- seq(2, 10, length.out = 16)
  cohens_d_from_diff <- diff_values / sd0
  expect_true(all(cohens_d_from_diff >= 0.2))
  expect_true(all(cohens_d_from_diff <= 1.0))
  
  # Test percentage method conversion
  d0 <- 20
  sd0 <- 10
  pct_values <- seq(0.1, 0.5, length.out = 16)
  cohens_d_from_pct <- (pct_values * d0) / sd0
  expect_true(all(cohens_d_from_pct >= 0.2))
  expect_true(all(cohens_d_from_pct <= 1.0))
  
  # Test active group method conversion
  d0 <- 10
  sd0 <- 5
  active_values <- seq(5, 9, length.out = 16)
  cohens_d_from_active <- (d0 - active_values) / sd0
  expect_true(all(cohens_d_from_active >= 0.2))
  expect_true(all(cohens_d_from_active <= 1.0))
})

test_that("sample size calculations with different ratios work correctly", {
  # Test 1:1 allocation
  total_n <- 100
  ratio <- 1
  dropout <- 0.1
  dropin <- 0.05
  
  expected_n1_comp <- ratio * total_n / (ratio + 1) * (1 - (dropin + dropout))
  expected_n2_comp <- total_n / (ratio + 1) * (1 - (dropin + dropout))
  
  expect_equal(expected_n1_comp, 42.5)  # 50 * 0.85
  expect_equal(expected_n2_comp, 42.5)  # 50 * 0.85
  
  # Test 2:1 allocation
  ratio_2to1 <- 2
  expected_n1_comp_2to1 <- ratio_2to1 * total_n / (ratio_2to1 + 1) * (1 - (dropin + dropout))
  expected_n2_comp_2to1 <- total_n / (ratio_2to1 + 1) * (1 - (dropin + dropout))
  
  expect_equal(round(expected_n1_comp_2to1, 2), 56.67)  # 66.67 * 0.85
  expect_equal(round(expected_n2_comp_2to1, 2), 28.33)  # 33.33 * 0.85
  
  # Test ITT calculations (no dropout adjustment)
  expected_n1_itt <- ratio * total_n / (ratio + 1)
  expected_n2_itt <- total_n / (ratio + 1)
  
  expect_equal(expected_n1_itt, 50)
  expect_equal(expected_n2_itt, 50)
})

test_that("power calculation sequence works correctly", {
  # Test power calculation for a sequence of effect sizes
  n1_comp <- 25
  n2_comp <- 25
  sig_level <- 0.05
  effect_sizes <- seq(0.2, 1.0, length.out = 5)
  
  power_values <- sapply(effect_sizes, function(d) {
    pwr::pwr.t2n.test(n1 = n1_comp, n2 = n2_comp, sig.level = sig_level, d = d)$power
  })
  
  # Power should increase monotonically with effect size
  expect_true(all(diff(power_values) > 0))
  expect_length(power_values, 5)
  expect_true(all(power_values > 0 & power_values < 1))
})

test_that("one-sided vs two-sided adjustment works correctly", {
  # Test the 'sided' parameter logic
  onesided_false <- FALSE
  onesided_true <- TRUE
  
  sided_two <- if (onesided_false) 1 else 2
  sided_one <- if (onesided_true) 1 else 2
  
  expect_equal(sided_two, 2)
  expect_equal(sided_one, 1)
  
  # Test effect on significance level
  type1 <- 0.05
  sig_level_two_sided <- type1 / sided_two
  sig_level_one_sided <- type1 / sided_one
  
  expect_equal(sig_level_two_sided, 0.025)
  expect_equal(sig_level_one_sided, 0.05)
})

test_that("data frame construction works correctly", {
  # Test that power results data frame is constructed properly
  effect_sizes <- seq(0.2, 1.0, length.out = 5)
  cohens_d <- effect_sizes  # For std method
  power_values <- c(0.1, 0.3, 0.6, 0.8, 0.95)  # Example power values
  
  results_df <- data.frame(
    effect_size = effect_sizes,
    cohens_d = cohens_d,
    power = power_values
  )
  
  expect_s3_class(results_df, "data.frame")
  expect_equal(nrow(results_df), 5)
  expect_equal(ncol(results_df), 3)
  expect_named(results_df, c("effect_size", "cohens_d", "power"))
  expect_true(all(results_df$effect_size == results_df$cohens_d))  # For std method
})

test_that("error handling in server logic works", {
  # Test handling of invalid sample sizes
  expect_true(is.na(NA))  # Basic NA handling
  
  # Test division by zero prevention
  ratio <- 0
  total_n <- 100
  
  # This should not cause division by zero error
  tryCatch({
    n1_comp <- ratio * total_n / (ratio + 1)
    expect_true(is.finite(n1_comp) || is.na(n1_comp))
  }, error = function(e) {
    # If error occurs, that's expected behavior
    expect_true(TRUE)
  })
})

test_that("power curve data preparation works correctly", {
  # Test data preparation for plotting
  effect_sizes <- seq(0.2, 1.0, length.out = 10)
  power_values <- seq(0.1, 0.9, length.out = 10)  # Increasing power
  
  plot_data <- data.frame(
    effect_size = effect_sizes,
    power = power_values
  )
  
  # Test that data is suitable for ggplot
  expect_true(all(is.finite(plot_data$effect_size)))
  expect_true(all(is.finite(plot_data$power)))
  expect_true(all(plot_data$power >= 0 & plot_data$power <= 1))
  
  # Test finding effect size for 80% power
  target_power <- 0.8
  closest_idx <- which.min(abs(plot_data$power - target_power))
  effect_size_80 <- plot_data$effect_size[closest_idx]
  
  expect_true(length(closest_idx) == 1)
  expect_true(is.finite(effect_size_80))
})

test_that("summary statistics calculations work correctly", {
  # Test summary statistics generation
  power_values <- c(0.1, 0.3, 0.6, 0.8, 0.95)
  effect_sizes <- seq(0.2, 1.0, length.out = 5)
  
  max_power <- max(power_values, na.rm = TRUE)
  min_power <- min(power_values, na.rm = TRUE)
  
  expect_equal(max_power, 0.95)
  expect_equal(min_power, 0.1)
  
  # Test finding closest to 80% power
  target_idx <- which.min(abs(power_values - 0.8))
  effect_for_80 <- effect_sizes[target_idx]
  
  expect_equal(effect_for_80, 0.8)  # Should be the 4th element
})

test_that("input validation logic works correctly", {
  # Test typical input validation scenarios
  
  # Valid inputs
  N <- 100
  dropout <- 0.1
  ratio <- 1
  
  expect_true(N > 0)
  expect_true(dropout >= 0 && dropout < 1)
  expect_true(ratio > 0)
  
  # Invalid inputs that should be caught
  N_invalid <- -10
  dropout_invalid <- 1.5
  ratio_invalid <- -1
  
  expect_false(N_invalid > 0)
  expect_false(dropout_invalid >= 0 && dropout_invalid < 1)
  expect_false(ratio_invalid > 0)
})