# Test that the function exists
expect_true(exists("launch_zzpower"))

# Test that it's a function
expect_true(is.function(launch_zzpower))

# Test function arguments
args <- names(formals(launch_zzpower))
expect_true("..." %in% args)
expect_true("launch.browser" %in% args)
expect_true("host" %in% args)
expect_true("port" %in% args)

# Test that required packages can be loaded
expect_true(requireNamespace("shiny", quietly = TRUE))
expect_true(requireNamespace("bslib", quietly = TRUE))
expect_true(requireNamespace("bsicons", quietly = TRUE))
expect_true(requireNamespace("pwr", quietly = TRUE))
expect_true(requireNamespace("ggplot2", quietly = TRUE))
expect_true(requireNamespace("DT", quietly = TRUE))
