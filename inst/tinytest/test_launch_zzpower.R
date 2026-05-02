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


# Test internal functions exist
expect_true(exists("create_ui"))
expect_true(exists("create_server"))

# Test they return appropriate objects
ui_obj <- create_ui()
server_obj <- create_server()

expect_inherits(ui_obj, "bslib_page")
expect_true(is.function(server_obj))


# Test UI component functions exist and return tagList objects
expect_true(exists("create_sample_size_inputs"))
expect_true(exists("create_effect_size_inputs")) 
expect_true(exists("create_advanced_settings"))

sample_inputs <- create_sample_size_inputs()
effect_inputs <- create_effect_size_inputs()
advanced_inputs <- create_advanced_settings()

expect_inherits(sample_inputs, "shiny.tag.list")
expect_inherits(effect_inputs, "shiny.tag.list")
expect_inherits(advanced_inputs, "shiny.tag.list")


# Test that required packages can be loaded
expect_true(requireNamespace("shiny", quietly = TRUE))
expect_true(requireNamespace("bslib", quietly = TRUE))
expect_true(requireNamespace("bsicons", quietly = TRUE))
expect_true(requireNamespace("pwr", quietly = TRUE))
expect_true(requireNamespace("ggplot2", quietly = TRUE))
expect_true(requireNamespace("DT", quietly = TRUE))
