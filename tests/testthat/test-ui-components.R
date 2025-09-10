test_that("UI components generate proper HTML structure", {
  # Test main UI creation
  ui <- create_ui()
  expect_s3_class(ui, "bslib_page")
  expect_true("shiny.tag.list" %in% class(ui))
  
  # Test that UI contains expected elements
  ui_html <- as.character(ui)
  expect_true(grepl("Power Analysis Calculator", ui_html))
  expect_true(grepl("sidebar", ui_html))
})

test_that("sample size input components work correctly", {
  sample_inputs <- create_sample_size_inputs()
  expect_s3_class(sample_inputs, "shiny.tag.list")
  
  # Convert to HTML and check for expected elements
  html_output <- as.character(sample_inputs)
  
  # Should contain slider inputs
  expect_true(grepl("Total Sample Size", html_output))
  expect_true(grepl("Dropout Rate", html_output))
  expect_true(grepl('id="N"', html_output))
  expect_true(grepl('id="dropout"', html_output))
  
  # Should contain HTML output for sample size display
  expect_true(grepl("sample_size_display", html_output))
})

test_that("effect size input components work correctly", {
  effect_inputs <- create_effect_size_inputs()
  expect_s3_class(effect_inputs, "shiny.tag.list")
  
  html_output <- as.character(effect_inputs)
  
  # Should contain radio buttons for method selection
  expect_true(grepl('id="dmeth"', html_output))
  expect_true(grepl("Standard Deviation Units", html_output))
  expect_true(grepl("Percent Reduction", html_output))
  expect_true(grepl("Difference in Change Scores", html_output))
  expect_true(grepl("Change in Active Group", html_output))
  
  # Should contain conditional panels
  expect_true(grepl("conditionalPanel", html_output))
  expect_true(grepl('input.dmeth == \'std\'', html_output))
  expect_true(grepl('input.dmeth == \'pct\'', html_output))
  expect_true(grepl('input.dmeth == \'diff\'', html_output))
  expect_true(grepl('input.dmeth == \'active\'', html_output))
})

test_that("advanced settings components work correctly", {
  advanced_inputs <- create_advanced_settings()
  expect_s3_class(advanced_inputs, "shiny.tag.list")
  
  html_output <- as.character(advanced_inputs)
  
  # Should contain checkbox for showing advanced settings
  expect_true(grepl("Show Advanced Settings", html_output))
  expect_true(grepl('id="show_advanced"', html_output))
  
  # Should contain conditional panel with advanced options
  expect_true(grepl('input.show_advanced == true', html_output))
  expect_true(grepl("Active to Control Group Ratio", html_output))
  expect_true(grepl("Drop-in Rate", html_output))
  expect_true(grepl("Type I Error Rate", html_output))
  expect_true(grepl("One-sided Test", html_output))
})

test_that("UI components have proper input IDs", {
  # Test that all expected input IDs are present in the UI
  ui <- create_ui()
  ui_html <- as.character(ui)
  
  # Sample size inputs
  expect_true(grepl('id="N"', ui_html))
  expect_true(grepl('id="dropout"', ui_html))
  
  # Effect size method inputs
  expect_true(grepl('id="dmeth"', ui_html))
  expect_true(grepl('id="del"', ui_html))
  expect_true(grepl('id="dff"', ui_html))
  expect_true(grepl('id="pct"', ui_html))
  expect_true(grepl('id="active"', ui_html))
  expect_true(grepl('id="sd0"', ui_html))
  expect_true(grepl('id="d0"', ui_html))
  
  # Advanced settings inputs
  expect_true(grepl('id="show_advanced"', ui_html))
  expect_true(grepl('id="ratio"', ui_html))
  expect_true(grepl('id="dropin"', ui_html))
  expect_true(grepl('id="type1"', ui_html))
  expect_true(grepl('id="onesided"', ui_html))
  
  # Output elements
  expect_true(grepl('id="power_plot"', ui_html))
  expect_true(grepl('id="results_table"', ui_html))
  expect_true(grepl('id="summary_text"', ui_html))
  expect_true(grepl('id="download_report"', ui_html))
})

test_that("UI components have proper CSS classes and attributes", {
  ui <- create_ui()
  ui_html <- as.character(ui)
  
  # Check for Bootstrap/bslib classes
  expect_true(grepl("card", ui_html))
  expect_true(grepl("sidebar", ui_html))
  
  # Check for proper input attributes
  sample_inputs <- create_sample_size_inputs()
  sample_html <- as.character(sample_inputs)
  
  # Sliders should have min, max, step attributes
  expect_true(grepl('data-min="20"', sample_html))
  expect_true(grepl('data-max="500"', sample_html))
  expect_true(grepl('data-step="10"', sample_html))
})

test_that("tooltips are properly created", {
  sample_inputs <- create_sample_size_inputs()
  sample_html <- as.character(sample_inputs)
  
  # Should contain tooltip elements
  expect_true(grepl("tooltip", sample_html) || grepl("data-bs-toggle", sample_html))
  
  # Should contain tooltip text content
  expect_true(grepl("Total number of participants", sample_html))
  expect_true(grepl("expected to withdraw", sample_html))
})

test_that("conditional panels have correct conditions", {
  effect_inputs <- create_effect_size_inputs()
  effect_html <- as.character(effect_inputs)
  
  # Check that conditional panel conditions are properly formatted
  expect_true(grepl("input\\.dmeth == 'std'", effect_html))
  expect_true(grepl("input\\.dmeth == 'diff'", effect_html))
  expect_true(grepl("input\\.dmeth == 'pct'", effect_html))
  expect_true(grepl("input\\.dmeth == 'active'", effect_html))
  
  # Advanced settings condition
  advanced_inputs <- create_advanced_settings()
  advanced_html <- as.character(advanced_inputs)
  expect_true(grepl("input\\.show_advanced == true", advanced_html))
})