#' Generate Power Analysis Report
#'
#' Creates a formatted report of power analysis results in multiple formats.
#'
#' @param input Shiny input object
#' @param power_results Reactive data frame with power calculation results
#' @param study_parameters Reactive list with study parameters
#' @param format Report format ("text", "html", or "pdf")
#'
#' @return Character string containing the formatted report
#'
#' @keywords internal
generate_power_report <- function(input, power_results, study_parameters, format = "text") {

  consts <- ZZPOWER_CONSTANTS
  results <- power_results()
  params <- study_parameters()

  # Calculate key metrics
  power_target_idx <- which.min(abs(results$power - consts$POWER_TARGET))
  power_target_es <- if (length(power_target_idx) > 0) {
    results$effect_size[power_target_idx]
  } else {
    NA
  }

  # Create report content structure
  report_data <- list(
    metadata = list(
      title = "Power Analysis Report",
      generated = Sys.time(),
      package_version = utils::packageVersion("zzpower"),
      r_version = R.version$version.string,
      user = Sys.getenv("USER", "Unknown")
    ),

    study_design = list(
      total_n = input$N,
      dropout_rate = input$dropout %||% consts$DROPOUT_DEFAULT,
      dropin_rate = input$dropin %||% consts$DROPIN_DEFAULT,
      allocation_ratio = input$ratio %||% consts$RATIO_DEFAULT,
      effect_size_method = input$dmeth,
      type1_error = input$type1 %||% consts$TYPE1_DEFAULT,
      test_type = if (input$onesided) "One-sided" else "Two-sided"
    ),

    sample_sizes = list(
      n1_itt = round(params$n1_itt),
      n2_itt = round(params$n2_itt),
      n1_completer = round(params$n1_comp),
      n2_completer = round(params$n2_comp)
    ),

    power_results = list(
      effect_size_range = c(min(results$effect_size), max(results$effect_size)),
      power_range = c(min(results$power, na.rm = TRUE),
                      max(results$power, na.rm = TRUE)),
      power_target_effect_size = power_target_es,
      power_target = consts$POWER_TARGET
    ),

    raw_data = results
  )

  switch(format,
    "text" = .format_text_report(report_data),
    "html" = .format_html_report(report_data),
    "pdf" = .format_html_report(report_data),  # Use HTML for now
    .format_text_report(report_data)  # Default to text
  )
}

#' Format Report as Plain Text
#' @keywords internal
.format_text_report <- function(report_data) {
  meta <- report_data$metadata
  design <- report_data$study_design
  ss <- report_data$sample_sizes
  pr <- report_data$power_results

  paste0(
    "================================================================================\n",
    "POWER ANALYSIS REPORT\n",
    "================================================================================\n\n",

    "METADATA\n",
    "--------\n",
    sprintf("Generated: %s\n", format(meta$generated, "%Y-%m-%d %H:%M:%S")),
    sprintf("zzpower Version: %s\n", as.character(meta$package_version)),
    sprintf("R Version: %s\n", meta$r_version),
    sprintf("User: %s\n\n", meta$user),

    "STUDY DESIGN PARAMETERS\n",
    "-----------------------\n",
    sprintf("Total Sample Size: %d\n", design$total_n),
    sprintf("Dropout Rate: %.1f%%\n", design$dropout_rate * 100),
    sprintf("Drop-in Rate: %.1f%%\n", design$dropin_rate * 100),
    sprintf("Allocation Ratio (Active:Control): %.2f\n", design$allocation_ratio),
    sprintf("Effect Size Method: %s\n", design$effect_size_method),
    sprintf("Type I Error (alpha): %.4f\n", design$type1_error),
    sprintf("Test Type: %s\n\n", design$test_type),

    "SAMPLE SIZES\n",
    "------------\n",
    sprintf("Intention-to-Treat (ITT):\n"),
    sprintf("  Active Group: %d\n", ss$n1_itt),
    sprintf("  Control Group: %d\n", ss$n2_itt),
    sprintf("Completer Analysis:\n"),
    sprintf("  Active Group: %d\n", ss$n1_completer),
    sprintf("  Control Group: %d\n\n", ss$n2_completer),

    "POWER ANALYSIS RESULTS\n",
    "---------------------\n",
    sprintf("Effect Size Range: %.3f to %.3f\n",
            pr$effect_size_range[1], pr$effect_size_range[2]),
    sprintf("Power Range: %.3f to %.3f\n",
            pr$power_range[1], pr$power_range[2]),
    sprintf("Effect Size for %.0f%% Power: %.3f\n",
            pr$power_target * 100, pr$power_target_effect_size),
    "\n",

    "DETAILED RESULTS TABLE\n",
    "---------------------\n",
    "Effect Size, Power\n",
    paste(
      sprintf("%.3f, %.3f",
              report_data$raw_data$effect_size,
              report_data$raw_data$power),
      collapse = "\n"
    ),
    "\n\n",

    "================================================================================\n",
    "This report was generated with the zzpower R package.\n",
    "For questions or issues, visit: https://github.com/rythomas/zzpower/issues\n",
    "================================================================================\n"
  )
}

#' Format Report as HTML
#' @keywords internal
.format_html_report <- function(report_data) {
  meta <- report_data$metadata
  design <- report_data$study_design
  ss <- report_data$sample_sizes
  pr <- report_data$power_results

  sprintf(
    "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>Power Analysis Report</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      max-width: 900px;
      margin: 0 auto;
      padding: 20px;
      line-height: 1.6;
      color: #333;
      background-color: #f5f5f5;
    }
    .container {
      background-color: white;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h1 {
      color: #2c3e50;
      border-bottom: 3px solid #2c3e50;
      padding-bottom: 15px;
      margin-bottom: 30px;
    }
    h2 {
      color: #34495e;
      margin-top: 30px;
      margin-bottom: 15px;
      border-left: 4px solid #3498db;
      padding-left: 10px;
    }
    .metadata {
      background-color: #ecf0f1;
      padding: 15px;
      border-radius: 5px;
      margin-bottom: 20px;
      font-size: 14px;
    }
    table {
      border-collapse: collapse;
      width: 100%%
      margin: 20px 0;
    }
    th, td {
      border: 1px solid #bdc3c7;
      padding: 12px;
      text-align: left;
    }
    th {
      background-color: #34495e;
      color: white;
      font-weight: bold;
    }
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    tr:hover {
      background-color: #f0f0f0;
    }
    .footer {
      margin-top: 30px;
      padding-top: 20px;
      border-top: 1px solid #bdc3c7;
      font-size: 12px;
      color: #666;
    }
    .results-summary {
      background-color: #e8f4f8;
      padding: 15px;
      border-left: 4px solid #3498db;
      margin: 20px 0;
    }
  </style>
</head>
<body>
  <div class='container'>
    <h1>Power Analysis Report</h1>

    <div class='metadata'>
      <p><strong>Generated:</strong> %s</p>
      <p><strong>zzpower Version:</strong> %s</p>
      <p><strong>R Version:</strong> %s</p>
    </div>

    <h2>Study Design Parameters</h2>
    <table>
      <tr><th>Parameter</th><th>Value</th></tr>
      <tr><td>Total Sample Size</td><td>%d</td></tr>
      <tr><td>Dropout Rate</td><td>%.1f%%</td></tr>
      <tr><td>Drop-in Rate</td><td>%.1f%%</td></tr>
      <tr><td>Allocation Ratio (Active:Control)</td><td>%.2f</td></tr>
      <tr><td>Effect Size Method</td><td>%s</td></tr>
      <tr><td>Type I Error (alpha)</td><td>%.4f</td></tr>
      <tr><td>Test Type</td><td>%s</td></tr>
    </table>

    <h2>Sample Sizes</h2>
    <table>
      <tr><th colspan='2'></th><th>Active Group</th><th>Control Group</th></tr>
      <tr><td>Intention-to-Treat (ITT)</td><td></td><td>%d</td><td>%d</td></tr>
      <tr><td>Completer Analysis</td><td></td><td>%d</td><td>%d</td></tr>
    </table>

    <h2>Power Analysis Results</h2>
    <div class='results-summary'>
      <p><strong>Effect Size Range:</strong> %.3f to %.3f</p>
      <p><strong>Power Range:</strong> %.3f to %.3f</p>
      <p><strong>Effect Size for %.0f%% Power:</strong> %.3f</p>
    </div>

    <h2>Detailed Results</h2>
    <table>
      <tr><th>Effect Size</th><th>Power</th></tr>
      %s
    </table>

    <div class='footer'>
      <p>This report was generated with the <strong>zzpower</strong> R package.</p>
      <p>For questions or issues, visit: <a href='https://github.com/rythomas/zzpower'>https://github.com/rythomas/zzpower</a></p>
    </div>
  </div>
</body>
</html>",
    format(meta$generated, "%Y-%m-%d %H:%M:%S"),
    as.character(meta$package_version),
    meta$r_version,
    design$total_n,
    design$dropout_rate * 100,
    design$dropin_rate * 100,
    design$allocation_ratio,
    design$effect_size_method,
    design$type1_error,
    design$test_type,
    ss$n1_itt, ss$n2_itt,
    ss$n1_completer, ss$n2_completer,
    pr$effect_size_range[1], pr$effect_size_range[2],
    pr$power_range[1], pr$power_range[2],
    pr$power_target * 100, pr$power_target_effect_size,
    paste(
      sprintf("<tr><td>%.3f</td><td>%.3f</td></tr>",
              report_data$raw_data$effect_size,
              report_data$raw_data$power),
      collapse = "\n      "
    )
  )
}

#' Generate Power Analysis Report for Two Proportions
#'
#' Creates a formatted report of two-proportions power analysis results.
#'
#' @param input Shiny input object
#' @param power_results Reactive data frame with power calculation results
#' @param study_parameters Reactive list with study parameters
#' @param format Report format ("text" or "html")
#'
#' @return Character string containing the formatted report
#'
#' @keywords internal
generate_proportions_report <- function(input, power_results, study_parameters, format = "text") {

  consts <- ZZPOWER_CONSTANTS
  results <- power_results()
  params <- study_parameters()

  # Calculate key metrics
  power_target_idx <- which.min(abs(results$power - consts$POWER_TARGET))
  power_target_es <- if (length(power_target_idx) > 0) {
    results$effect_size[power_target_idx]
  } else {
    NA
  }

  # Create report content
  method <- input$prop_dmeth
  type1_val <- input$prop_type1 %||% consts$TYPE1_DEFAULT
  onesided_val <- input$prop_onesided %||% FALSE

  report_data <- list(
    metadata = list(
      title = "Power Analysis Report: Two Proportions",
      generated = Sys.time(),
      package_version = utils::packageVersion("zzpower"),
      r_version = R.version$version.string,
      user = Sys.getenv("USER", "Unknown")
    ),

    study_design = list(
      test_type = "Two-sample proportion comparison (pwr.2p2n.test)",
      total_n = round(params$total_n),
      n1 = round(params$n1),
      n2 = round(params$n2),
      effect_size_method = method,
      type1_error = type1_val,
      test_direction = ifelse(onesided_val, "One-sided", "Two-sided")
    ),

    power_results = list(
      effect_size_range = c(min(results$effect_size), max(results$effect_size)),
      power_range = c(min(results$power, na.rm = TRUE),
                      max(results$power, na.rm = TRUE)),
      power_target_effect_size = power_target_es,
      power_target = consts$POWER_TARGET
    ),

    raw_data = results
  )

  switch(format,
    "text" = .format_prop_text_report(report_data),
    "html" = .format_prop_html_report(report_data),
    .format_prop_text_report(report_data)
  )
}

#' Format Two Proportions Report as Text
#' @keywords internal
.format_prop_text_report <- function(report_data) {
  meta <- report_data$metadata
  design <- report_data$study_design
  pr <- report_data$power_results

  paste0(
    "================================================================================\n",
    "POWER ANALYSIS REPORT: TWO PROPORTIONS COMPARISON\n",
    "================================================================================\n\n",

    "METADATA\n",
    "--------\n",
    sprintf("Generated: %s\n", format(meta$generated, "%Y-%m-%d %H:%M:%S")),
    sprintf("zzpower Version: %s\n", as.character(meta$package_version)),
    sprintf("R Version: %s\n", meta$r_version),
    sprintf("User: %s\n\n", meta$user),

    "TEST SPECIFICATIONS\n",
    "-------------------\n",
    sprintf("Statistical Test: %s\n", design$test_type),
    sprintf("Effect Size Method: %s\n", design$effect_size_method),
    sprintf("Type I Error (alpha): %.4f\n", design$type1_error),
    sprintf("Test Direction: %s\n\n", design$test_direction),

    "SAMPLE SIZES\n",
    "------------\n",
    sprintf("Group 1: %d\n", design$n1),
    sprintf("Group 2: %d\n", design$n2),
    sprintf("Total: %d\n\n", design$total_n),

    "POWER ANALYSIS RESULTS\n",
    "---------------------\n",
    sprintf("Effect Size Range: %.3f to %.3f\n",
            pr$effect_size_range[1], pr$effect_size_range[2]),
    sprintf("Power Range: %.3f to %.3f\n",
            pr$power_range[1], pr$power_range[2]),
    sprintf("Effect Size for %.0f%% Power: %.3f\n",
            pr$power_target * 100, pr$power_target_effect_size),
    "\n",

    "DETAILED RESULTS TABLE\n",
    "---------------------\n",
    "Effect Size, Cohen's h, Power\n",
    paste(
      sprintf("%.3f, %.3f, %.3f",
              report_data$raw_data$effect_size,
              report_data$raw_data$cohens_h,
              report_data$raw_data$power),
      collapse = "\n"
    ),
    "\n\n",

    "NOTES\n",
    "-----\n",
    "- Cohen's h is the standardized effect size used for power calculations\n",
    "- Power calculations use pwr::pwr.2p2n.test for unequal sample sizes\n",
    "- Effect sizes shown in original scale of selected method\n",
    "\n",

    "================================================================================\n",
    "This report was generated with the zzpower R package.\n",
    "For questions or issues, visit: https://github.com/rythomas/zzpower/issues\n",
    "================================================================================\n"
  )
}

#' Format Two Proportions Report as HTML
#' @keywords internal
.format_prop_html_report <- function(report_data) {
  meta <- report_data$metadata
  design <- report_data$study_design
  pr <- report_data$power_results

  sprintf(
    "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>Two Proportions Power Analysis Report</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      max-width: 900px;
      margin: 0 auto;
      padding: 20px;
      line-height: 1.6;
      color: #333;
      background-color: #f5f5f5;
    }
    .container {
      background-color: white;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h1 {
      color: #2c3e50;
      border-bottom: 3px solid #2c3e50;
      padding-bottom: 15px;
      margin-bottom: 30px;
    }
    h2 {
      color: #34495e;
      margin-top: 30px;
      margin-bottom: 15px;
      border-left: 4px solid #3498db;
      padding-left: 10px;
    }
    .metadata {
      background-color: #ecf0f1;
      padding: 15px;
      border-radius: 5px;
      margin-bottom: 20px;
      font-size: 14px;
    }
    table {
      border-collapse: collapse;
      width: 100%%;
      margin: 20px 0;
    }
    th, td {
      border: 1px solid #bdc3c7;
      padding: 12px;
      text-align: left;
    }
    th {
      background-color: #34495e;
      color: white;
      font-weight: bold;
    }
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    tr:hover {
      background-color: #f0f0f0;
    }
    .results-summary {
      background-color: #e8f4f8;
      padding: 15px;
      border-left: 4px solid #3498db;
      margin: 20px 0;
    }
    .footer {
      margin-top: 30px;
      padding-top: 20px;
      border-top: 1px solid #bdc3c7;
      font-size: 12px;
      color: #666;
    }
  </style>
</head>
<body>
  <div class='container'>
    <h1>Power Analysis Report: Two Proportions Comparison</h1>

    <div class='metadata'>
      <p><strong>Generated:</strong> %s</p>
      <p><strong>zzpower Version:</strong> %s</p>
      <p><strong>R Version:</strong> %s</p>
    </div>

    <h2>Test Specifications</h2>
    <table>
      <tr><th>Parameter</th><th>Value</th></tr>
      <tr><td>Statistical Test</td><td>%s</td></tr>
      <tr><td>Effect Size Method</td><td>%s</td></tr>
      <tr><td>Type I Error (alpha)</td><td>%.4f</td></tr>
      <tr><td>Test Direction</td><td>%s</td></tr>
    </table>

    <h2>Sample Sizes</h2>
    <table>
      <tr><th>Group</th><th>Sample Size</th></tr>
      <tr><td>Group 1</td><td>%d</td></tr>
      <tr><td>Group 2</td><td>%d</td></tr>
      <tr><td>Total</td><td>%d</td></tr>
    </table>

    <h2>Power Analysis Results</h2>
    <div class='results-summary'>
      <p><strong>Effect Size Range:</strong> %.3f to %.3f</p>
      <p><strong>Power Range:</strong> %.3f to %.3f</p>
      <p><strong>Effect Size for %.0f%% Power:</strong> %.3f</p>
    </div>

    <h2>Detailed Results</h2>
    <table>
      <tr><th>Effect Size</th><th>Cohen's h</th><th>Power</th></tr>
      %s
    </table>

    <div class='footer'>
      <p><strong>Notes:</strong></p>
      <ul>
        <li>Cohen's h is the standardized effect size used for power calculations</li>
        <li>Power calculations use pwr::pwr.2p2n.test for unequal sample sizes</li>
        <li>Effect sizes shown in original scale of selected method</li>
      </ul>
      <p>This report was generated with the <strong>zzpower</strong> R package.</p>
      <p>For questions or issues, visit: <a href='https://github.com/rythomas/zzpower'>https://github.com/rythomas/zzpower</a></p>
    </div>
  </div>
</body>
</html>",
    format(meta$generated, "%Y-%m-%d %H:%M:%S"),
    as.character(meta$package_version),
    meta$r_version,
    design$test_type,
    design$effect_size_method,
    design$type1_error,
    design$test_direction,
    design$n1, design$n2, design$total_n,
    pr$effect_size_range[1], pr$effect_size_range[2],
    pr$power_range[1], pr$power_range[2],
    pr$power_target * 100, pr$power_target_effect_size,
    paste(
      sprintf("<tr><td>%.3f</td><td>%.3f</td><td>%.3f</td></tr>",
              report_data$raw_data$effect_size,
              report_data$raw_data$cohens_h,
              report_data$raw_data$power),
      collapse = "\n      "
    )
  )
}
