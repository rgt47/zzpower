# Dropout statement lines for the downloadable reports

Resolves the dropout rate in effect for a report (explicit
\`report_data\$dropout\`, else the canonical sample-size record, else
the power-mode parameters, else the 10 returns plain-text lines
describing it, so no report omits the dropout assumption.

## Usage

``` r
.report_dropout_info(report_data)
```

## Arguments

- report_data:

  Report data list built by the download handler.

## Value

A list with \`rate\` (numeric) and \`lines\` (character).
