# Launch the zzpower Shiny Application

This function launches the interactive 'Shiny' application for power
analysis and sample size calculations for two-group parallel designs.

## Usage

``` r
launch_zzpower(..., launch.browser = TRUE, host = "127.0.0.1", port = NULL)
```

## Arguments

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)

- launch.browser:

  Logical, whether to launch the app in browser. Default is `TRUE`.

- host:

  Character string of IP address to listen on. Default is "127.0.0.1".

- port:

  Integer specifying the port to listen on. Default is `NULL` (random
  port).

## Value

No return value, launches the Shiny application

## Details

The application provides interactive power analysis for two-group
parallel clinical trial designs with the following features:

- Multiple effect size specifications (Cohen's d, percentage reduction,
  difference in change scores, treatment group change)

- Accounts for dropout rates and unequal group allocation ratios

- Interactive power curves and detailed results tables

- Downloadable reports in PDF, HTML, or Word formats

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the application
launch_zzpower()

# Launch on specific port
launch_zzpower(port = 3838)
} # }
```
