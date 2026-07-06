# Launch the zzpower Shiny Application

This function launches the interactive 'Shiny' application for power
analysis and sample size calculations using a registry of statistical
tests. Each test runs as an independent Shiny module.

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

The application provides interactive power analysis for multiple
statistical tests:

- Two-group t-tests (independent samples)

- Paired t-tests

- One-sample t-tests

- Two proportions (binomial comparison)

- Correlation tests

- Survival log-rank test

- Fisher's exact test

- Cochran-Armitage trend in proportions

- Multiple effect size specifications per test

- Interactive power curves and detailed results tables

- Downloadable reports in text or HTML formats

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the application
launch_zzpower()

# Launch on specific port
launch_zzpower(port = 3838)
} # }
```
