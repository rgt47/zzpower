# zzpower: Power Analysis Calculator for Two-Group Parallel Designs

The zzpower package provides an interactive 'Shiny' application for
power analysis and sample size calculations specifically designed for
two-group parallel clinical trial designs. The application supports
multiple effect size specifications, accounts for dropout rates and
unequal group allocation ratios, and provides interactive visualizations
with downloadable reports.

## Details

A comprehensive 'Shiny' application for conducting power analysis and
sample size calculations for two-group parallel clinical trial designs.

## Key Features

- **Multiple Effect Size Methods:** Supports Cohen's d, percentage
  reductions, difference in change scores, and treatment group changes

- **Comprehensive Design Considerations:** Accounts for dropout rates,
  drop-in rates, and unequal group allocation ratios

- **Interactive Visualizations:** Real-time power curves and detailed
  results tables

- **Report Generation:** Downloadable reports in PDF, HTML, or Word
  formats

- **Advanced Settings:** Configurable Type I error rates and one-sided
  vs two-sided testing

## Getting Started

To launch the interactive application, simply run:


    library(zzpower)
    launch_zzpower()

## Effect Size Methods

The application supports four different approaches to specifying effect
sizes:

- **Standard Deviation Units (Cohen's d):**:

  Direct specification of standardized effect sizes

- **Percent Reduction:**:

  Effect size specified as percentage reduction from placebo

- **Difference in Change Scores:**:

  Absolute difference between treatment groups in outcome units

- **Change in Active Group:**:

  Specifying the treatment group change directly

## Sample Size Calculations

The application calculates both intention-to-treat (ITT) and completer
sample sizes, accounting for:

- Expected dropout rates

- Drop-in rates (control group receiving treatment)

- Unequal randomization ratios between groups

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

## See also

- [`launch_zzpower`](https://rgt47.github.io/zzpower/reference/launch_zzpower.md) -
  Launch the interactive application

- [`pwr.t2n.test`](https://rdrr.io/pkg/pwr/man/pwr.t2n.test.html) -
  Underlying power calculation function

## Author

Ronald "Ryy" G. Thomas
