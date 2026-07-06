# zzpower: Interactive Power Analysis Calculator for Clinical Trials

The zzpower package provides an interactive 'Shiny' application covering
eleven statistical tests via a registry/plugin architecture: two-group,
paired, and one-sample t-tests; two-proportion comparisons; Pearson
correlation; log-rank survival; Fisher's exact test; Cochran-Armitage
trend; one-way ANOVA; McNemar's test for paired proportions; and a basic
linear mixed-model power calculation. Each test supports multiple
effect-size specifications, design parameters (alpha, allocation,
dropout), and downloadable reports.

## Details

A 'Shiny' application for power analysis and sample size calculations
across a catalogue of statistical tests common in clinical trial design.

## Key Features

- **Eleven statistical tests** via the
  [`get_power_test_registry`](https://rgt47.github.io/zzpower/reference/get_power_test_registry.md)
  registry, each rendered through a generic UI/server factory pair
  ([`create_generic_test_ui`](https://rgt47.github.io/zzpower/reference/create_generic_test_ui.md),
  [`create_generic_test_server`](https://rgt47.github.io/zzpower/reference/create_generic_test_server.md))

- **Multiple effect-size methods** per test (Cohen's d, h, f, hazard
  ratio, odds ratio, percent reduction, etc.)

- **Sample-size and power solve modes:** compute power at a given N or N
  required for target power

- **Interactive power curves** with reference lines and formatted result
  tables (`DT`)

- **Downloadable reports** (text and HTML) summarising study design and
  power calculation

- **Design considerations:** dropout, drop-in, allocation ratio,
  one-sided vs two-sided testing

## Getting Started

To launch the interactive application, run:


    library(zzpower)
    launch_zzpower()

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

Schoenfeld, D. A. (1983). Sample-size formula for the proportional-
hazards regression model. Biometrics, 39(2), 499-503.

Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package.

## See also

- [`launch_zzpower`](https://rgt47.github.io/zzpower/reference/launch_zzpower.md)
  for the interactive application

- [`get_power_test_registry`](https://rgt47.github.io/zzpower/reference/get_power_test_registry.md)
  for the test registry

- The pwr package for the underlying power-calculation primitives

## Author

Ronald G. Thomas
