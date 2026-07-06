# Programmatic power calculation

Compute power (or required N) for a single test from R, without
launching the Shiny app. The companion to \`power_table()\` for tabular
sweeps.

## Usage

``` r
power_calc(
  test,
  sample_size = NULL,
  effect_size,
  effect_method = NULL,
  target_power = NULL,
  alpha = 0.05,
  alternative = "two.sided",
  ...
)
```

## Arguments

- test:

  Character test id (one of \`names(get_power_test_registry())\`) or a
  test_spec list returned by the registry.

- sample_size:

  Total enrolled sample size. Optional in sample-size mode
  (\`target_power\` supplied) – left NULL the function solves for
  required N via bisection.

- effect_size:

  Effect size on the native scale of the chosen \`effect_method\` (e.g.
  raw difference for \`"difference"\`, hazard ratio for
  \`"hazard_ratio"\`, Cohen's d for \`"cohens_d"\`).

- effect_method:

  Character; one of \`test_spec\$effect_size_methods\`. Default is the
  first.

- target_power:

  Target power threshold. If supplied the function solves for required
  N; otherwise computes achieved power at \`sample_size\`.

- alpha:

  Type I error rate.

- alternative:

  \`"two.sided"\` or \`"one.sided"\`.

- ...:

  Additional design parameters passed through to the spec's
  \`sample_size_calc()\` (e.g. \`dropout\`, \`allocation\`, \`ratio\`,
  \`event_prob\`, \`n_groups\`) and \`standardize()\` (e.g. \`sd0\`,
  \`p2\`, \`baseline\`, \`sigma\`, \`correlation\`).

## Value

A \`calc_context\` named list (see \`.build_calc_context\`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Power at fixed N
power_calc("ttest_2groups",
           sample_size = 100, effect_size = 0.5,
           effect_method = "cohens_d")

# Required N at target power
power_calc("ttest_2groups",
           target_power = 0.80, effect_size = 0.5,
           effect_method = "cohens_d")
} # }
```
