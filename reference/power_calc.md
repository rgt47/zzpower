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
  dropout = 0.1,
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

- dropout:

  Expected dropout rate in \`\[0, 1)\`. Defaults to \`0.1\` (10
  \`dropout = 0\` to disable the adjustment. See the section 'Dropout
  adjustment'.

- ...:

  Additional design parameters passed through to the spec's
  \`sample_size_calc()\` (e.g. \`allocation\`, \`ratio\`,
  \`event_prob\`, \`n_groups\`) and \`standardize()\` (e.g. \`sd0\`,
  \`p2\`, \`baseline\`, \`sigma\`, \`correlation\`).

## Value

A \`calc_context\` object: a named list of class \`c("calc_context",
"list")\` (see \`.build_calc_context\`). Its top-level \`dropout\` field
is the dropout rate applied, \`n_effective\` is the total evaluable
(post-dropout) N at which power was computed, and \`n_enrolled\` is the
total enrolled N.

## Dropout adjustment

Every registered test applies an expected dropout rate, which defaults
to 10 enrolled counts, and power is computed at the evaluable (analyzed)
sample size, \`n_evaluable = n_enrolled \* (1 - dropout)\`, applied per
arm.

\* Power mode (\`sample_size\` supplied): the achieved power is the
power at \`sample_size \* (1 - dropout)\` evaluable participants, so it
is lower than the power that \`pwr\` reports for the same nominal N. For
example, a two-sample t-test with 128 enrolled participants and d = 0.5
has power 0.758 under the default and 0.801 with \`dropout = 0\` (the
\`pwr::pwr.t2n.test()\` value for 64 per arm). \* Sample-size mode
(\`target_power\` supplied): the solved N is the enrolled N, inflated so
that the evaluable N reaches the target power.

Set \`dropout = 0\` to reproduce unadjusted textbook or \`pwr\` results.
The rate actually applied is recorded in the returned object
(\`dropout\`, \`n_effective\`, \`n_enrolled\`) and is stated in its
print method, the methods paragraph, the reproducibility script and the
downloadable reports. The exported engine helpers (\`logrank_power()\`,
\`mcnemar_power()\`, \`mixed_model_power()\`, \`trend_power()\`) take
the analyzed sample size directly and do not apply any dropout
adjustment.

## Examples

``` r
if (FALSE) { # \dontrun{
# Power at fixed N (default 10% dropout: 115.2 evaluable of 128)
power_calc("ttest_2groups",
           sample_size = 128, effect_size = 0.5,
           effect_method = "cohens_d")

# Same design without the dropout adjustment (matches pwr)
power_calc("ttest_2groups",
           sample_size = 128, effect_size = 0.5,
           effect_method = "cohens_d", dropout = 0)

# Required N at target power
power_calc("ttest_2groups",
           target_power = 0.80, effect_size = 0.5,
           effect_method = "cohens_d")
} # }
```
