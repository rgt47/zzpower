# Sample-size sensitivity table for a test

Produces the Sec.2.1 Layout 1 sensitivity table: rows = effect-size
grid, columns = required N at each power threshold (typically 0.80 and
0.90). The artifact most often pasted directly into an NIH proposal.

## Usage

``` r
power_table(
  test,
  effect_grid = NULL,
  power_thresholds = c(0.8, 0.9),
  effect_method = NULL,
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

- effect_grid:

  Numeric vector of effect sizes on the native scale. If NULL, uses the
  spec's \`default_effect_grid\` for the chosen \`effect_method\`.

- power_thresholds:

  Numeric vector of power thresholds for the table columns. Defaults to
  \`c(0.80, 0.90)\`.

- effect_method:

  Character; one of \`test_spec\$effect_size_methods\`. Default is the
  first.

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

Tidy \`data.frame\` with columns \`effect_size\`, \`effect_size_std\`,
\`dropout\` (the dropout rate applied), and one
\`n_total_enrolled_at\_\<power\>\` column per power threshold (plus
\`n_total_evaluable_at\_\*\`). Enrolled N is inflated for dropout;
evaluable N equals enrolled N times \`(1 - dropout)\`.

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
# Default 10% dropout: enrolled N is inflated above evaluable N
power_table("ttest_2groups",
            effect_grid = c(0.20, 0.50, 0.80),
            effect_method = "cohens_d",
            power_thresholds = c(0.80, 0.90))

# No dropout adjustment
power_table("ttest_2groups",
            effect_grid = c(0.20, 0.50, 0.80),
            effect_method = "cohens_d",
            dropout = 0)
} # }
```
