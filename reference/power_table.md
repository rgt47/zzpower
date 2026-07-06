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

- ...:

  Additional design parameters passed through to the spec's
  \`sample_size_calc()\` (e.g. \`dropout\`, \`allocation\`, \`ratio\`,
  \`event_prob\`, \`n_groups\`) and \`standardize()\` (e.g. \`sd0\`,
  \`p2\`, \`baseline\`, \`sigma\`, \`correlation\`).

## Value

Tidy \`data.frame\` with columns \`effect_size\`, \`effect_size_std\`,
and one \`n_total_enrolled_at\_\<power\>\` column per power threshold
(plus \`n_total_evaluable_at\_\*\`).

## Examples

``` r
if (FALSE) { # \dontrun{
power_table("ttest_2groups",
            effect_grid = c(0.20, 0.50, 0.80),
            effect_method = "cohens_d",
            power_thresholds = c(0.80, 0.90))
} # }
```
