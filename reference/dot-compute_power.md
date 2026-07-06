# Compose a power-function call for a single (test, design) pair

Mirrors the call assembly inside \`create_generic_test_server\`'s
\`.power_at_n()\` closure but takes plain values rather than Shiny
\`input\` references. Centralising the call shape here lets the
programmatic API (\`power_calc\`, \`power_table\`) and the Shiny server
share one engine.

## Usage

``` r
.compute_power(
  test_spec,
  sample_sizes,
  effect_size_std,
  alpha = 0.05,
  alternative = "two.sided"
)
```

## Arguments

- test_spec:

  A registry entry.

- sample_sizes:

  Output of the spec's \`sample_size_calc()\`.

- effect_size_std:

  Standardized effect size (Cohen's d/h/f or analogous).

- alpha:

  Type I error rate.

- alternative:

  \`"two.sided"\` or \`"one.sided"\`.

## Value

Numeric scalar power, or \`NA_real\_\` if the call errors.
