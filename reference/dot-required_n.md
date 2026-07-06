# Solve for required N to achieve a target power

Bisection on the total sample-size slider domain. Mirrors
\`.find_required_n()\` inside the server module.

## Usage

``` r
.required_n(
  test_spec,
  target_power,
  effect_size_std,
  design_params = list(),
  alpha = 0.05,
  alternative = "two.sided",
  n_lo = 10,
  n_hi = 10000
)
```

## Arguments

- test_spec:

  A registry entry.

- target_power:

  Target power threshold (e.g. 0.80).

- effect_size_std:

  Standardized effect size.

- design_params:

  Named list of design parameters consumed by the spec's
  \`sample_size_calc()\` (e.g. \`dropout\`, \`allocation\`).

- alpha:

  Type I error rate.

- alternative:

  \`"two.sided"\` or \`"one.sided"\`.

- n_lo, n_hi:

  Bisection bounds on total enrolled sample size.

## Value

Smallest total enrolled N within \[n_lo, n_hi\] that achieves at least
\`target_power\`, or \`NA_real\_\` if not found.
