# Generate Effect Size Range for Proportion Studies

Creates a sequence of Cohen's h values for power curve visualization
based on the specified effect size method.

## Usage

``` r
generate_prop_effect_sizes(
  method = "proportions",
  p1 = NULL,
  p2 = NULL,
  diff_min = NULL,
  diff_max = NULL,
  or_min = NULL,
  or_max = NULL,
  rr_min = NULL,
  rr_max = NULL,
  baseline = 0.5,
  n_points = 16
)
```

## Arguments

- method:

  Effect size method: "proportions", "difference", "or", or "rr"

- p1:

  Proportion in group 1 (for "proportions" method)

- p2:

  Proportion in group 2 (for "proportions" method)

- diff_min:

  Minimum proportion difference (for "difference" method)

- diff_max:

  Maximum proportion difference (for "difference" method)

- or_min:

  Minimum odds ratio (for "or" method)

- or_max:

  Maximum odds ratio (for "or" method)

- rr_min:

  Minimum relative risk (for "rr" method)

- rr_max:

  Maximum relative risk (for "rr" method)

- baseline:

  Baseline proportion (for "difference", "or", "rr" methods)

- n_points:

  Number of points in the sequence (default: 16)

## Value

List with components: - effect_sizes: Original effect size scale
values - cohens_h: Cohen's h equivalents for pwr.2p2n.test
