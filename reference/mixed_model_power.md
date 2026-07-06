# Longitudinal Mixed Model Power (Diggle et al. 2002)

Computes power for a two-group comparison of slopes in a linear mixed
model with equally spaced time points and compound symmetry correlation.
Based on the formula from Diggle et al. (2002, p. 29).

## Usage

``` r
mixed_model_power(n, d, sig.level, alternative = "two.sided")
```

## Arguments

- n:

  Per-group sample size

- d:

  Standardized slope effect size: delta \* sqrt(S_tt / (2 \* sigma^2 \*
  (1 - rho)))

- sig.level:

  Significance level

- alternative:

  "two.sided" or "one.sided"

## Value

List with `power` element

## Details

The standardized effect size d incorporates the slope difference,
residual variance, within-subject correlation, and number of time
points. The parameter n represents the per-group sample size.

## References

Diggle PJ, Heagerty P, Liang K-Y, Zeger SL (2002). Analysis of
Longitudinal Data, 2nd ed. Oxford University Press, p. 29.
