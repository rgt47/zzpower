# Cochran-Armitage Trend Test Power

Computes power for the Cochran-Armitage test for linear trend in
proportions across ordered groups. Uses the chi-square approximation
with 1 degree of freedom. The standardized effect size d encodes the
trend strength such that the non-centrality parameter is n \* d^2.

## Usage

``` r
trend_power(n, d, sig.level, alternative = "two.sided")
```

## Arguments

- n:

  Total sample size across all dose groups

- d:

  Standardized trend effect size

- sig.level:

  Significance level

- alternative:

  "two.sided" or "one.sided"

## Value

List with `power` element

## References

Cochran WG (1954). Some methods for strengthening the common chi-squared
tests. Biometrics, 10(4), 417-451.

Armitage P (1955). Tests for linear trends in proportions and
frequencies. Biometrics, 11(3), 375-386.
