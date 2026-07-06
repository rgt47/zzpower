# McNemar Test Power (Connor 1987)

Computes power for McNemar's test comparing paired binary outcomes using
the normal approximation from Connor (1987). The effect size h
represents the difference in discordant proportions (p10 - p01), and n
is the total number of pairs. The overall discordant rate (p10 + p01) is
folded into the standardized effect size by the spec's standardize
function.

## Usage

``` r
mcnemar_power(n, d, sig.level, alternative = "two.sided")
```

## Arguments

- n:

  Number of pairs

- d:

  Standardized effect size: \|p10 - p01\| / sqrt(p_disc - (p10-p01)^2)

- sig.level:

  Significance level

- alternative:

  "two.sided" or "one.sided"

## Value

List with `power` element

## References

Connor RJ (1987). Sample size for testing differences in proportions for
the paired-sample design. Biometrics, 43(1), 207-211.
