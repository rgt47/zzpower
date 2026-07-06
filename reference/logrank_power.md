# Log-rank Test Power (Schoenfeld Formula)

Computes power for the log-rank test comparing two survival curves. Uses
the Schoenfeld (1981) formula based on total expected events. Parameters
n1 and n2 represent expected events per group (not raw sample sizes), as
the event probability is applied in sample_size_calc before these values
reach this function.

## Usage

``` r
logrank_power(h, n1, n2, sig.level, alternative = "two.sided")
```

## Arguments

- h:

  Standardized effect size: log(hazard ratio)

- n1:

  Expected events in group 1

- n2:

  Expected events in group 2

- sig.level:

  Significance level

- alternative:

  "two.sided" or "one.sided"

## Value

List with `power` element

## References

Schoenfeld DA (1981). The asymptotic properties of nonparametric tests
for comparing survival distributions. Biometrika, 68(1), 316-319.
