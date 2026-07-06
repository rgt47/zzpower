# Survival Log-rank Test Specification

Power analysis for comparing two survival curves using the log-rank
test. Uses the Schoenfeld (1981) formula. The event probability
parameter converts total sample sizes to expected event counts before
power calculation.

## Usage

``` r
create_logrank_spec()
```
