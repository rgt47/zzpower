# Calculate Cohen's h from Relative Risk

Convert relative risk to Cohen's h given a baseline proportion

## Usage

``` r
rr_to_cohens_h(rr, baseline)
```

## Arguments

- rr:

  Relative risk (RR), range 0.1 to 10

- baseline:

  Baseline proportion (p2), range 0 to 1

## Value

Cohen's h effect size

## Examples

``` r
if (FALSE) { # \dontrun{
# If baseline is 50% and RR is 1.5
rr_to_cohens_h(1.5, 0.5)
} # }
```
