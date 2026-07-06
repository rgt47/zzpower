# Calculate Cohen's h from Odds Ratio

Convert odds ratio to Cohen's h given a baseline proportion

## Usage

``` r
or_to_cohens_h(or, baseline)
```

## Arguments

- or:

  Odds ratio (OR), range 0.1 to 10

- baseline:

  Baseline proportion (p2), range 0 to 1

## Value

Cohen's h effect size

## Examples

``` r
if (FALSE) { # \dontrun{
# If baseline is 50% and OR is 2
or_to_cohens_h(2, 0.5)
} # }
```
