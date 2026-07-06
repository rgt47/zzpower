# Calculate Cohen's h from Two Proportions

Cohen's h is the standardized difference between two proportions: h = 2
\* (arcsin(sqrt(p1)) - arcsin(sqrt(p2)))

## Usage

``` r
prop_to_cohens_h(p1, p2)
```

## Arguments

- p1:

  Proportion in group 1 (0 to 1)

- p2:

  Proportion in group 2 (0 to 1)

## Value

Cohen's h effect size

## Examples

``` r
if (FALSE) { # \dontrun{
prop_to_cohens_h(0.6, 0.4)  # Returns ~0.41
} # }
```
