# Calculate Cohen's h from Proportion Difference

Convert proportion difference to Cohen's h given a baseline proportion

## Usage

``` r
diff_to_cohens_h(diff, baseline)
```

## Arguments

- diff:

  Proportion difference (p1 - p2), range -1 to 1

- baseline:

  Baseline proportion (p2), range 0 to 1

## Value

Cohen's h effect size

## Examples

``` r
if (FALSE) { # \dontrun{
# If baseline is 50% and difference is -20%, p1=30%, p2=50%
diff_to_cohens_h(-0.2, 0.5)  # Returns h for (0.3, 0.5)
} # }
```
