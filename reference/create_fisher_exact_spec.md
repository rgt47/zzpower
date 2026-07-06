# Fisher's Exact Test Specification

Power analysis for Fisher's exact test on 2x2 contingency tables. Uses
the normal approximation to the exact test (equivalent to pwr.2p2n.test
with Cohen's h). Appropriate for small-sample studies with binary
outcomes.

## Usage

``` r
create_fisher_exact_spec()
```
