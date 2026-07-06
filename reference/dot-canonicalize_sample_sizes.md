# Canonicalize a sample-size calculation result

Converts per-arm enrolled counts plus a dropout rate into the canonical
four-layer record (\`n_per_arm_evaluable\`, \`n_per_arm_enrolled\`,
\`n_total_evaluable\`, \`n_total_enrolled\`) that every grant-writing
artifact (Gaps 1, 2, 3, 9) reads from. Back-compat scalars (\`n\`,
\`n1\`, \`n2\`, \`n1_itt\`, \`n2_itt\`, \`k\`) are populated so
consumers that read the legacy keys keep working unchanged.

## Usage

``` r
.canonicalize_sample_sizes(per_arm_enrolled, dropout = 0, arm_labels = NULL)
```

## Arguments

- per_arm_enrolled:

  Numeric vector of enrolled (pre-dropout) counts per arm. Length 1 for
  single-sample tests, 2 for two-arm tests, k for ANOVA-style designs.

- dropout:

  Expected dropout rate in \[0, 1).

- arm_labels:

  Optional character vector of arm names for display. Defaults to
  "Sample" for one arm, "Group i" otherwise.

## Value

Named list with the canonical fields plus legacy shims.
