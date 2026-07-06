# Build a calc_context for a single power calculation

Bundles inputs, outputs, provenance, and metadata for one
power/sample-size calculation into a single named list. Every
grant-writing artifact (Gap 1 methods paragraph, Gap 2 table builder,
Gap 9 reproducibility script, Gap 3 multi-aim aggregator) reads its
values from a calc_context. Building the context once from inputs and
consuming it from many places replaces what would otherwise be eleven
specs x five output-paste sites.

## Usage

``` r
.build_calc_context(
  test_spec,
  sample_sizes,
  effect_size,
  effect_size_std = NA_real_,
  effect_method = NULL,
  effect_params = list(),
  target_power = NULL,
  achieved_power = NULL,
  alpha = 0.05,
  alternative = "two.sided",
  effect_source = "",
  effect_doi = "",
  sensitivity_factor = NULL,
  include_sex_paragraph = TRUE
)
```

## Arguments

- test_spec:

  A test spec (one of the elements of \`get_power_test_registry()\`).

- sample_sizes:

  Output of the spec's \`sample_size_calc()\`, already canonicalized via
  \`.canonicalize_sample_sizes()\`.

- effect_size:

  Numeric scalar effect size on the native scale of the chosen
  \`effect_method\`.

- effect_size_std:

  Numeric scalar effect size on the standardized scale (Cohen's d, h, f,
  or analogous).

- effect_method:

  Character string identifying which entry of
  \`test_spec\$effect_size_methods\` was selected.

- effect_params:

  Named list of method-dependent parameters (e.g. \`sd0\`, \`p2\`,
  \`baseline\`).

- target_power:

  Target power threshold (sample-size mode) or NULL (power mode).

- achieved_power:

  Achieved power at the proposed N (power mode) or NULL (sample-size
  mode).

- alpha:

  Type I error rate.

- alternative:

  \`"two.sided"\` or \`"one.sided"\`.

- effect_source, effect_doi:

  Provenance strings for the effect-size assumption (Gap 5). Default
  empty.

- sensitivity_factor:

  Numeric multiplier for the conservative-effect sentence in the methods
  paragraph (Gap 6).

- include_sex_paragraph:

  Logical; whether the sex-as-bio paragraph is appended (Gap 12).

## Value

Named list with the calc_context shape.

## Details

This is the structural shape consumed by the programmatic API
(\`power_calc()\`, Gap 10) and the Wave 2 generators. Field values
populated in earlier waves are treated as ground truth; fields populated
in later waves default to NULL until then.
