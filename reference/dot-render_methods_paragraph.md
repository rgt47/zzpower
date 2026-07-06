# Render a methods-section paragraph from a calc_context

Produces the Glueck-Muller-shaped paragraph that pastes into an NIH
proposal's Statistical Design and Power attachment or an ICH E9 Sec. 3.5
sample-size statement. Composes seven sentences: (1) test + outcome, (2)
effect-size assumption with citation, (3) alpha + power + required N,
(4) dropout inflation, (5) sensitivity sentence (ICH E9 Sec. 3.5; if
\`sensitivity_factor\` \< 1), (6) software citation, (7)
sex-as-biological-variable paragraph (NIH rigor; if
\`include_sex_paragraph\`).

## Usage

``` r
.render_methods_paragraph(ctx)
```

## Arguments

- ctx:

  A \`calc_context\` returned by \`power_calc()\` or
  \`.build_calc_context()\`.

## Value

A single character string, paragraph-shaped (no embedded newlines).

## Details

Each \`calc_context\` field is consumed at most once so the function is
safe to call repeatedly. Sentences for which the relevant field is
missing are dropped silently rather than producing "(NA)" placeholders
in the output.
