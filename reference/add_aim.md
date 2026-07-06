# Append a calc_context to a multi-aim study

Append a calc_context to a multi-aim study

## Usage

``` r
add_aim(study, ctx, name = NULL, outcome = NULL)
```

## Arguments

- study:

  A \`multi_aim_study\` from \`multi_aim_study()\`.

- ctx:

  A \`calc_context\` from \`power_calc()\`.

- name:

  Aim label (e.g. "Aim 1", "Primary"). Defaults to "Aim N" where N is
  the next ordinal.

- outcome:

  Outcome description (e.g. "Toxicity at 3 months"). Defaults to the
  test description.

## Value

The study with the new aim appended.
