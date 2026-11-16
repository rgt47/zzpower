# Validate the \`...\` of power_table()

\`power_table()\` forwards \`...\` to \`power_calc()\` as design
parameters. A \`sample_size\`, \`target_power\` or \`effect_size\`
supplied there (or an unnamed value, which lands positionally on
\`sample_size\`) would override the quantity the table solves for, so
every row would silently report the same N. Such arguments are rejected
with an informative error. Names are compared as prefixes because R
partially matches them to \`power_calc()\` formals.

## Usage

``` r
.check_power_table_dots(...)
```

## Arguments

- ...:

  The \`...\` of \`power_table()\`.

## Value

\`NULL\`, invisibly; called for its error side effect.
