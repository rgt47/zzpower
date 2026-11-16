# Look up a test's accent color with a fallback

\`colors\[id\] \`NA\` (not \`NULL\`) for a missing name. This helper
checks membership explicitly so a test id absent from the palette gets
\`default\` instead of an \`NA\` color in the generated CSS.

## Usage

``` r
.card_color(colors, id, default)
```

## Arguments

- colors:

  Named character vector of colors keyed by test id.

- id:

  Character scalar test id.

- default:

  Color returned when \`id\` is not in \`names(colors)\` or its entry is
  \`NA\`.

## Value

A single unnamed character color.
