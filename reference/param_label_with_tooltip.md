# Wrap a parameter label with an info-icon tooltip

If the parameter spec includes a \`description\`, the label is followed
by a small info-circle icon that shows the description on hover. If not,
the label is returned plain.

## Usage

``` r
param_label_with_tooltip(param_spec)
```

## Arguments

- param_spec:

  Named list from the registry: must contain \`label\` and may contain
  \`description\`.

## Value

A character or \`tagList\` suitable as the \`label\` argument of
\`shiny::sliderInput\`, \`shiny::numericInput\`, etc.
