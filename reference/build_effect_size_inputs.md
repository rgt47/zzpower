# Build Effect Size Input Controls (Static)

Generates effect size method selection and range inputs at UI build
time. Method switching is handled entirely client-side via
conditionalPanel.

## Usage

``` r
build_effect_size_inputs(test_spec, ns)
```

## Arguments

- test_spec:

  Test specification from registry

- ns:

  Namespace function from NS(test_id)

## Value

Shiny tagList with effect size controls
