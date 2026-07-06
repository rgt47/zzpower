# Build Sample Size Input Controls (Static)

Generates input controls at UI build time from test specification.
Conditional inputs use conditionalPanel (client-side JS) to avoid
server-side renderUI loops.

## Usage

``` r
build_sample_size_inputs(test_spec, ns)
```

## Arguments

- test_spec:

  Test specification from registry

- ns:

  Namespace function from NS(test_id)

## Value

Shiny tagList with input controls
