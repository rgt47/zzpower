# Build Design Parameter Inputs (for Sample Size Mode)

Shows design parameters (allocation, dropout, event probability, etc.)
but excludes sample_size since that is the solve target. Input IDs use a
"ss\_" prefix to avoid collisions with the power mode inputs.

## Usage

``` r
build_design_inputs(test_spec, ns)
```

## Arguments

- test_spec:

  Test specification from registry

- ns:

  Namespace function from NS(test_id)

## Value

Shiny tagList with design controls
