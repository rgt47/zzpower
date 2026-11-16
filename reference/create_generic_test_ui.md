# Create Module UI for a Power Analysis Test

Module UI function. Uses NS(id) for all input/output IDs. Inputs are
generated statically from the registry spec (no renderUI) to prevent
reactive invalidation loops.

## Usage

``` r
create_generic_test_ui(test_id)
```

## Arguments

- test_id:

  Module ID (must match a registry entry)

## Value

A bslib layout_sidebar suitable for embedding in a tab

## Details

The design-parameter controls include a Dropout Rate slider (default 10
evaluable sample size, \`n_enrolled \* (1 - dropout)\`; set the slider
to 0 to disable the adjustment. See \[power_calc()\], section 'Dropout
adjustment'.
