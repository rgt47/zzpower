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
