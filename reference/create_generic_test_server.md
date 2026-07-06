# Create Module Server for a Power Analysis Test

Registers all reactive expressions and output renderers for a single
test tab via moduleServer(). Input IDs are automatically scoped by the
module namespace.

## Usage

``` r
create_generic_test_server(
  id,
  test_spec,
  registry_func = get_power_test_registry
)
```

## Arguments

- id:

  Module ID (must match the test_id used in the UI)

- test_spec:

  Test specification from registry

- registry_func:

  Function returning the full test registry

## Value

The return value of moduleServer() (called for side effects)
