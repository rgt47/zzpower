# Reshape the report_data list into a minimal calc_context

The Wave 3 reproducibility-script renderer (\`.render_repro_script\`)
consumes a calc_context. Reports already have all the same information
in the report_data list shape, so this helper just rewires the names.
Returns NULL if the report lacks the data needed (e.g. sample-size mode
with no closed-form sample_sizes).

## Usage

``` r
.report_data_to_ctx(report_data)
```
