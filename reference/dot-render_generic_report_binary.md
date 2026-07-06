# Render the markdown report to PDF or Word

For PDF, render via rmarkdown's xelatex pipeline.

## Usage

``` r
.render_generic_report_binary(report_data, test_spec, fmt)
```

## Value

Path to the rendered file, or \`NULL\` if no rendering backend is
installed or rendering fails.
