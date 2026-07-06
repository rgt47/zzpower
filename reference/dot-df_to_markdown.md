# Format a data frame as a markdown table

Minimal markdown-table formatter for the sensitivity-table download (Gap
2) and the multi-aim aggregator (Gap 3). Rounds numeric columns to a
reasonable number of significant digits for narrative use; leaves
character columns untouched.

## Usage

``` r
.df_to_markdown(df, caption = NULL)
```

## Arguments

- df:

  A data frame.

- caption:

  Optional caption rendered above the table.

## Value

A single character string with embedded newlines.
