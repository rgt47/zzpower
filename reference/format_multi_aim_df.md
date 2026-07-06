# Format a multi-aim study as a tidy data frame

Produces the Sec.2.5 Layout 4 study-level table: one row per aim with
Outcome, Test, Effect size, Alpha, Power, N evaluable, N enrolled, and a
\`binding\` flag identifying the row with the largest enrolled N (the
aim that drives the overall study size).

## Usage

``` r
format_multi_aim_df(study)
```

## Arguments

- study:

  A \`multi_aim_study\`.

## Value

A data frame.
