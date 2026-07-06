# Render a runnable reproducibility R script

Emits a paste-ready block of R code that, run in a fresh R session with
the relevant package(s) installed, exactly reproduces the headline N
(sample-size mode) or achieved power (power mode) for the calc_context.
NIMH item 4 ("not sufficient to merely cite the software") made literal.

## Usage

``` r
.render_repro_script(ctx, fence = FALSE)
```

## Arguments

- ctx:

  A calc_context returned by \`power_calc()\`.

- fence:

  Logical; wrap output in a Markdown “\`r fence. Default \`FALSE\`
  returns plain R lines for embedding into a text report; \`TRUE\` is
  appropriate for the markdown / HTML reports.

## Value

A single character string with embedded newlines.

## Details

For pwr-backed tests the script calls \`pwr::pwr.\<test\>(...)\`
directly with literal numeric arguments. For zzpower's custom helpers
(logrank_power, mcnemar_power, mixed_model_power, trend_power) the
script calls \`zzpower::\<helper\>(...)\` – those are exported precisely
so this script works.
