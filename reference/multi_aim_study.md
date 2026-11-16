# Initialize an empty multi-aim study

A multi-aim study is a list of \`calc_context\` objects (one per
Specific Aim), plus optional metadata (study name, target agency). Use
\`add_aim()\` to append entries and \`format_multi_aim_df()\` /
\`multi_aim_markdown()\` / \`multi_aim_csv()\` to render the Sec.2.5
Layout 4 study-level table for an NIH proposal.

## Usage

``` r
multi_aim_study(study_name = NULL)
```

## Arguments

- study_name:

  Optional study identifier for the table caption (e.g. "TEST-001:
  Reducing Chemotherapy Toxicity").

## Value

A \`multi_aim_study\` object (a list with class).

## Details

Each aim's sample size reflects the dropout rate used in its
\`power_calc()\` call (10 \[power_calc()\]). The rendered table reports
that rate in a \`Dropout\` column alongside N evaluable and N enrolled.

## Dropout adjustment

Every registered test applies an expected dropout rate, which defaults
to 10 enrolled counts, and power is computed at the evaluable (analyzed)
sample size, \`n_evaluable = n_enrolled \* (1 - dropout)\`, applied per
arm.

\* Power mode (\`sample_size\` supplied): the achieved power is the
power at \`sample_size \* (1 - dropout)\` evaluable participants, so it
is lower than the power that \`pwr\` reports for the same nominal N. For
example, a two-sample t-test with 128 enrolled participants and d = 0.5
has power 0.758 under the default and 0.801 with \`dropout = 0\` (the
\`pwr::pwr.t2n.test()\` value for 64 per arm). \* Sample-size mode
(\`target_power\` supplied): the solved N is the enrolled N, inflated so
that the evaluable N reaches the target power.

Set \`dropout = 0\` to reproduce unadjusted textbook or \`pwr\` results.
The rate actually applied is recorded in the returned object
(\`dropout\`, \`n_effective\`, \`n_enrolled\`) and is stated in its
print method, the methods paragraph, the reproducibility script and the
downloadable reports. The exported engine helpers (\`logrank_power()\`,
\`mcnemar_power()\`, \`mixed_model_power()\`, \`trend_power()\`) take
the analyzed sample size directly and do not apply any dropout
adjustment.

## Examples

``` r
if (FALSE) { # \dontrun{
study <- multi_aim_study(study_name = "Phase II Trial")
study <- add_aim(study,
  power_calc("ttest_2groups", target_power = 0.80,
             effect_size = 0.5, effect_method = "cohens_d",
             dropout = 0.10),
  outcome = "Toxicity at 3 months")
study <- add_aim(study,
  power_calc("logrank", target_power = 0.80,
             effect_size = 0.65, effect_method = "hazard_ratio",
             dropout = 0.10, event_prob = 0.7),
  outcome = "6-month survival")
format_multi_aim_df(study)
multi_aim_markdown(study)
} # }
```
