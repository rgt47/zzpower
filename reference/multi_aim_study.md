# Initialise an empty multi-aim study

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
