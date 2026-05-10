# zzpower v1.0.0 (in development)

## CRAN-readiness pass

* **`R CMD check --no-tests` reports `Status: OK`** (0 errors,
  0 warnings, 0 notes) on the built tarball. The full check
  with tests included also passes.
* **Typst PDF backend removed.** The on-demand-install prompt
  and the Typst rendering path were dropped because the typst
  R package is not on CRAN, which forced one of: (a) a CRAN
  policy violation (`Suggests` package not in mainstream
  repositories) or (b) a static-analysis-evading code
  obfuscation. PDF reports now render exclusively via
  rmarkdown's xelatex pipeline (the existing fallback). Roughly
  220 lines removed from `R/generic_server_factory.R`. The
  feature can be reinstated when typst is published to CRAN.
* **Test scaffolding cleaned up.** Six unprefixed internal-
  function calls in `inst/tinytest/test_module_server.R` (e.g.
  bare `.render_methods_paragraph(ctx)`) were updated to
  `zzpower:::` prefixes. The bare calls had been working under
  `devtools::test()` (which exposes the namespace via
  `load_all()`) but failing under `R CMD check` (which uses
  `library()`, hiding internals). Stale `tests/testthat/`
  scaffolding and `tests/testthat.R` removed (the package
  migrated to tinytest in an earlier release).
* **Roxygen orphan-block fix.** Five files had file-level
  roxygen header blocks (e.g. `#' Power Test Registry`) with no
  function attached. roxygen2 silently merged them into the
  next function's docs, producing a cascade of malformed
  `\keyword{...}` entries (one per word from the merged
  description) and a wrong `\title{}`. Files: `R/power_helpers.R`,
  `R/power_test_registry.R`, `R/proportion_helpers.R`,
  `R/generic_ui_builder.R`, `R/generic_server_factory.R`. The
  hoisted `@importFrom` directives moved to
  `R/zzpower-package.R` where they belong. The stripped header
  blocks were redundant boilerplate, not real documentation.
* **Vignette artifact removed.** `vignettes/quickstart.pdf`
  deleted; its presence triggered the same
  `find_vignette_product()` trap as in zzedc (`.pdf` selected
  over `.html` in the tie-mtime case, then stripped by
  `.Rbuildignore` ordering, dropping the HTML output).
  `.gitignore` already covered `vignettes/*.pdf`.
* **DESCRIPTION corrections.** `R (>= 4.0.0)` -> `R (>= 4.1.0)`
  (the package uses the native pipe `|>`); `LazyData: true`
  removed (no `data/` directory exists).
* **Top-level cleanup.** Placeholder `LICENSE` file deleted
  (the `License: GPL (>= 3)` entry does not need it; CRAN
  flagged the file as unmentioned). Hidden `.zzcollab` directory
  added to `.Rbuildignore`. Non-ASCII middle-dot in
  `R/launch_zzpower.R:147` swapped to ASCII `|`.

## Wave 5: multi-aim aggregation (Gap 3, programmatic)

The roadmap's Gap 3 calls for a study-level table assembling
multiple Specific Aims into one §2.5 Layout 4 table. This
release ships the **algorithmic core** as exported R functions;
the Shiny nav-panel + URL-bookmarking UI is deferred to v1.1.x
(it was the bulk of the 24h estimate).

### New programmatic API

* `multi_aim_study(study_name)` — initialise an empty study
  object. Returns a `multi_aim_study` (S3) carrying optional
  metadata and an aims list.
* `add_aim(study, ctx, name, outcome)` — append one
  `calc_context` (from `power_calc()`) per call. Accumulates
  aims for the study.
* `format_multi_aim_df(study)` — produce a tidy data frame
  with columns Aim, Outcome, Test, Effect size, Alpha, Power,
  N evaluable, N enrolled, Binding. The `Binding` column flags
  the row whose enrolled N is the largest — the aim that
  drives the overall study size.
* `multi_aim_markdown(study)` — render the table as a
  paste-ready Markdown block with §2.3-style caption, "Aim *"
  asterisk on the binding row, and a one-line legend below.
* `multi_aim_csv(study, file)` — return the table as a CSV
  string (when `file = NULL`) or write it to disk. Reviewers
  paste straight into Excel.

### Worked example: reconstructing the Mohile R01 multi-aim table

```r
study <- multi_aim_study(study_name = "Reducing Chemo Toxicity")
study <- add_aim(study,
  power_calc("cluster_prop", target_power = 0.80,
             effect_size = -0.13, effect_method = "difference",
             m_cluster = 39, icc = 0.10, baseline = 0.46,
             dropout = 0.10),
  name = "Primary",
  outcome = "Grade 3-5 chemo toxicity within 3 months")
study <- add_aim(study,
  power_calc("logrank", target_power = 0.80,
             effect_size = 0.511, effect_method = "hazard_ratio",
             event_prob = 0.7, dropout = 0.10),
  name = "Secondary 1",
  outcome = "6-month survival")
multi_aim_markdown(study)
```

### Deferred (v1.1.x)

* Shiny nav-panel "Multi-aim study" with `enableBookmarking('url')`
  for shareable URL state, "Add this aim to study" buttons on
  each test panel, in-app delete/rename, etc.
* The programmatic API ships the data structures and rendering
  logic that the nav-panel will consume; the deferred work is
  pure UI plumbing.

# zzpower v0.7.0 (in development)

## Wave 4: design coverage (Gaps 7, 8, 11)

### Cluster-randomized RCT specs (Gap 7)

* Three new test specifications: `cluster_rct` (continuous
  outcome via Cohen's d), `cluster_prop` (binary outcome via
  Cohen's h), `cluster_logrank` (survival outcome via Schoenfeld
  formula). Each adds `m_cluster` (mean cluster size) and `icc`
  (intracluster correlation) parameters; the design effect
  DE = 1 + (m̄ − 1) × ICC inflates the variance, equivalent to
  reducing the per-arm effective sample size to N / DE.
  Methodologically standard (Donner & Klar 2000); fills the NIH
  "special methods are required" gap for cluster-randomized
  trials. Reference design: NCI Mohile R01CA177592.
* The Schoenfeld back-compat `n1`/`n2` for `cluster_logrank`
  encode `participants × event_prob / DE` (events with cluster
  shrinkage), mirroring the pattern the non-cluster `logrank`
  spec uses for events.
* Total registry size grows from 11 to 14 specs.

### Bonferroni-aware ANOVA (Gap 11)

* `anova_oneway` gains a `n_pairwise_contrasts` input. When set
  > 1 the spec's new `effective_alpha` hook divides α by the
  contrast count before solving the omnibus F-test. The omnibus
  F itself does not require Bonferroni correction; this readout
  is conservative (more N than strictly needed for the omnibus)
  but supports protocol-level commitment to Bonferroni for
  post-hoc pairwise comparisons.
* The methods paragraph annotates the adjustment in-line:
  *α=0.0167 (Bonferroni-adjusted from 0.05)*. The reproducibility
  R script uses the adjusted α directly.
* Generic `effective_alpha = function(input, alpha)` registry
  hook plumbed through `power_calc()` and `.required_n()`. Other
  specs inherit identity behavior; future specs can declare
  their own multiplicity adjustments without engine changes.

### Non-inferiority option (Gap 8, partial)

* `ttest_2groups` gains `hypothesis_type` (radio:
  superiority / non_inferiority) and `ni_margin` (numeric,
  conditional on NI selection). When NI is selected, the
  `standardize()` function shifts the standardized effect by
  the margin so power is computed on `(true_effect + margin) /
  sd`. This matches the FDA convention where one-sided α=0.025
  has the same critical value as two-sided α=0.05.
* Latent bug fix in `.compute_power`: zzpower's internal
  `"one.sided"` label is now mapped to `"greater"` before being
  passed to `pwr::pwr.*` functions, which only accept
  `"two.sided"`/`"less"`/`"greater"`. This bug was previously
  masked because superiority tests typically use two-sided
  defaults; one-sided + NI workflow surfaced it.
* **Deferred to v0.7.x:** non-inferiority for `prop_2groups`
  (Cohen's h margin conversion) and `logrank` (log-HR margin)
  needs careful per-scale margin handling and is left as
  follow-up work. Equivalence (TOST) is also deferred.

# zzpower v0.6.1 (in development)

## Wave 3: reproducibility R script export (Gap 9)

* **Reproducibility script appended to every report.** All four
  formats (markdown, text, HTML, PDF via Typst, Word via
  rmarkdown) now include a "Reproducibility script" section that
  reproduces the headline calculation in a fresh R session.
  `library(pwr)` for the seven `pwr`-backed tests;
  `library(zzpower)` for the four custom helpers (logrank,
  McNemar, mixed model, Cochran-Armitage trend). The script
  uses literal numeric arguments — no Shiny state required.
* **Custom helpers exported.** `logrank_power`, `mcnemar_power`,
  `mixed_model_power`, and `trend_power` are now part of the
  package's public API so the reproducibility script runs.
  Previously `@keywords internal` only.
* **Argument order matches the function signature.** The
  rendered call places arguments in the order their
  `formals()` declares them (e.g. `d`, `n1`, `n2`, `sig.level`,
  `alternative`) rather than alphabetic insertion order, so
  the script reads naturally.
* **Per-spec `repro_call` field populated.** Wave 1 reserved
  `repro_call` as NULL; Wave 3 fills it with the qualified
  function name (`"pwr::pwr.t2n.test"`,
  `"zzpower::logrank_power"`, etc.) for each of the eleven
  registry entries.
* **`.report_data_to_ctx()` helper.** Reshapes the existing
  report-data list into a calc_context so the renderer can
  share one engine across the methods-paragraph (Wave 2) and
  the reproducibility-script (Wave 3) generators.

# zzpower v0.6.0 (in development)

## Wave 2: headline grant-proposal artifacts

This release ships the two artifacts grant writers paste directly:
the methods-section paragraph (Gap 1) and the sensitivity table
(Gap 2). Both consume the calc_context contract built in Wave 1.

### Methods paragraph (Gap 1)

* New "Methods paragraph" card on every test panel renders a
  Glueck-Muller-shaped paragraph for the NIH Statistical Design
  and Power attachment or the ICH E9 §3.5 sample-size statement.
  Composed of seven sentences: planned analysis, effect-size
  assumption with citation, alpha + power + N (per-arm phrased
  for multi-arm designs), dropout inflation, sensitivity scenario
  (ICH E9 §3.5), software citation, sex-as-biological-variable
  paragraph (NIH rigor; toggleable). The S3 phrasing flips
  between "the achieved power is X%" (power mode) and "X
  evaluable participants are required" (sample-size mode).
* "Copy" button uses `navigator.clipboard.writeText` to put the
  paragraph on the clipboard; the button briefly reads "Copied"
  for confirmation.
* The paragraph reflects the **lower end** of the effect-size
  slider — the most conservative assumption, which produces the
  largest N. This matches NIH convention where reviewers
  assume the smallest effect from the user's range.
* Effect-size phrasing is humanised per method
  (`cohens_d` → "Cohen's d", `hazard_ratio` → "the hazard
  ratio", `correlation` → "the correlation coefficient r", and
  so on for the eleven methods).

### Sensitivity table (Gap 2)

* New "Sensitivity table" card on every test panel renders the
  §2.1 Layout 1 sample-size table: rows = effect-size grid,
  columns = required N at 80% and 90% power, in both evaluable
  (analyzed) and enrolled (post-dropout-inflation) layers. Default
  rows seed from each spec's `default_effect_grid` (Cohen's
  small/medium/large for d, f, r; reasonable per-method grids for
  proportion difference, odds ratio, hazard ratio, etc.).
* The Effect-size column is **editable**: double-click a cell to
  override; the four N columns recompute via `power_table()`.
  Other columns are read-only.
* Two download handlers ship CSV and Markdown formats. The
  Markdown caption follows §2.3 conventions (which inputs vary
  / which are held fixed / formula citation). A new
  `.df_to_markdown(df, caption)` helper produces the table body.

### Tests

* Twelve new assertions cover the methods-paragraph renderer
  (Test name, citation, alpha, "evaluable participants",
  dropout, sensitivity sentence, software, sex paragraph), the
  sex-toggle off-path, the sensitivity-factor off-path, the
  Shiny output reactive, the sensitivity-table reactive (default
  3 rows from `cohens_d` grid; required-N monotonicity in effect;
  90% > 80% N), and the markdown formatter. Total assertions
  rose from 174 to 197.

# zzpower v0.5.0 (in development)

## Wave 1: foundation for grant-proposal features

This release lands the structural plumbing for the grant-writing
roadmap (Gaps 4, 5, 6, 10, 12 from
`docs/grant-proposals-sample-size-practice.md`). No new headline
artifact ships in this version — the methods-paragraph generator,
sensitivity table builder, and reproducibility script export
(Wave 2 / Wave 3) build on top of these foundations.

### Three-layer N contract (Gap 4)

* All eleven `sample_size_calc()` returns now share the canonical
  shape `n_per_arm_evaluable / n_per_arm_enrolled /
  n_total_evaluable / n_total_enrolled / n_arms / arm_labels /
  dropout`, plus the legacy `n / n1 / n2 / k / n1_itt / n2_itt`
  shims that existing consumers (the server module, report
  builders, headline value-boxes) read. A new
  `.canonicalize_sample_sizes()` helper centralises the
  enrolled-to-evaluable conversion.
* **Behaviour change:** the ten specs that previously had no
  `dropout` parameter now expose a Dropout Rate slider with
  default 0.10. Power curves on those tests are now ~10% lower
  than before unless the user dials dropout to 0. This aligns
  every test with the existing `ttest_2groups` convention and
  matches the NIH-realistic default of 10% loss to follow-up.
* Logrank's back-compat `n1`/`n2` continue to report **expected
  events** (the value the Schoenfeld formula consumes); the
  canonical `n_per_arm_*` fields report **participants** for the
  first time.

### Programmatic R API (Gap 10)

* New exported `power_calc(test, sample_size, effect_size,
  effect_method, target_power, alpha, alternative, ...)` returns
  a `calc_context` named list — every grant-writing artifact
  reads its values from this single object. Supplying
  `target_power` instead of `sample_size` triggers bisection on
  the required-N answer.
* New exported `power_table(test, effect_grid, power_thresholds,
  ...)` returns the §2.1 Layout 1 sensitivity table as a tidy
  `data.frame`. Defaults to Cohen small/medium/large from the
  spec's `default_effect_grid`; both `0.80` and `0.90` power
  thresholds in the column set out of the box.
* Two free helpers (`.compute_power`, `.required_n`) now hold
  the call-assembly logic that the Shiny module's `.power_at_n`
  closure inlined; the server keeps its closure unchanged for
  this release, but the next refactor will route through the
  shared helpers.

### Registry metadata (Gap 5 / 6 / 12 plumbing)

* Each registry entry gains four new fields:
  `formula_citation` (a one-liner with the canonical citation
  for the test's formula — Cohen, Schoenfeld, Connor, Diggle, or
  Cochran–Armitage as applicable), `default_effect_grid` (a
  per-method numeric grid for `power_table()`),
  `paragraph_template` (NULL placeholder; populated in Wave 2),
  and `repro_call` (NULL placeholder; populated in Wave 3).
* `.build_calc_context()` constructs the `calc_context` from a
  test_spec + canonical sample sizes + provenance fields. It is
  the contract every Wave 2/3 generator reads from.

### Sidebar inputs (Gap 5 / Gap 6 / Gap 12)

* The Advanced Settings accordion gains four new inputs that the
  Wave 2 methods-paragraph generator will inject into its
  output: `effect_source` (citation text), `effect_doi`,
  `sensitivity_factor` (multiplier for the conservative-effect
  sentence per ICH E9 §3.5), and `include_sex_paragraph` (NIH
  rigor toggle).
* These inputs render but do not affect any output yet; their
  rendered output is Wave 2.

### Tests

* `inst/tinytest/test_module_server.R` gains 160 assertions:
  per-spec verification that the canonical N contract holds
  for every entry in the registry, and end-to-end smoke tests
  for `power_calc()` and `power_table()`. Total assertions
  rose from 14 to 174.

# zzpower v0.4.1 (2026-05-03)

## Reports

* PDF and Word formats added to the Download Report card. PDF
  rendering uses Typst (via the optional `typst` package) for
  fast, no-LaTeX-required output and falls back to xelatex via
  `rmarkdown::pdf_document()` when Typst is not installed. Word
  uses `rmarkdown::word_document()` (pandoc).
* When the user selects PDF without `typst` installed, the app
  shows a once-per-session modal offering to install the package
  and download the Typst CLI binary.
* All four report formats (text, HTML, PDF, Word) now include the
  package citation in their footer in plain-text form, drawn from
  a new `inst/CITATION` file. The citation reads "Thomas, R.G.
  (2026). zzpower: ..."

## Plot polish

* Power-curve plots now annotate both the 80 percent and the 90
  percent power thresholds. The 80 percent line is gold dashed
  (primary); the 90 percent line is grey dotted (secondary), with
  a smaller marker and label. The y-axis renders as percent
  (50 percent / 80 percent / ...) rather than 0.5 / 0.8.
* Sample-size plots gain a "min N = ..." annotation at the
  smallest required-N point.
* The plotly experiment introduced mid-cycle was rolled back; the
  geom_label refactor it produced is retained because it is
  strictly cleaner than the original `annotate("label", ...)`
  pattern.

## App shell

* Page footer added: version, source link, issue tracker, "How to
  cite" modal trigger, and licence.
* Hero category grids on the landing page use breakpoint-aware
  column widths so single-card categories no longer leave 75
  percent of the row empty on small screens.

## Dependencies

* `htmltools` added to `Imports` (used for citation HTML escape
  in the report footer).
* `typst` added to `Suggests` (optional, for PDF reports).

# zzpower v0.4.0 (2026-05-02)

## Bug fixes

* The hero-card launcher cards on the landing screen are now keyboard
  accessible. Previously they were rendered as `<div>` with an
  `onclick` handler, which made them unreachable by tab navigation
  and unannounced as buttons by screen readers. Each card now
  declares `role = "button"`, `tabindex = "0"`, an `aria-label`, an
  `onkeydown` handler that triggers on Enter or Space, and a
  visible focus outline.
* `inst/app/server.R` no longer runs both code paths after the
  enhanced gsheets integration succeeds. (Not applicable to
  zzpower; carry-over note from upstream review.)

## User-visible improvements

* **Headline value boxes.** Each test panel now shows three
  `bslib::value_box` tiles above the power curve and results table,
  surfacing the answer the user came for: the effect size at 80%
  power, the maximum power in the tested range, and the total
  sample size (in power-solve mode); or the smallest and largest
  required N and the target power (in sample-size-solve mode).
* **Sidebar reorganised into accordion panels.** The sidebar now
  uses `bslib::accordion` with three panels ('Sample Size & Design',
  'Effect Size', 'Advanced Settings'). The Advanced Settings panel
  is closed by default; the other two are open. Replaces a tall
  stack of `<hr>`-separated sections.
* **Structured study-summary card.** The Study Summary card
  previously rendered as `verbatimTextOutput` (monospace, ASCII).
  It is now a `<dl>` definition list with semantic Bootstrap
  styling.
* **Parameter tooltips.** Every input now shows the `description`
  field from the test registry as a hover tooltip via an info-icon
  next to the label. This metadata existed in the registry but
  was never rendered.
* **Validation messages.** When inputs are invalid, the plot and
  table cards now show 'Resolve the input issues shown in the
  sidebar to see results.' instead of going blank
  (`shiny::validate(shiny::need(...))` replaces silent `req()`).
* **Result-table formatting.** The DT results table now uses
  `formatRound` for effect-size and standardized columns and
  `formatPercentage` for the power column. Pagination chrome is
  hidden because the table fits in 16 rows.
* **Busy indicators.** `shiny::useBusyIndicators()` (Shiny 1.9.0+)
  now provides automatic recalculating glyphs and a global pulse
  during reactive computation.

## Performance

* **Caching.** Both `power_results` and `sample_size_results`
  reactives are now wrapped in `shiny::bindCache()`, keyed on all
  inputs that affect the calculation. Repeated parameter
  combinations (common when a user drags a slider back to a
  previous value) return instantly.

## Code maintenance

* **Removed ~1,800 lines of dead legacy code.** Deleted
  `R/server_logic.R`, `R/ui_components.R`, `R/server_proportions.R`,
  `R/ui_proportions.R`, and `R/report_generation.R`, plus the four
  test files that exercised them (`test_server_logic.R`,
  `test_ui_components.R`, `test_proportions.R`, `test_validation.R`).
  These were all replaced by the registry/factory architecture
  (`generic_ui_builder.R`, `generic_server_factory.R`,
  `power_test_registry.R`) but had been kept alive only by tests.
* **DT API modernised.** Replaced deprecated `DT::dataTableOutput`
  and `DT::renderDataTable` with current `DT::DTOutput` and
  `DT::renderDT` aliases.
* **Pipe convention.** Removed `magrittr` from `Imports`; the
  surviving `%>%` pipes were inside the deleted legacy files. All
  code now uses the native `|>`.
* **`shiny` floor.** Bumped from `>= 1.7.0` to `>= 1.9.0`. Required
  for `useBusyIndicators()`; covers `bindCache()` (1.6.0) and
  `ExtendedTask` (1.8.0) by reach.
* **Public module API.** `create_generic_test_ui()` and
  `create_generic_test_server()` are now exported. They were the
  de-facto public surface (used by `launch_zzpower()`) but
  previously marked `@keywords internal`.
* **Updated package-level docstring.** Now reflects the eleven
  registered tests rather than just the original two-group
  parallel design.

## Testing

* **`testServer()` reactivity tests.** New test file
  `inst/tinytest/test_module_server.R` exercises the four
  power-function shapes in the registry (n1+n2 / single-n /
  proportions via Cohen's h / survival via custom helper) end to
  end through `shiny::testServer()`. Total test count rose by
  ~15 assertions.

# zzpower v0.3.0

* Initial public release.
