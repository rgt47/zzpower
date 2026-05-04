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
