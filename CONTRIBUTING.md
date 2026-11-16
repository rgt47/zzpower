# Contributing to zzpower

Thank you for your interest in contributing. This document describes the
expected workflow for proposing changes, reporting issues, and submitting
pull requests.

## Reporting issues

Before opening an issue, please search existing issues at
<https://github.com/rgt47/zzpower/issues> to confirm the problem has
not already been reported. When opening a new issue, include:

- A minimal reproducible example (a `reprex::reprex()` is preferred).
  For Shiny-app issues, describe the input values and the steps that
  reproduce the problem.
- The output of `sessionInfo()`.
- The version of zzpower in use (`packageVersion('zzpower')`).
- The expected behaviour and the observed behaviour.

## Proposing a change

For non-trivial changes, please open an issue first to discuss the
approach. The plugin/registry architecture has well-defined extension
points; a brief design discussion saves rework.

## Adding a new statistical test

zzpower's registry pattern makes adding tests straightforward:

1. Create a test specification function (see `R/power_test_registry.R`)
   that returns a list with `id`, `name`, `power_function`,
   effect-size methods, and the `standardize()`,
   `sample_size_calc()`, and `validation()` callbacks.
2. Register the spec in `get_power_test_registry()`.
3. Add the test id to `test_categories` and `card_colors` in
   `R/launch_zzpower.R` so that it appears on the home page.
4. Add specification tests to `inst/tinytest/test_framework.R`
   exercising your effect-size methods and edge cases, and update the
   registry-length assertion there.
5. The framework generates the test's tab, server logic, and reports.

## Pull request workflow

1. Fork the repository and create a topic branch off `main`.
2. Install development dependencies. zzpower uses `renv`; restore the
   pinned environment with:
   ```r
   renv::restore()
   ```
3. Make your changes. Keep commits focused; prefer many small commits
   over one large one.
4. Add or update tests in `inst/tinytest/`. New exported functions
   require tests.
5. Run `devtools::document()` to regenerate `man/` and `NAMESPACE`.
6. Run the full check locally:
   ```r
   devtools::check()
   ```
   The check must pass with no errors, warnings, or notes other than
   the standard 'New submission' note.
7. Update `NEWS.md` with a one-line bullet under the unreleased section.
8. Open a pull request against `main`. Reference any related issues.

## Coding style

- Use the native R pipe (`|>`); avoid `%>%` in new code.
- Use `<-` for assignment, never `=`.
- Use `snake_case` for functions and variables.
- Prefer implicit returns; reserve `return()` for early exits.
- Document all exported functions with `roxygen2`. Each must have
  `@title`, `@description`, `@param`, `@return`, and `@examples`.
- Two-space indentation. Single quotes for character literals.
- Do not add 'what' comments. Reserve comments for non-obvious 'why'.

## Tests

Tests use `tinytest`, with test files in `inst/tinytest/`. Run with:

```r
pkgload::load_all()
tinytest::run_test_dir('inst/tinytest')
```

For coverage reports:

```r
covr::package_coverage()
```

## Code of Conduct

By participating in this project, you agree to abide by the
[Code of Conduct](CODE_OF_CONDUCT.md).
