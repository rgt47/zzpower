#' Power Test Registry
#'
#' Centralized registry defining all available power analysis tests.
#' Each entry defines the parameters, effect size methods, and power calculation
#' for a specific statistical test.
#'
#' @keywords internal

#' Canonicalize a sample-size calculation result
#'
#' Converts per-arm enrolled counts plus a dropout rate into the
#' canonical four-layer record (`n_per_arm_evaluable`,
#' `n_per_arm_enrolled`, `n_total_evaluable`, `n_total_enrolled`)
#' that every grant-writing artifact (Gaps 1, 2, 3, 9) reads from.
#' Back-compat scalars (`n`, `n1`, `n2`, `n1_itt`, `n2_itt`, `k`)
#' are populated so consumers that read the legacy keys keep
#' working unchanged.
#'
#' @param per_arm_enrolled Numeric vector of enrolled (pre-dropout)
#'   counts per arm. Length 1 for single-sample tests, 2 for
#'   two-arm tests, k for ANOVA-style designs.
#' @param dropout Expected dropout rate in [0, 1).
#' @param arm_labels Optional character vector of arm names for
#'   display. Defaults to "Sample" for one arm, "Group i" otherwise.
#'
#' @return Named list with the canonical fields plus legacy shims.
#'
#' @keywords internal
.canonicalize_sample_sizes <- function(per_arm_enrolled,
                                       dropout = 0,
                                       arm_labels = NULL) {
  per_arm_enrolled  <- as.numeric(per_arm_enrolled)
  per_arm_evaluable <- per_arm_enrolled * (1 - dropout)
  n_arms <- length(per_arm_enrolled)

  if (is.null(arm_labels)) {
    arm_labels <- if (n_arms == 1L) {
      "Sample"
    } else {
      paste("Group", seq_len(n_arms))
    }
  }

  list(
    n_per_arm_evaluable = per_arm_evaluable,
    n_per_arm_enrolled  = per_arm_enrolled,
    n_total_evaluable   = sum(per_arm_evaluable),
    n_total_enrolled    = sum(per_arm_enrolled),
    n_arms              = n_arms,
    arm_labels          = arm_labels,
    dropout             = dropout,

    # Back-compat scalars consumed by generic_server_factory.R
    # and the report builders. Evaluable values (post-dropout)
    # are what the power-calc functions expect.
    n  = if (n_arms == 1L) per_arm_evaluable else per_arm_evaluable[1L],
    n1 = if (n_arms >= 1L) per_arm_evaluable[1L] else NULL,
    n2 = if (n_arms >= 2L) per_arm_evaluable[2L] else NULL,
    n1_itt = if (n_arms >= 1L) per_arm_enrolled[1L] else NULL,
    n2_itt = if (n_arms >= 2L) per_arm_enrolled[2L] else NULL,
    k  = if (n_arms >= 2L) n_arms else NULL
  )
}

#' Build a calc_context for a single power calculation
#'
#' Bundles inputs, outputs, provenance, and metadata for one
#' power/sample-size calculation into a single named list. Every
#' grant-writing artifact (Gap 1 methods paragraph, Gap 2 table
#' builder, Gap 9 reproducibility script, Gap 3 multi-aim aggregator)
#' reads its values from a calc_context. Building the context once
#' from inputs and consuming it from many places replaces what
#' would otherwise be eleven specs × five output-paste sites.
#'
#' This is the structural shape consumed by the programmatic API
#' (`power_calc()`, Gap 10) and the Wave 2 generators. Field
#' values populated in earlier waves are treated as ground truth;
#' fields populated in later waves default to NULL until then.
#'
#' @param test_spec A test spec (one of the elements of
#'   `get_power_test_registry()`).
#' @param sample_sizes Output of the spec's `sample_size_calc()`,
#'   already canonicalized via `.canonicalize_sample_sizes()`.
#' @param effect_size Numeric scalar effect size on the native
#'   scale of the chosen `effect_method`.
#' @param effect_size_std Numeric scalar effect size on the
#'   standardized scale (Cohen's d, h, f, or analogous).
#' @param effect_method Character string identifying which entry
#'   of `test_spec$effect_size_methods` was selected.
#' @param effect_params Named list of method-dependent parameters
#'   (e.g. `sd0`, `p2`, `baseline`).
#' @param target_power Target power threshold (sample-size mode)
#'   or NULL (power mode).
#' @param achieved_power Achieved power at the proposed N (power
#'   mode) or NULL (sample-size mode).
#' @param alpha Type I error rate.
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @param effect_source,effect_doi Provenance strings for the
#'   effect-size assumption (Gap 5). Default empty.
#' @param sensitivity_factor Numeric multiplier for the
#'   conservative-effect sentence in the methods paragraph (Gap 6).
#' @param include_sex_paragraph Logical; whether the sex-as-bio
#'   paragraph is appended (Gap 12).
#'
#' @return Named list with the calc_context shape.
#'
#' @keywords internal
.build_calc_context <- function(test_spec,
                                sample_sizes,
                                effect_size,
                                effect_size_std    = NA_real_,
                                effect_method      = NULL,
                                effect_params      = list(),
                                target_power       = NULL,
                                achieved_power     = NULL,
                                alpha              = 0.05,
                                alternative        = "two.sided",
                                effect_source      = "",
                                effect_doi         = "",
                                sensitivity_factor = NULL,
                                include_sex_paragraph = TRUE) {

  list(
    # Identity
    test_id          = test_spec$id,
    test_name        = test_spec$name,
    test_description = test_spec$description,

    # Inputs (the user knobs)
    alpha              = alpha,
    alternative        = alternative,
    target_power       = target_power,
    effect_size        = effect_size,
    effect_size_std    = effect_size_std,
    effect_method      = effect_method,
    effect_params      = effect_params,

    # Provenance (Gaps 5, 6, 12)
    effect_source         = effect_source,
    effect_doi            = effect_doi,
    sensitivity_factor    = sensitivity_factor,
    include_sex_paragraph = include_sex_paragraph,

    # Outputs
    sample_sizes   = sample_sizes,
    achieved_power = achieved_power,

    # Metadata for downstream artifacts
    formula_citation     = test_spec$formula_citation %||% "",
    paragraph_template   = test_spec$paragraph_template,  # may be NULL
    repro_call_template  = test_spec$repro_call,          # may be NULL
    default_effect_grid  = test_spec$default_effect_grid %||% list()
  )
}

#' Compose a power-function call for a single (test, design) pair
#'
#' Mirrors the call assembly inside `create_generic_test_server`'s
#' `.power_at_n()` closure but takes plain values rather than
#' Shiny `input` references. Centralising the call shape here lets
#' the programmatic API (`power_calc`, `power_table`) and the
#' Shiny server share one engine.
#'
#' @param test_spec A registry entry.
#' @param sample_sizes Output of the spec's `sample_size_calc()`.
#' @param effect_size_std Standardized effect size (Cohen's d/h/f
#'   or analogous).
#' @param alpha Type I error rate.
#' @param alternative `"two.sided"` or `"one.sided"`.
#'
#' @return Numeric scalar power, or `NA_real_` if the call errors.
#'
#' @keywords internal
.compute_power <- function(test_spec, sample_sizes, effect_size_std,
                           alpha = 0.05, alternative = "two.sided") {
  fn <- test_spec$power_function
  fn_formals <- names(formals(fn))
  es_param <- intersect(c("d", "h", "r", "f"), fn_formals)[1]

  call_args <- list(sig.level = alpha)
  if ("alternative" %in% fn_formals) {
    call_args$alternative <- alternative
  }
  if (!is.na(es_param)) call_args[[es_param]] <- effect_size_std

  if (!is.null(sample_sizes$n1) && "n1" %in% fn_formals) {
    call_args$n1 <- sample_sizes$n1
    call_args$n2 <- sample_sizes$n2
  } else if ("n" %in% fn_formals) {
    call_args$n <- sample_sizes$n
  }

  if ("type" %in% fn_formals && !is.null(test_spec$test_type)) {
    call_args$type <- test_spec$test_type
  }

  if (!is.null(test_spec$power_args)) {
    for (nm in names(test_spec$power_args)) {
      if (nm %in% fn_formals) {
        val <- test_spec$power_args[[nm]]
        call_args[[nm]] <- if (is.character(val) &&
                               val %in% names(sample_sizes)) {
          sample_sizes[[val]]
        } else {
          val
        }
      }
    }
  }

  tryCatch({
    out <- do.call(fn, call_args)
    out$power %||% NA_real_
  }, warning = function(w) NA_real_,
     error = function(e) NA_real_)
}

#' Solve for required N to achieve a target power
#'
#' Bisection on the total sample-size slider domain. Mirrors
#' `.find_required_n()` inside the server module.
#'
#' @param test_spec A registry entry.
#' @param target_power Target power threshold (e.g. 0.80).
#' @param effect_size_std Standardized effect size.
#' @param design_params Named list of design parameters consumed by
#'   the spec's `sample_size_calc()` (e.g. `dropout`, `allocation`).
#' @param alpha Type I error rate.
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @param n_lo,n_hi Bisection bounds on total enrolled sample size.
#'
#' @return Smallest total enrolled N within [n_lo, n_hi] that
#'   achieves at least `target_power`, or `NA_real_` if not found.
#'
#' @keywords internal
.required_n <- function(test_spec, target_power, effect_size_std,
                        design_params = list(),
                        alpha = 0.05, alternative = "two.sided",
                        n_lo = 10, n_hi = 10000) {
  power_at <- function(n_total) {
    ss <- test_spec$sample_size_calc(
      c(list(sample_size = n_total), design_params)
    )
    .compute_power(test_spec, ss, effect_size_std, alpha, alternative)
  }

  p_lo <- power_at(n_lo)
  while (is.na(p_lo) && n_lo < 100) {
    n_lo <- n_lo + 10
    p_lo <- power_at(n_lo)
  }
  if (is.na(p_lo)) return(NA_real_)

  p_hi <- power_at(n_hi)
  if (is.na(p_hi))         return(NA_real_)
  if (p_hi < target_power) return(NA_real_)
  if (p_lo >= target_power) return(n_lo)

  for (iter in seq_len(30)) {
    n_mid <- ceiling((n_lo + n_hi) / 2)
    p_mid <- power_at(n_mid)
    if (is.na(p_mid)) return(NA_real_)
    if (p_mid < target_power) n_lo <- n_mid else n_hi <- n_mid
    if (n_hi - n_lo <= 1) break
  }

  n_hi
}

#' Programmatic power calculation
#'
#' Compute power (or required N) for a single test from R, without
#' launching the Shiny app. The companion to `power_table()` for
#' tabular sweeps.
#'
#' @param test Character test id (one of `names(get_power_test_registry())`)
#'   or a test_spec list returned by the registry.
#' @param sample_size Total enrolled sample size. Optional in
#'   sample-size mode (`target_power` supplied) — left NULL the
#'   function solves for required N via bisection.
#' @param effect_size Effect size on the native scale of the chosen
#'   `effect_method` (e.g. raw difference for `"difference"`,
#'   hazard ratio for `"hazard_ratio"`, Cohen's d for `"cohens_d"`).
#' @param effect_method Character; one of
#'   `test_spec$effect_size_methods`. Default is the first.
#' @param target_power Target power threshold. If supplied the
#'   function solves for required N; otherwise computes achieved
#'   power at `sample_size`.
#' @param alpha Type I error rate.
#' @param alternative `"two.sided"` or `"one.sided"`.
#' @param ... Additional design parameters passed through to the
#'   spec's `sample_size_calc()` (e.g. `dropout`, `allocation`,
#'   `ratio`, `event_prob`, `n_groups`) and `standardize()`
#'   (e.g. `sd0`, `p2`, `baseline`, `sigma`, `correlation`).
#'
#' @return A `calc_context` named list (see `.build_calc_context`).
#'
#' @examples
#' \dontrun{
#' # Power at fixed N
#' power_calc("ttest_2groups",
#'            sample_size = 100, effect_size = 0.5,
#'            effect_method = "cohens_d")
#'
#' # Required N at target power
#' power_calc("ttest_2groups",
#'            target_power = 0.80, effect_size = 0.5,
#'            effect_method = "cohens_d")
#' }
#'
#' @export
power_calc <- function(test, sample_size = NULL, effect_size,
                       effect_method = NULL,
                       target_power  = NULL,
                       alpha         = 0.05,
                       alternative   = "two.sided",
                       ...) {

  reg <- get_power_test_registry()
  test_spec <- if (is.character(test)) reg[[test]] else test
  if (is.null(test_spec)) {
    stop("unknown test: ", test, call. = FALSE)
  }

  effect_method <- effect_method %||% test_spec$effect_size_methods[1]
  if (!effect_method %in% test_spec$effect_size_methods) {
    stop(sprintf("effect_method '%s' not valid for test '%s'",
                 effect_method, test_spec$id), call. = FALSE)
  }

  extra <- list(...)
  effect_std <- as.numeric(
    test_spec$standardize(effect_size, effect_method, extra)
  )

  # Gap 11: spec-declared alpha adjustment (e.g. Bonferroni for the
  # ANOVA's pairwise-contrast count). Apply once; downstream
  # power/required-N calculations use the adjusted value.
  alpha_eff <- if (!is.null(test_spec$effective_alpha)) {
    test_spec$effective_alpha(extra, alpha)
  } else {
    alpha
  }

  if (!is.null(target_power)) {
    n_total <- .required_n(test_spec, target_power, effect_std,
                           design_params = extra,
                           alpha = alpha_eff,
                           alternative = alternative)
    if (is.null(sample_size)) sample_size <- n_total
  }

  if (is.null(sample_size)) {
    stop("Either `sample_size` or `target_power` must be supplied",
         call. = FALSE)
  }

  ss <- test_spec$sample_size_calc(c(list(sample_size = sample_size),
                                     extra))
  achieved <- .compute_power(test_spec, ss, effect_std,
                             alpha_eff, alternative)

  ctx <- .build_calc_context(
    test_spec       = test_spec,
    sample_sizes    = ss,
    effect_size     = effect_size,
    effect_size_std = effect_std,
    effect_method   = effect_method,
    effect_params   = extra,
    target_power    = target_power,
    achieved_power  = achieved,
    alpha           = alpha,
    alternative     = alternative
  )
  # Surface the adjusted alpha so the methods paragraph and the
  # reproducibility script use the right number.
  ctx$alpha_effective <- alpha_eff
  ctx
}

#' Sample-size sensitivity table for a test
#'
#' Produces the §2.1 Layout 1 sensitivity table: rows = effect-size
#' grid, columns = required N at each power threshold (typically
#' 0.80 and 0.90). The artifact most often pasted directly into
#' an NIH proposal.
#'
#' @inheritParams power_calc
#' @param effect_grid Numeric vector of effect sizes on the native
#'   scale. If NULL, uses the spec's `default_effect_grid` for the
#'   chosen `effect_method`.
#' @param power_thresholds Numeric vector of power thresholds for
#'   the table columns. Defaults to `c(0.80, 0.90)`.
#'
#' @return Tidy `data.frame` with columns `effect_size`,
#'   `effect_size_std`, and one `n_total_enrolled_at_<power>`
#'   column per power threshold (plus `n_total_evaluable_at_*`).
#'
#' @examples
#' \dontrun{
#' power_table("ttest_2groups",
#'             effect_grid = c(0.20, 0.50, 0.80),
#'             effect_method = "cohens_d",
#'             power_thresholds = c(0.80, 0.90))
#' }
#'
#' @export
power_table <- function(test, effect_grid = NULL,
                        power_thresholds = c(0.80, 0.90),
                        effect_method = NULL,
                        alpha       = 0.05,
                        alternative = "two.sided",
                        ...) {

  reg <- get_power_test_registry()
  test_spec <- if (is.character(test)) reg[[test]] else test
  if (is.null(test_spec)) {
    stop("unknown test: ", test, call. = FALSE)
  }

  effect_method <- effect_method %||% test_spec$effect_size_methods[1]
  if (is.null(effect_grid)) {
    effect_grid <- test_spec$default_effect_grid[[effect_method]]
    if (is.null(effect_grid)) {
      stop(sprintf("no default_effect_grid for method '%s' in test '%s'; supply `effect_grid`",
                   effect_method, test_spec$id), call. = FALSE)
    }
  }

  rows <- lapply(effect_grid, function(eff) {
    ctx <- power_calc(test_spec,
                      effect_size   = eff,
                      effect_method = effect_method,
                      target_power  = power_thresholds[1L],
                      alpha         = alpha,
                      alternative   = alternative,
                      ...)
    out <- list(
      effect_size      = eff,
      effect_size_std  = ctx$effect_size_std
    )
    for (p in power_thresholds) {
      ctx_p <- power_calc(test_spec,
                          effect_size   = eff,
                          effect_method = effect_method,
                          target_power  = p,
                          alpha         = alpha,
                          alternative   = alternative,
                          ...)
      ss <- ctx_p$sample_sizes
      pname <- formatC(p * 100, format = "d")
      out[[paste0("n_total_enrolled_p", pname)]]  <- ss$n_total_enrolled
      out[[paste0("n_total_evaluable_p", pname)]] <- ss$n_total_evaluable
    }
    out
  })

  do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
}

#' Format a data frame as a markdown table
#'
#' Minimal markdown-table formatter for the sensitivity-table
#' download (Gap 2) and the multi-aim aggregator (Gap 3).
#' Rounds numeric columns to a reasonable number of significant
#' digits for narrative use; leaves character columns untouched.
#'
#' @param df A data frame.
#' @param caption Optional caption rendered above the table.
#'
#' @return A single character string with embedded newlines.
#'
#' @keywords internal
.df_to_markdown <- function(df, caption = NULL) {
  if (nrow(df) == 0) return("")

  fmt <- function(x) {
    if (is.numeric(x)) {
      ifelse(is.na(x), "—",
             ifelse(abs(x - round(x)) < 1e-6,
                    format(round(x), big.mark = ",", trim = TRUE),
                    formatC(x, digits = 3, format = "g")))
    } else {
      as.character(x)
    }
  }

  cols  <- names(df)
  body  <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(cols, function(c) fmt(df[[c]][i]),
                              character(1)),
                       collapse = " | "), " |")
  }, character(1))

  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  align  <- paste0("|", paste(rep("---", length(cols)),
                              collapse = "|"), "|")

  out <- c(if (!is.null(caption)) paste0("**", caption, "**\n"),
           header, align, body)
  paste(out, collapse = "\n")
}

#' Render a methods-section paragraph from a calc_context
#'
#' Produces the Glueck-Muller-shaped paragraph that pastes into an
#' NIH proposal's Statistical Design and Power attachment or an
#' ICH E9 §3.5 sample-size statement. Composes seven sentences:
#' (1) test + outcome, (2) effect-size assumption with citation,
#' (3) alpha + power + required N, (4) dropout inflation,
#' (5) sensitivity sentence (ICH E9 §3.5; if `sensitivity_factor`
#' < 1), (6) software citation, (7) sex-as-biological-variable
#' paragraph (NIH rigor; if `include_sex_paragraph`).
#'
#' Each `calc_context` field is consumed at most once so the
#' function is safe to call repeatedly. Sentences for which the
#' relevant field is missing are dropped silently rather than
#' producing "(NA)" placeholders in the output.
#'
#' @param ctx A `calc_context` returned by `power_calc()` or
#'   `.build_calc_context()`.
#'
#' @return A single character string, paragraph-shaped (no
#'   embedded newlines).
#'
#' @keywords internal
.effect_method_phrase <- function(method) {
  switch(method,
    cohens_d          = "Cohen's d",
    cohens_f          = "Cohen's f",
    correlation       = "the correlation coefficient r",
    hazard_ratio      = "the hazard ratio",
    odds_ratio        = "the odds ratio",
    relative_risk     = "the relative risk",
    difference        = "the proportion difference",
    proportions       = "the treatment-arm proportion",
    percent_reduction = "the percent reduction",
    active_change     = "the treatment-arm change",
    slope_diff        = "the slope difference",
    discordant        = "the discordant-pair probability",
    prop_range        = "the highest-dose proportion",
    method
  )
}

.render_methods_paragraph <- function(ctx) {
  ss <- ctx$sample_sizes

  # Numeric formatters that round defensively for narrative use.
  pct  <- function(x) sprintf("%.0f%%", x * 100)
  num0 <- function(x) format(round(x), big.mark = ",")
  num2 <- function(x) {
    if (is.na(x)) "NA" else sprintf("%.2f", x)
  }
  num3 <- function(x) {
    if (is.na(x)) "NA" else format(signif(x, 3), drop0trailing = TRUE)
  }
  trim_period <- function(x) sub("\\.+\\s*$", "", x)

  # Use Bonferroni-adjusted alpha if the spec declared a
  # multiplicity adjustment (e.g. ANOVA with pairwise contrasts);
  # otherwise the raw alpha. The paragraph mentions both when
  # they differ so reviewers see the adjustment applied.
  alpha_eff <- ctx$alpha_effective %||% ctx$alpha
  alpha_str <- num3(alpha_eff)
  alpha_note <- if (!isTRUE(all.equal(alpha_eff, ctx$alpha))) {
    sprintf(" (Bonferroni-adjusted from %s)", num3(ctx$alpha))
  } else {
    ""
  }
  alt_str   <- if (identical(ctx$alternative, "two.sided")) {
    "two-sided"
  } else {
    "one-sided"
  }

  # Sample-size mode if the user supplied a target_power;
  # otherwise power mode. The S3 phrasing flips accordingly.
  is_ss_mode <- !is.null(ctx$target_power)

  citation_clause <- if (nzchar(ctx$effect_source %||% "")) {
    if (nzchar(ctx$effect_doi %||% "")) {
      sprintf("(%s; %s)", ctx$effect_source, ctx$effect_doi)
    } else {
      sprintf("(%s)", ctx$effect_source)
    }
  } else {
    "(pilot data, citation pending)"
  }

  # Sentence 1: planned analysis.
  s1 <- sprintf("We plan to use a %s as the primary analysis.",
                ctx$test_name)

  # Sentence 2: effect-size assumption + citation.
  effect_label <- .effect_method_phrase(ctx$effect_method %||% "")
  if (!is.na(ctx$effect_size_std) &&
      !isTRUE(all.equal(ctx$effect_size_std, ctx$effect_size))) {
    s2 <- sprintf(
      paste0("Based on prior data %s, we assume %s = %s ",
             "(standardised: %s)."),
      citation_clause, effect_label,
      num3(ctx$effect_size),
      num2(ctx$effect_size_std)
    )
  } else {
    s2 <- sprintf(
      "Based on prior data %s, we assume %s = %s.",
      citation_clause, effect_label, num3(ctx$effect_size)
    )
  }

  # Sentence 3: alpha + power + N (phrasing flips by mode).
  if (is_ss_mode) {
    power_str <- pct(ctx$target_power)
    if (ss$n_arms == 1L) {
      s3 <- sprintf(
        paste0("For α=%s%s (%s) and a target power of %s, ",
               "%s evaluable participants are required."),
        alpha_str, alpha_note, alt_str, power_str,
        num0(ss$n_total_evaluable)
      )
    } else {
      s3 <- sprintf(
        paste0("For α=%s%s (%s) and a target power of %s, ",
               "%s evaluable participants per arm are required ",
               "(%s total)."),
        alpha_str, alpha_note, alt_str, power_str,
        num0(ss$n_per_arm_evaluable[1]),
        num0(ss$n_total_evaluable)
      )
    }
  } else {
    achieved_str <- if (!is.na(ctx$achieved_power %||% NA_real_)) {
      pct(ctx$achieved_power)
    } else {
      "NA"
    }
    if (ss$n_arms == 1L) {
      s3 <- sprintf(
        paste0("With %s evaluable participants and α=%s%s (%s), ",
               "the achieved power is %s."),
        num0(ss$n_total_evaluable),
        alpha_str, alpha_note, alt_str, achieved_str
      )
    } else {
      s3 <- sprintf(
        paste0("With %s evaluable participants per arm (%s total) ",
               "and α=%s%s (%s), the achieved power is %s."),
        num0(ss$n_per_arm_evaluable[1]),
        num0(ss$n_total_evaluable),
        alpha_str, alpha_note, alt_str, achieved_str
      )
    }
  }

  # Sentence 4: dropout inflation (skip if dropout = 0).
  s4 <- if (!is.null(ss$dropout) && ss$dropout > 0) {
    if (ss$n_arms == 1L) {
      sprintf(
        paste0("Accounting for %s dropout, total enrolment will be ",
               "%s."),
        pct(ss$dropout), num0(ss$n_total_enrolled)
      )
    } else {
      sprintf(
        paste0("Accounting for %s dropout, total enrolment will be ",
               "%s (%s per arm)."),
        pct(ss$dropout),
        num0(ss$n_total_enrolled),
        num0(ss$n_per_arm_enrolled[1])
      )
    }
  } else {
    ""
  }

  # Sentence 5: sensitivity scenario (Gap 6).
  s5 <- if (!is.null(ctx$sensitivity_factor) &&
            ctx$sensitivity_factor < 1 &&
            !is.na(ctx$effect_size_std)) {
    smaller_std <- ctx$effect_size_std * ctx$sensitivity_factor
    smaller_raw <- ctx$effect_size      * ctx$sensitivity_factor
    pct_smaller <- round((1 - ctx$sensitivity_factor) * 100)

    # Recompute achieved power at the smaller effect.
    reg <- get_power_test_registry()
    test_spec <- reg[[ctx$test_id]]
    achieved_smaller <- if (is.null(test_spec)) {
      NA_real_
    } else {
      .compute_power(test_spec, ss, smaller_std,
                     alpha = ctx$alpha,
                     alternative = ctx$alternative)
    }

    if (is.na(achieved_smaller)) {
      ""
    } else {
      sprintf(
        paste0("If the true effect is %d%% smaller (e.g. %s on the ",
               "native scale), the achieved power at the proposed ",
               "sample size drops to %s."),
        pct_smaller, num3(smaller_raw), pct(achieved_smaller)
      )
    }
  } else {
    ""
  }

  # Sentence 6: software + formula citation.
  s6 <- sprintf(
    paste0("Power was computed using zzpower (Thomas, R.G. 2026); ",
           "the calculation follows %s."),
    trim_period(ctx$formula_citation)
  )

  # Sentence 7: sex-as-biological-variable paragraph (Gap 12).
  s7 <- if (isTRUE(ctx$include_sex_paragraph)) {
    paste0("Recruitment will be stratified to ensure approximately ",
           "equal representation of male and female participants. ",
           "Sex-disaggregated secondary analyses will be conducted; ",
           "the primary analysis is not powered for a ",
           "sex × treatment interaction.")
  } else {
    ""
  }

  parts <- c(s1, s2, s3, s4, s5, s6, s7)
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Render a runnable reproducibility R script
#'
#' Emits a paste-ready block of R code that, run in a fresh R
#' session with the relevant package(s) installed, exactly
#' reproduces the headline N (sample-size mode) or achieved
#' power (power mode) for the calc_context. NIMH item 4
#' ("not sufficient to merely cite the software") made literal.
#'
#' For pwr-backed tests the script calls `pwr::pwr.<test>(...)`
#' directly with literal numeric arguments. For zzpower's custom
#' helpers (logrank_power, mcnemar_power, mixed_model_power,
#' trend_power) the script calls `zzpower::<helper>(...)` — those
#' are exported precisely so this script works.
#'
#' @param ctx A calc_context returned by `power_calc()`.
#' @param fence Logical; wrap output in a Markdown ```r fence.
#'   Default `FALSE` returns plain R lines for embedding into a
#'   text report; `TRUE` is appropriate for the markdown / HTML
#'   reports.
#'
#' @return A single character string with embedded newlines.
#'
#' @keywords internal
.render_repro_script <- function(ctx, fence = FALSE) {
  reg <- get_power_test_registry()
  test_spec <- reg[[ctx$test_id]]
  if (is.null(test_spec)) return("")

  call_name <- test_spec$repro_call %||% ""
  if (!nzchar(call_name)) return("")

  # Mirror the call assembly used by .compute_power so the
  # arguments emitted are exactly the ones used at runtime.
  fn <- test_spec$power_function
  fn_formals <- names(formals(fn))
  es_param <- intersect(c("d", "h", "r", "f"), fn_formals)[1]

  ss <- ctx$sample_sizes
  # Use the Bonferroni-adjusted alpha (Gap 11) if the spec
  # supplied one; otherwise the raw alpha.
  args <- list(sig.level = ctx$alpha_effective %||% ctx$alpha)
  if ("alternative" %in% fn_formals) {
    args$alternative <- ctx$alternative
  }
  if (!is.na(es_param)) args[[es_param]] <- ctx$effect_size_std

  if (!is.null(ss$n1) && "n1" %in% fn_formals) {
    args$n1 <- ss$n1
    args$n2 <- ss$n2
  } else if (!is.null(ss$n) && "n" %in% fn_formals) {
    args$n <- ss$n
  }

  if ("type" %in% fn_formals && !is.null(test_spec$test_type)) {
    args$type <- test_spec$test_type
  }

  if (!is.null(test_spec$power_args)) {
    for (nm in names(test_spec$power_args)) {
      if (nm %in% fn_formals) {
        val <- test_spec$power_args[[nm]]
        args[[nm]] <- if (is.character(val) &&
                          val %in% names(ss)) {
          ss[[val]]
        } else {
          val
        }
      }
    }
  }

  # Reorder args to match the function's signature so the rendered
  # call reads naturally (e.g. d, n1, n2, sig.level, alternative
  # rather than alphabetic insertion order).
  ordered <- intersect(fn_formals, names(args))
  args <- args[ordered]

  fmt_arg <- function(x) {
    if (is.character(x)) sprintf('"%s"', x)
    else if (is.numeric(x)) {
      format(round(x, 4), trim = TRUE,
             drop0trailing = FALSE, scientific = FALSE)
    } else format(x)
  }

  arg_lines <- vapply(seq_along(args), function(i) {
    sep <- if (i == length(args)) "" else ","
    sprintf("  %-12s = %s%s", names(args)[i],
            fmt_arg(args[[i]]), sep)
  }, character(1))

  pkg <- if (grepl("^pwr::", call_name)) "pwr" else "zzpower"

  ts_str <- format(Sys.time(), "%Y-%m-%d")
  result_comment <- if (!is.na(ctx$achieved_power %||% NA_real_)) {
    sprintf("# Achieved power: %.3f", ctx$achieved_power)
  } else {
    "# Achieved power computed from result$power"
  }

  enrolled_comment <- sprintf(
    "# Total enrolled: %s (with %.0f%% dropout)",
    format(round(ss$n_total_enrolled), big.mark = ","),
    (ss$dropout %||% 0) * 100
  )

  body <- c(
    sprintf("# Reproducibility script for %s", test_spec$name),
    sprintf("# Generated by zzpower v%s on %s",
            tryCatch(as.character(utils::packageVersion("zzpower")),
                     error = function(e) "0.x"),
            ts_str),
    "",
    sprintf("library(%s)", pkg),
    "",
    sprintf("result <- %s(", call_name),
    arg_lines,
    ")",
    "print(result)",
    "",
    result_comment,
    enrolled_comment
  )

  if (fence) {
    paste(c("```r", body, "```"), collapse = "\n")
  } else {
    paste(body, collapse = "\n")
  }
}

#' Get the complete test registry
#'
#' @return List of all available power tests with their configurations
#'
#' @keywords internal
#' @export
get_power_test_registry <- function() {
  list(
    ttest_2groups = create_ttest_2groups_spec(),
    ttest_paired = create_ttest_paired_spec(),
    ttest_one_sample = create_ttest_one_sample_spec(),
    prop_2groups = create_prop_2groups_spec(),
    correlation = create_correlation_spec(),
    logrank = create_logrank_spec(),
    fisher_exact = create_fisher_exact_spec(),
    trend_prop = create_trend_prop_spec(),
    anova_oneway = create_anova_oneway_spec(),
    mcnemar = create_mcnemar_spec(),
    mixed_model = create_mixed_model_spec(),
    cluster_rct = create_cluster_rct_spec(),
    cluster_prop = create_cluster_prop_spec(),
    cluster_logrank = create_cluster_logrank_spec()
  )
}

#' Two-Group Independent t-test Specification
#'
#' @keywords internal
create_ttest_2groups_spec <- function() {
  list(
    id = "ttest_2groups",
    name = "Two-Group t-test",
    description = "Independent samples t-test for continuous outcomes",
    icon = "bar-chart-line",
    power_function = pwr::pwr.t2n.test,
    effect_size_methods = c("cohens_d", "percent_reduction", "difference", "active_change"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 2 (Cohen's d, two-sample t-test; ex. 2.10 p. 59). Implementation: pwr::pwr.t2n.test (Champely 2020), based on Dalgaard's stats::power.t.test.",
    default_effect_grid = list(
      cohens_d  = c(0.2, 0.5, 0.8),
      difference = c(2, 5, 8),
      percent_reduction = c(0.10, 0.25, 0.40)
    ),
    paragraph_template = NULL,  # populated in Wave 2 (Gap 1)
    repro_call         = "pwr::pwr.t2n.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 500, default = 100, step = 10,
        description = "Total number of participants in both groups"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      ),
      allocation = list(
        type = "radio",
        label = "Sample Size Allocation",
        options = c("equal", "unequal"),
        default = "equal",
        description = "Equal vs unequal group sizes"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio (n1/n2)",
        min = 0.5, max = 5, default = 1, step = 0.1,
        condition = "allocation == 'unequal'",
        description = "Ratio of group 1 to group 2 sample sizes"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size in SD Units"
      ),
      percent_reduction = list(
        min = 0, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Percent Reduction",
        requires = list(sd0 = list(
          type = "numeric", label = "Placebo SD", default = 10
        ))
      ),
      difference = list(
        min = 0, max = 10, default_min = 1, default_max = 5,
        label = "Difference in Scores",
        requires = list(sd0 = list(
          type = "numeric", label = "SD", default = 10
        ))
      ),
      active_change = list(
        min = 0, max = 10, default_min = 0, default_max = 6,
        label = "Treatment Change",
        requires = list(
          d0 = list(type = "numeric", label = "Placebo Change", default = 10),
          sd0 = list(type = "numeric", label = "SD", default = 10)
        )
      )
    ),

    standardize = function(effect_sizes, method, params) {
      # Convert all effect size methods to Cohen's d
      switch(method,
        "cohens_d" = effect_sizes,
        "percent_reduction" = effect_sizes / (params$sd0 %||% 10),
        "difference" = effect_sizes / (params$sd0 %||% 10),
        "active_change" = ((params$d0 %||% 10) - effect_sizes) / (params$sd0 %||% 10),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 100
      allocation <- input$allocation %||% "equal"
      dropout <- input$dropout %||% 0.1

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm, dropout = dropout)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Paired t-test Specification
#'
#' @keywords internal
create_ttest_paired_spec <- function() {
  list(
    id = "ttest_paired",
    name = "Paired t-test",
    description = "Paired samples t-test for before-after designs",
    icon = "arrow-repeat",
    power_function = pwr::pwr.t.test,
    test_type = "paired",
    effect_size_methods = c("cohens_d"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 2 (Cohen's d, paired; example p. 50). Implementation: pwr::pwr.t.test type='paired' (Champely 2020), based on Dalgaard's stats::power.t.test.",
    default_effect_grid = list(cohens_d = c(0.2, 0.5, 0.8)),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.t.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of paired observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size (Cohen's d)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 50
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Pairs"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' One-Sample t-test Specification
#'
#' @keywords internal
create_ttest_one_sample_spec <- function() {
  list(
    id = "ttest_one_sample",
    name = "One-Sample t-test",
    description = "One-sample t-test comparing to a fixed value",
    icon = "bullseye",
    power_function = pwr::pwr.t.test,
    test_type = "one.sample",
    effect_size_methods = c("cohens_d"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 2 (Cohen's d, one-sample; ex. 2.5 p. 47). Implementation: pwr::pwr.t.test type='one.sample' (Champely 2020), based on Dalgaard's stats::power.t.test.",
    default_effect_grid = list(cohens_d = c(0.2, 0.5, 0.8)),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.t.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 500, default = 50, step = 10,
        description = "Number of observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size (Cohen's d)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 50
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Sample"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 50) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Two Proportions Specification
#'
#' @keywords internal
create_prop_2groups_spec <- function() {
  list(
    id = "prop_2groups",
    name = "Two Proportions",
    description = "Comparison of two binary outcomes (proportions)",
    icon = "percent",
    power_function = pwr::pwr.2p2n.test,
    effect_size_methods = c("proportions", "difference", "odds_ratio", "relative_risk"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 6 (Cohen's h, two-proportion test; ex. 6.1 p. 198, ex. 6.3 p. 200, ex. 6.7 p. 207). Effect size h = 2·asin(√p1) − 2·asin(√p2) is Fisher's variance-stabilising arcsine transform (Fisher 1922). Implementation: pwr::pwr.2p2n.test (Champely 2020).",
    default_effect_grid = list(
      difference    = c(0.05, 0.10, 0.15, 0.20),
      odds_ratio    = c(1.25, 1.50, 2.00, 3.00),
      relative_risk = c(1.25, 1.50, 2.00, 3.00),
      proportions   = c(0.40, 0.50, 0.60)
    ),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.2p2n.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 200, step = 10,
        description = "Total number of participants"
      ),
      allocation = list(
        type = "radio",
        label = "Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio",
        min = 0.5, max = 5, default = 1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      proportions = list(
        min = 0.01, max = 0.99, default_min = 0.3, default_max = 0.5,
        label = "Group Proportions (p1, p2)"
      ),
      difference = list(
        min = -0.5, max = 0.5, default_min = -0.3, default_max = -0.05,
        label = "Proportion Difference"
      ),
      odds_ratio = list(
        min = 0.1, max = 10, default_min = 1.2, default_max = 3,
        label = "Odds Ratio (OR)"
      ),
      relative_risk = list(
        min = 0.1, max = 10, default_min = 1.2, default_max = 3,
        label = "Relative Risk (RR)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      baseline <- params$baseline %||% 0.5
      switch(method,
        "proportions" = sapply(effect_sizes, function(p1) {
          prop_to_cohens_h(p1, params$p2 %||% 0.3)
        }),
        "difference" = sapply(effect_sizes, function(d) {
          diff_to_cohens_h(d, baseline)
        }),
        "odds_ratio" = sapply(effect_sizes, function(or) {
          or_to_cohens_h(or, baseline)
        }),
        "relative_risk" = sapply(effect_sizes, function(rr) {
          rr_to_cohens_h(rr, baseline)
        }),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 200
      allocation <- input$allocation %||% "equal"

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Correlation Test Specification
#'
#' @keywords internal
create_correlation_spec <- function() {
  list(
    id = "correlation",
    name = "Correlation Test",
    description = "Test for correlation between two variables",
    icon = "diagram-2",
    power_function = pwr::pwr.r.test,
    effect_size_methods = c("correlation"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 3 (Pearson r; ex. 3.1 p. 96, ex. 3.4 p. 208). Uses Fisher's Z' transform (Fisher 1915) with bias correction; pwr::pwr.r.test applies a one-sided correction that diverges from Cohen 1988 p. 546 — see pwr docs for the exact form. Implementation: pwr::pwr.r.test (Champely 2020), modified bias correction by Jeffrey Gill.",
    default_effect_grid = list(correlation = c(0.10, 0.30, 0.50)),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.r.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Sample Size",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of observations"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      correlation = list(
        min = 0, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Correlation Coefficient (r)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 100
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Sample"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Survival Log-rank Test Specification
#'
#' Power analysis for comparing two survival curves using the log-rank
#' test. Uses the Schoenfeld (1981) formula. The event probability
#' parameter converts total sample sizes to expected event counts
#' before power calculation.
#'
#' @keywords internal
create_logrank_spec <- function() {
  list(
    id = "logrank",
    name = "Survival Log-rank",
    description = "Log-rank test comparing two survival curves",
    icon = "hourglass-split",
    power_function = logrank_power,
    effect_size_methods = c("hazard_ratio"),
    formula_citation = "Schoenfeld DA (1981). The asymptotic properties of nonparametric tests for comparing survival distributions. Biometrika, 68(1), 316-319.",
    default_effect_grid = list(hazard_ratio = c(0.50, 0.65, 0.75)),
    paragraph_template = NULL,
    repro_call         = "zzpower::logrank_power",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 200, step = 10,
        description = "Total participants across both groups"
      ),
      event_prob = list(
        type = "slider",
        label = "Event Probability",
        min = 0.1, max = 1, default = 0.7, step = 0.05,
        description = "Probability of observing the event"
      ),
      allocation = list(
        type = "radio",
        label = "Sample Size Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio (n1/n2)",
        min = 0.5, max = 5, default = 1, step = 0.1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      hazard_ratio = list(
        min = 0.1, max = 5, default_min = 1.2, default_max = 3.0,
        label = "Hazard Ratio (HR)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      log(effect_sizes)
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 200
      event_prob <- input$event_prob %||% 0.7
      allocation <- input$allocation %||% "equal"
      dropout <- input$dropout %||% 0.1

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      result <- .canonicalize_sample_sizes(per_arm, dropout = dropout)

      # Schoenfeld's log-rank formula expects expected event counts,
      # not participants. Override the back-compat n1/n2 scalars
      # consumed by the power function with events; canonical
      # n_per_arm_* fields continue to report participants.
      result$n1 <- result$n_per_arm_evaluable[1] * event_prob
      result$n2 <- result$n_per_arm_evaluable[2] * event_prob
      result$expected_events_total <-
        result$n_total_evaluable * event_prob

      result
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 200) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      ep <- input$event_prob %||% 0.7
      if (ep <= 0 || ep > 1) {
        issues <- c(issues, "Event probability must be between 0 and 1")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Fisher's Exact Test Specification
#'
#' Power analysis for Fisher's exact test on 2x2 contingency tables.
#' Uses the normal approximation to the exact test (equivalent to
#' pwr.2p2n.test with Cohen's h). Appropriate for small-sample
#' studies with binary outcomes.
#'
#' @keywords internal
create_fisher_exact_spec <- function() {
  list(
    id = "fisher_exact",
    name = "Fisher's Exact Test",
    description = "Exact test for 2x2 contingency tables",
    icon = "grid-3x2",
    power_function = pwr::pwr.2p2n.test,
    effect_size_methods = c("proportions", "odds_ratio"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 6 (Cohen's h normal approximation to Fisher's exact test; ex. 6.5 p. 203). Implementation: pwr::pwr.2p2n.test (Champely 2020).",
    default_effect_grid = list(
      proportions = c(0.20, 0.35, 0.50),
      odds_ratio  = c(2.0, 4.0, 8.0)
    ),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.2p2n.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 10, max = 200, default = 60, step = 2,
        description = "Total participants in both groups"
      ),
      allocation = list(
        type = "radio",
        label = "Allocation",
        options = c("equal", "unequal"),
        default = "equal"
      ),
      ratio = list(
        type = "numeric",
        label = "Group Ratio",
        min = 0.5, max = 5, default = 1,
        condition = "allocation == 'unequal'"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      proportions = list(
        min = 0.01, max = 0.99, default_min = 0.1, default_max = 0.5,
        label = "Treatment Proportion (p1)",
        requires = list(p2 = list(
          type = "numeric", label = "Control Proportion (p2)", default = 0.1
        ))
      ),
      odds_ratio = list(
        min = 0.1, max = 20, default_min = 2, default_max = 8,
        label = "Odds Ratio (OR)",
        requires = list(baseline = list(
          type = "numeric", label = "Baseline Proportion", default = 0.1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      switch(method,
        "proportions" = sapply(effect_sizes, function(p1) {
          prop_to_cohens_h(p1, params$p2 %||% 0.1)
        }),
        "odds_ratio" = sapply(effect_sizes, function(or) {
          or_to_cohens_h(or, params$baseline %||% 0.1)
        }),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 60
      allocation <- input$allocation %||% "equal"

      if (allocation == "equal") {
        per_arm <- c(total_n / 2, total_n / 2)
      } else {
        ratio <- input$ratio %||% 1
        per_arm <- c(ratio * total_n / (ratio + 1),
                     total_n / (ratio + 1))
      }

      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 60) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Trend in Proportions Specification (Cochran-Armitage)
#'
#' Power analysis for the Cochran-Armitage test for linear trend in
#' proportions across ordered dose groups. The standardized effect
#' size is derived from the expected proportions at the lowest and
#' highest dose levels assuming a linear dose-response relationship.
#'
#' @keywords internal
create_trend_prop_spec <- function() {
  list(
    id = "trend_prop",
    name = "Trend in Proportions",
    description = "Cochran-Armitage test for dose-response trend",
    icon = "graph-up-arrow",
    power_function = trend_power,
    effect_size_methods = c("prop_range"),
    formula_citation = "Cochran WG (1954). Some methods for strengthening the common chi-squared tests. Biometrics, 10(4), 417-451; Armitage P (1955). Tests for linear trends in proportions and frequencies. Biometrics, 11(3), 375-386.",
    default_effect_grid = list(prop_range = c(0.30, 0.45, 0.60)),
    paragraph_template = NULL,
    repro_call         = "zzpower::trend_power",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 30, max = 1000, default = 150, step = 10,
        description = "Total participants across all dose groups"
      ),
      n_groups = list(
        type = "slider",
        label = "Number of Dose Groups",
        min = 3, max = 8, default = 3, step = 1,
        description = "Number of ordered dose levels"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      prop_range = list(
        min = 0.01, max = 0.99, default_min = 0.2, default_max = 0.6,
        label = "Proportion at Highest Dose",
        requires = list(p_low = list(
          type = "numeric",
          label = "Proportion at Lowest Dose",
          default = 0.1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      p_low <- params$p_low %||% 0.1
      k <- params$n_groups %||% 3

      sapply(effect_sizes, function(p_high) {
        if (k < 2) return(0)
        doses <- seq_len(k) - (k + 1) / 2
        props <- p_low + (seq_len(k) - 1) / (k - 1) * (p_high - p_low)
        p_bar <- mean(props)
        if (p_bar <= 0 || p_bar >= 1) return(0)

        num <- sum(doses * props)
        denom <- p_bar * (1 - p_bar) * sum(doses^2)
        if (denom <= 0) return(0)

        sqrt(num^2 / (k * denom))
      })
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 150
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Total"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 150) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 3) {
        issues <- c(issues, "At least 3 dose groups required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' One-Way ANOVA Specification
#'
#' Power analysis for one-way analysis of variance comparing means
#' across k independent groups. Uses Cohen's f as the effect size.
#'
#' @keywords internal
create_anova_oneway_spec <- function() {
  list(
    id = "anova_oneway",
    name = "One-Way ANOVA",
    description = "Compare means across multiple independent groups",
    icon = "bar-chart-steps",
    power_function = pwr::pwr.anova.test,
    power_args = list(k = "k"),
    effect_size_methods = c("cohens_f"),
    formula_citation = "Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences, 2nd ed., Lawrence Erlbaum, ch. 8 (Cohen's f, balanced one-way ANOVA; ex. 8.1 p. 357, ex. 8.10 p. 391). Implementation: pwr::pwr.anova.test (Champely 2020), based on Dalgaard's stats::power.anova.test.",
    default_effect_grid = list(cohens_f = c(0.10, 0.25, 0.40)),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.anova.test",

    # Gap 11: Bonferroni-aware alpha. When the user plans
    # `n_pairwise_contrasts` post-hoc pairwise comparisons, divide
    # α by that count so the omnibus F is solved at the
    # family-wise-controlled level. The omnibus F itself does not
    # need a Bonferroni correction; this readout is conservative
    # (more N than strictly needed for the omnibus) but supports
    # the protocol-level commitment to the multiplicity correction
    # carried into the pairwise post-hoc contrasts.
    effective_alpha = function(input, alpha) {
      contrasts <- max(1L,
                        as.integer(input$n_pairwise_contrasts %||% 1L))
      alpha / contrasts
    },

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 20, max = 1000, default = 120, step = 10,
        description = "Total participants across all groups"
      ),
      n_groups = list(
        type = "slider",
        label = "Number of Groups",
        min = 2, max = 10, default = 3, step = 1,
        description = "Number of independent groups"
      ),
      n_pairwise_contrasts = list(
        type = "numeric",
        label = "Number of Pairwise Contrasts (Bonferroni)",
        min = 1, max = 45, default = 1, step = 1,
        description = "Post-hoc comparisons to control with Bonferroni; 1 = omnibus only"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_f = list(
        min = 0.01, max = 1, default_min = 0.1, default_max = 0.5,
        label = "Effect Size (Cohen's f)"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      effect_sizes
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 120
      k <- input$n_groups %||% 3
      per_arm <- rep(total_n / k, k)
      .canonicalize_sample_sizes(
        per_arm_enrolled = per_arm,
        dropout    = input$dropout %||% 0.1,
        arm_labels = paste("Group", seq_len(k))
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 120) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_groups %||% 3) < 2) {
        issues <- c(issues, "At least 2 groups required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' McNemar Test Specification (Paired Proportions)
#'
#' Power analysis for McNemar's test comparing paired binary outcomes.
#' Uses the normal approximation from Connor (1987). The user specifies
#' the two discordant cell probabilities (p10 and p01).
#'
#' @keywords internal
create_mcnemar_spec <- function() {
  list(
    id = "mcnemar",
    name = "McNemar Test",
    description = "Paired proportions (before-after binary outcomes)",
    icon = "arrow-left-right",
    power_function = mcnemar_power,
    effect_size_methods = c("discordant"),
    formula_citation = "Connor RJ (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics, 43(1), 207-211.",
    default_effect_grid = list(discordant = c(0.10, 0.20, 0.30)),
    paragraph_template = NULL,
    repro_call         = "zzpower::mcnemar_power",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Number of Pairs",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Number of matched pairs"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      discordant = list(
        min = 0.01, max = 0.5, default_min = 0.05, default_max = 0.25,
        label = "P(+Treatment, -Control): p10",
        requires = list(p01 = list(
          type = "numeric",
          label = "P(-Treatment, +Control): p01",
          default = 0.05
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      p01 <- params$p01 %||% 0.05

      sapply(effect_sizes, function(p10) {
        p_disc <- p10 + p01
        p_diff <- p10 - p01
        denom <- p_disc - p_diff^2
        if (denom <= 0 || p_disc <= 0) return(0)
        abs(p_diff) / sqrt(denom)
      })
    },

    sample_size_calc = function(input) {
      n <- input$sample_size %||% 100
      .canonicalize_sample_sizes(
        per_arm_enrolled = n,
        dropout    = input$dropout %||% 0.1,
        arm_labels = "Pairs"
      )
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Number of pairs must be positive")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Longitudinal Mixed Model Specification
#'
#' Power analysis for a two-group comparison of slopes in a linear
#' mixed model with equally spaced time points and compound symmetry
#' correlation structure. Based on Diggle et al. (2002, p. 29).
#'
#' @keywords internal
create_mixed_model_spec <- function() {
  list(
    id = "mixed_model",
    name = "Mixed Model (Slopes)",
    description = "Longitudinal two-group comparison of rates of change",
    icon = "graph-up",
    power_function = mixed_model_power,
    effect_size_methods = c("slope_diff"),
    formula_citation = "Diggle PJ, Heagerty P, Liang K-Y, Zeger SL (2002). Analysis of Longitudinal Data, 2nd ed., Oxford University Press, p. 29.",
    default_effect_grid = list(slope_diff = c(0.20, 0.50, 1.00)),
    paragraph_template = NULL,
    repro_call         = "zzpower::mixed_model_power",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Sample Size",
        min = 10, max = 1000, default = 100, step = 10,
        description = "Total participants across both groups"
      ),
      n_timepoints = list(
        type = "slider",
        label = "Number of Time Points",
        min = 3, max = 20, default = 5, step = 1,
        description = "Measurement occasions per subject"
      ),
      correlation = list(
        type = "slider",
        label = "Within-Subject Correlation",
        min = 0.05, max = 0.95, default = 0.5, step = 0.05,
        description = "Compound symmetry correlation (rho)"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      slope_diff = list(
        min = 0.01, max = 5, default_min = 0.1, default_max = 1.0,
        label = "Slope Difference (delta)",
        requires = list(sigma = list(
          type = "numeric",
          label = "Residual SD (sigma)",
          default = 1
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      sigma <- params$sigma %||% 1
      rho <- params$correlation %||% 0.5
      m <- params$n_timepoints %||% 5

      t_vals <- seq(0, 1, length.out = m)
      s_tt <- sum((t_vals - mean(t_vals))^2)

      effect_sizes * sqrt(s_tt / (2 * sigma^2 * (1 - rho)))
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 100
      per_arm <- c(total_n / 2, total_n / 2)
      .canonicalize_sample_sizes(per_arm,
                                 dropout = input$dropout %||% 0.1)
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 100) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$n_timepoints %||% 5) < 3) {
        issues <- c(issues, "At least 3 time points required")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Cluster-Randomized RCT Specification (continuous outcome)
#'
#' Two-group cluster-randomized trial with a continuous outcome.
#' Uses Cohen's d on the per-arm effective sample size, where the
#' design effect DE = 1 + (m̄ − 1) × ICC inflates the variance.
#' The user enters total participants (across both arms); the
#' power function consumes the effective N = N / DE.
#'
#' Modeled after the cluster-RCT design in NCI sample R01
#' R01CA177592 (Mohile et al., reducing chemotherapy toxicity in
#' older adults; see grant-proposals-sample-size-practice.md
#' §1.4 for the exemplar paragraph). NIH cluster-randomized
#' trials require this adjustment per the "special methods are
#' required" clause for trials that randomize groups.
#'
#' @keywords internal
create_cluster_rct_spec <- function() {
  list(
    id = "cluster_rct",
    name = "Cluster-Randomized RCT (continuous)",
    description = "Cluster-randomized two-group trial with continuous outcome",
    icon = "diagram-3",
    power_function = pwr::pwr.t2n.test,
    effect_size_methods = c("cohens_d", "difference"),
    formula_citation = "Cohen J (1988) ch. 2 (Cohen's d) with design-effect inflation DE = 1 + (m̄ − 1) × ICC (Donner & Klar 2000, Design and Analysis of Cluster Randomization Trials in Health Research, Arnold). Implementation: pwr::pwr.t2n.test on N_eff = N / DE.",
    default_effect_grid = list(
      cohens_d   = c(0.2, 0.5, 0.8),
      difference = c(2, 5, 8)
    ),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.t2n.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Participants",
        min = 40, max = 5000, default = 400, step = 20,
        description = "Total participants across both arms (pre-DE inflation)"
      ),
      m_cluster = list(
        type = "slider",
        label = "Mean Cluster Size",
        min = 2, max = 200, default = 30, step = 1,
        description = "Average number of participants per cluster"
      ),
      icc = list(
        type = "slider",
        label = "Intracluster Correlation (ICC)",
        min = 0, max = 0.5, default = 0.05, step = 0.01,
        description = "Within-cluster correlation"
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05,
        description = "Expected dropout rate"
      )
    ),

    effect_size_params = list(
      cohens_d = list(
        min = 0, max = 2, default_min = 0.2, default_max = 1.0,
        label = "Effect Size (Cohen's d)"
      ),
      difference = list(
        min = 0, max = 10, default_min = 1, default_max = 5,
        label = "Difference in Scores",
        requires = list(sd0 = list(
          type = "numeric", label = "SD", default = 10
        ))
      )
    ),

    standardize = function(effect_sizes, method, params) {
      switch(method,
        "cohens_d"   = effect_sizes,
        "difference" = effect_sizes / (params$sd0 %||% 10),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 400
      m       <- input$m_cluster %||% 30
      icc     <- input$icc %||% 0.05
      dropout <- input$dropout %||% 0.1

      DE <- 1 + (m - 1) * icc

      per_arm <- c(total_n / 2, total_n / 2)
      result <- .canonicalize_sample_sizes(per_arm, dropout = dropout)

      # Override back-compat n1/n2 with effective N for power calc
      # (same pattern as logrank). Canonical participant counts
      # remain in n_per_arm_evaluable / n_per_arm_enrolled.
      result$n1 <- result$n_per_arm_evaluable[1] / DE
      result$n2 <- result$n_per_arm_evaluable[2] / DE

      result$design_effect       <- DE
      result$icc                 <- icc
      result$m_cluster           <- m
      result$n_clusters_per_arm  <- per_arm[1] / m

      result
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 400) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$m_cluster %||% 30) < 2) {
        issues <- c(issues, "Cluster size must be at least 2")
      }
      icc <- input$icc %||% 0.05
      if (icc < 0 || icc > 1) {
        issues <- c(issues, "ICC must be between 0 and 1")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Cluster-Randomized Two-Proportion Specification
#'
#' Two-group cluster-randomized trial with a binary outcome.
#' Cohen's h on the per-arm effective sample size; design effect
#' inflates variance via DE = 1 + (m̄ − 1) × ICC.
#'
#' @keywords internal
create_cluster_prop_spec <- function() {
  list(
    id = "cluster_prop",
    name = "Cluster-Randomized RCT (proportion)",
    description = "Cluster-randomized two-group trial with binary outcome",
    icon = "diagram-3-fill",
    power_function = pwr::pwr.2p2n.test,
    effect_size_methods = c("difference", "odds_ratio"),
    formula_citation = "Cohen J (1988) ch. 6 (Cohen's h) with design-effect inflation DE = 1 + (m̄ − 1) × ICC (Donner & Klar 2000). Implementation: pwr::pwr.2p2n.test on N_eff = N / DE.",
    default_effect_grid = list(
      difference = c(0.05, 0.10, 0.15),
      odds_ratio = c(1.5, 2.0, 3.0)
    ),
    paragraph_template = NULL,
    repro_call         = "pwr::pwr.2p2n.test",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Participants",
        min = 40, max = 5000, default = 400, step = 20
      ),
      m_cluster = list(
        type = "slider",
        label = "Mean Cluster Size",
        min = 2, max = 200, default = 30, step = 1
      ),
      icc = list(
        type = "slider",
        label = "Intracluster Correlation (ICC)",
        min = 0, max = 0.5, default = 0.05, step = 0.01
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05
      )
    ),

    effect_size_params = list(
      difference = list(
        min = -0.5, max = 0.5, default_min = -0.20, default_max = -0.05,
        label = "Proportion Difference"
      ),
      odds_ratio = list(
        min = 0.1, max = 10, default_min = 1.5, default_max = 3,
        label = "Odds Ratio"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      baseline <- params$baseline %||% 0.5
      switch(method,
        "difference" = sapply(effect_sizes, function(d) {
          diff_to_cohens_h(d, baseline)
        }),
        "odds_ratio" = sapply(effect_sizes, function(or) {
          or_to_cohens_h(or, baseline)
        }),
        effect_sizes
      )
    },

    sample_size_calc = function(input) {
      total_n <- input$sample_size %||% 400
      m       <- input$m_cluster %||% 30
      icc     <- input$icc %||% 0.05
      dropout <- input$dropout %||% 0.1

      DE <- 1 + (m - 1) * icc

      per_arm <- c(total_n / 2, total_n / 2)
      result <- .canonicalize_sample_sizes(per_arm, dropout = dropout)

      result$n1 <- result$n_per_arm_evaluable[1] / DE
      result$n2 <- result$n_per_arm_evaluable[2] / DE

      result$design_effect       <- DE
      result$icc                 <- icc
      result$m_cluster           <- m
      result$n_clusters_per_arm  <- per_arm[1] / m

      result
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 400) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$m_cluster %||% 30) < 2) {
        issues <- c(issues, "Cluster size must be at least 2")
      }
      icc <- input$icc %||% 0.05
      if (icc < 0 || icc > 1) {
        issues <- c(issues, "ICC must be between 0 and 1")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}

#' Cluster-Randomized Log-rank Specification
#'
#' Two-group cluster-randomized trial with a survival outcome.
#' Schoenfeld formula on the per-arm effective event count, where
#' both event_prob and design effect (DE) inflate the variance.
#'
#' @keywords internal
create_cluster_logrank_spec <- function() {
  list(
    id = "cluster_logrank",
    name = "Cluster-Randomized Log-rank",
    description = "Cluster-randomized two-group trial with survival outcome",
    icon = "hourglass-bottom",
    power_function = logrank_power,
    effect_size_methods = c("hazard_ratio"),
    formula_citation = "Schoenfeld DA (1981) Biometrika 68(1) 316-319, with design-effect inflation DE = 1 + (m̄ − 1) × ICC (Donner & Klar 2000). Implementation: zzpower::logrank_power on expected events × event_prob, divided by DE.",
    default_effect_grid = list(hazard_ratio = c(0.50, 0.65, 0.75)),
    paragraph_template = NULL,
    repro_call         = "zzpower::logrank_power",

    parameters = list(
      sample_size = list(
        type = "slider",
        label = "Total Participants",
        min = 40, max = 5000, default = 400, step = 20
      ),
      event_prob = list(
        type = "slider",
        label = "Event Probability",
        min = 0.1, max = 1, default = 0.7, step = 0.05
      ),
      m_cluster = list(
        type = "slider",
        label = "Mean Cluster Size",
        min = 2, max = 200, default = 30, step = 1
      ),
      icc = list(
        type = "slider",
        label = "Intracluster Correlation (ICC)",
        min = 0, max = 0.5, default = 0.05, step = 0.01
      ),
      dropout = list(
        type = "slider",
        label = "Dropout Rate",
        min = 0, max = 0.5, default = 0.1, step = 0.05
      )
    ),

    effect_size_params = list(
      hazard_ratio = list(
        min = 0.1, max = 5, default_min = 1.2, default_max = 3.0,
        label = "Hazard Ratio"
      )
    ),

    standardize = function(effect_sizes, method, params) {
      log(effect_sizes)
    },

    sample_size_calc = function(input) {
      total_n    <- input$sample_size %||% 400
      event_prob <- input$event_prob %||% 0.7
      m          <- input$m_cluster %||% 30
      icc        <- input$icc %||% 0.05
      dropout    <- input$dropout %||% 0.1

      DE <- 1 + (m - 1) * icc

      per_arm <- c(total_n / 2, total_n / 2)
      result  <- .canonicalize_sample_sizes(per_arm, dropout = dropout)

      # Schoenfeld formula expects events; cluster RCT additionally
      # divides by DE. Compose both adjustments on the effective N.
      result$n1 <- result$n_per_arm_evaluable[1] * event_prob / DE
      result$n2 <- result$n_per_arm_evaluable[2] * event_prob / DE

      result$design_effect          <- DE
      result$icc                    <- icc
      result$m_cluster              <- m
      result$n_clusters_per_arm     <- per_arm[1] / m
      result$expected_events_total  <-
        result$n_total_evaluable * event_prob

      result
    },

    validation = function(input) {
      issues <- character()
      if ((input$sample_size %||% 400) <= 0) {
        issues <- c(issues, "Sample size must be positive")
      }
      if ((input$m_cluster %||% 30) < 2) {
        issues <- c(issues, "Cluster size must be at least 2")
      }
      icc <- input$icc %||% 0.05
      if (icc < 0 || icc > 1) {
        issues <- c(issues, "ICC must be between 0 and 1")
      }
      ep <- input$event_prob %||% 0.7
      if (ep <= 0 || ep > 1) {
        issues <- c(issues, "Event probability must be between 0 and 1")
      }
      if ((input$dropout %||% 0) > 1) {
        issues <- c(issues, "Dropout rate cannot exceed 100%")
      }
      issues
    }
  )
}
