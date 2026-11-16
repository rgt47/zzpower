# Regression tests for the silent default 10% dropout (review item F3).
# power_calc() applied dropout = 0.1 by default with no mention in the
# help page or any output, so its power (0.758) differed from pwr
# (0.801) for the same nominal N without explanation. The default is
# kept; these tests pin that it is recorded and stated everywhere.

library(zzpower)

defect <- 'F3 silent default dropout'

ctx_def <- power_calc('ttest_2groups', sample_size = 128,
                      effect_size = 0.5, effect_method = 'cohens_d')
ctx_0 <- power_calc('ttest_2groups', sample_size = 128,
                    effect_size = 0.5, effect_method = 'cohens_d',
                    dropout = 0)

expect_equal(round(ctx_def$achieved_power, 3), 0.758,
             info = paste(defect, ': default power is unchanged (0.758)'))
if (requireNamespace('pwr', quietly = TRUE)) {
  expect_equal(ctx_0$achieved_power,
               pwr::pwr.t2n.test(n1 = 64, n2 = 64, d = 0.5)$power,
               tolerance = 1e-12,
               info = paste(defect, ': dropout = 0 matches pwr exactly'))
}

expect_identical(formals(power_calc)$dropout, 0.1,
                 info = paste(defect, ': power_calc() has dropout = 0.1'))
expect_identical(formals(power_table)$dropout, 0.1,
                 info = paste(defect, ': power_table() has dropout = 0.1'))

expect_true(inherits(ctx_def, 'calc_context') && is.list(ctx_def),
            info = paste(defect, ': result is a calc_context list'))
expect_equal(ctx_def$dropout, 0.1,
             info = paste(defect, ': ctx$dropout records the default'))
expect_equal(ctx_def$n_effective, 115.2,
             info = paste(defect, ': ctx$n_effective = 128 x 0.9'))
expect_equal(ctx_def$n_enrolled, 128,
             info = paste(defect, ': ctx$n_enrolled is the input N'))
expect_true(isTRUE(ctx_def$dropout_is_default),
            info = paste(defect, ': default use is flagged'))
expect_equal(ctx_0$dropout, 0,
             info = paste(defect, ': explicit dropout = 0 is recorded'))
expect_equal(ctx_0$n_effective, 128,
             info = paste(defect, ': no reduction when dropout = 0'))
expect_false(isTRUE(ctx_0$dropout_is_default),
             info = paste(defect, ': explicit dropout is not flagged'))

printed <- paste(capture.output(print(ctx_def)), collapse = '\n')
expect_true(grepl('Dropout:       10% (package default', printed,
                  fixed = TRUE),
            info = paste(defect, ': print() states the default dropout'))
expect_true(grepl('N evaluable:   115.2', printed, fixed = TRUE),
            info = paste(defect, ': print() states the evaluable N'))

# Using the default stays silent (decided design: visible in output,
# not via conditions).
conds <- character()
withCallingHandlers(
  power_calc('ttest_2groups', sample_size = 128, effect_size = 0.5,
             effect_method = 'cohens_d'),
  warning = function(w) {
    conds <<- c(conds, conditionMessage(w))
    invokeRestart('muffleWarning')
  },
  message = function(m) {
    conds <<- c(conds, conditionMessage(m))
    invokeRestart('muffleMessage')
  }
)
expect_identical(length(conds), 0L,
                 info = paste(defect, ': no warning or message emitted'))

tbl <- power_table('ttest_2groups', effect_grid = 0.5,
                   effect_method = 'cohens_d')
expect_true('dropout' %in% names(tbl),
            info = paste(defect, ': power_table() has a dropout column'))
expect_equal(tbl$dropout, 0.1,
             info = paste(defect, ': power_table() dropout column value'))
tbl0 <- power_table('ttest_2groups', effect_grid = 0.5,
                    effect_method = 'cohens_d', dropout = 0)
expect_equal(tbl0$n_total_enrolled_p80, tbl0$n_total_evaluable_p80,
             info = paste(defect, ': power_table(dropout = 0) is unadjusted'))

study <- add_aim(multi_aim_study('S'), ctx_def)
df <- format_multi_aim_df(study)
expect_equal(df$Dropout, 0.1,
             info = paste(defect, ': format_multi_aim_df() Dropout column'))
expect_true('Dropout' %in% names(format_multi_aim_df(multi_aim_study())),
            info = paste(defect, ': empty multi-aim frame has Dropout'))
md <- multi_aim_markdown(study)
expect_true(grepl('| 10% |', md, fixed = TRUE),
            info = paste(defect, ': multi_aim_markdown() shows 10%'))

repro <- zzpower:::.render_repro_script(ctx_def)
expect_true(grepl('# Dropout assumption: 10%', repro, fixed = TRUE),
            info = paste(defect, ': repro script states the dropout'))

spec <- get_power_test_registry()$ttest_2groups
report_data <- list(
  test_id = 'ttest_2groups', test_name = spec$name, format = 'text',
  solve_mode = 'power', timestamp = Sys.time(), r_version = '4.4.0',
  parameters = list(sample_size = 128, dropout = 0.1),
  dropout = 0.1,
  sample_sizes = ctx_def$sample_sizes,
  effect_size_range = list(effect_sizes = c(0.3, 0.5),
                           standardized = c(0.3, 0.5),
                           method = 'cohens_d'),
  power_results = data.frame(effect_size = c(0.3, 0.5),
                             standardized_es = c(0.3, 0.5),
                             power = c(0.4, 0.758)),
  target_power = 0.8, type1_error = 0.05, one_sided = FALSE
)
txt <- paste(zzpower:::.format_generic_text_report(report_data, spec),
             collapse = '\n')
expect_true(grepl('Dropout rate: 10%', txt, fixed = TRUE),
            info = paste(defect, ': text report states the dropout'))
expect_true(grepl('Total evaluable: 115.2', txt, fixed = TRUE),
            info = paste(defect, ': text report states evaluable N'))
html <- zzpower:::.format_generic_html_report(report_data, spec)
expect_true(grepl('Dropout rate: 10%', html, fixed = TRUE),
            info = paste(defect, ': HTML report states the dropout'))
mdr <- zzpower:::.format_generic_md_report(report_data, spec)
expect_true(grepl('Dropout rate: 10%', mdr, fixed = TRUE),
            info = paste(defect, ': Markdown report states the dropout'))

# Sample-size mode reads `ss_`-prefixed inputs, so its dropout can
# differ from the power-mode slider.
report_ss <- report_data
report_ss$solve_mode <- 'sample_size'
report_ss$sample_sizes <- NULL
report_ss$dropout <- 0.2
report_ss$power_results <- data.frame(effect_size = c(0.3, 0.5),
                                      standardized_es = c(0.3, 0.5),
                                      required_n = c(390, 142))
txt_ss <- paste(zzpower:::.format_generic_text_report(report_ss, spec),
                collapse = '\n')
expect_true(grepl('Dropout rate: 20%', txt_ss, fixed = TRUE),
            info = paste(defect, ': sample-size report uses its dropout'))
expect_true(grepl('with 20% dropout', txt_ss, fixed = TRUE),
            info = paste(defect, ': sample-size repro uses its dropout'))
