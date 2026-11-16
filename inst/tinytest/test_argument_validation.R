# Regression tests for argument handling defects found 2026-11-16.

# power_table(): `sample_size` in `...` used to override the solved N,
# so every row reported the same N (e.g. 128) with no condition.
expect_error(
  power_table('ttest_2groups', effect_grid = c(0.2, 0.5, 0.8),
              effect_method = 'cohens_d', sample_size = 128),
  pattern = 'sample_size',
  info = 'power_table sample_size defect: sample_size in ... must error'
)
expect_error(
  power_table('ttest_2groups', effect_grid = c(0.2, 0.5),
              effect_method = 'cohens_d', sample = 128),
  pattern = 'sample_size',
  info = 'power_table sample_size defect: partial name must error'
)
expect_error(
  power_table('ttest_2groups', c(0.2, 0.5), c(0.8, 0.9), 'cohens_d',
              0.05, 'two.sided', 0.1, 128),
  pattern = 'unnamed',
  info = 'power_table sample_size defect: unnamed extra arg must error'
)
expect_error(
  power_table('ttest_2groups', effect_grid = 0.5,
              effect_method = 'cohens_d', target_power = 0.5),
  pattern = 'power_thresholds',
  info = 'power_table sample_size defect: target_power in ... must error'
)
expect_error(
  power_table('ttest_2groups', effect_grid = 0.5,
              effect_method = 'cohens_d', effect_size = 0.5),
  pattern = 'effect_grid',
  info = 'power_table sample_size defect: effect_size in ... must error'
)

tab <- power_table('ttest_2groups', effect_grid = c(0.2, 0.5, 0.8),
                   effect_method = 'cohens_d')
expect_true(
  all(diff(tab$n_total_enrolled_p80) < 0),
  info = 'power_table sample_size defect: N must vary across effect grid'
)

tab_alloc <- power_table('ttest_2groups', effect_grid = 0.5,
                         effect_method = 'cohens_d',
                         allocation = 'unequal', ratio = 2)
expect_true(
  tab_alloc$n_total_enrolled_p80 > tab$n_total_enrolled_p80[2],
  info = 'power_table sample_size defect: named design params still apply'
)

# .card_color(): `card_colors[id] %||% default` returned NA, not the
# default, for an id missing from the palette.
card_color <- zzpower:::.card_color
palette <- c(ttest_2groups = '#182B49')
expect_identical(
  card_color(palette, 'not_a_test', '#334155'), '#334155',
  info = 'card_colors NA defect: missing id must return the default'
)
expect_identical(
  card_color(palette, 'ttest_2groups', '#334155'), '#182B49',
  info = 'card_colors NA defect: present id must return its unnamed color'
)
expect_identical(
  card_color(c(a = NA_character_), 'a', '#334155'), '#334155',
  info = 'card_colors NA defect: NA entry must return the default'
)
