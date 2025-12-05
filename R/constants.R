#' zzpower Package Constants
#'
#' Centralized constants for the zzpower package. These values define
#' parameter bounds, defaults, and computational settings.
#'
#' @keywords internal
#' @export

ZZPOWER_CONSTANTS <- list(
  # Sample size constraints
  SAMPLE_SIZE_MIN = 20,
  SAMPLE_SIZE_MAX = 500,
  SAMPLE_SIZE_DEFAULT = 100,
  SAMPLE_SIZE_STEP = 10,

  # Effect size constraints (Cohen's d method)
  COHENS_D_MIN = 0,
  COHENS_D_MAX = 2,
  COHENS_D_DEFAULT_MIN = 0.2,
  COHENS_D_DEFAULT_MAX = 1.0,

  # Difference in change scores
  DIFFERENCE_MIN = 0,
  DIFFERENCE_MAX = 10,
  DIFFERENCE_DEFAULT_MIN = 1,
  DIFFERENCE_DEFAULT_MAX = 5,

  # Percent reduction
  PERCENT_REDUCTION_MIN = 0,
  PERCENT_REDUCTION_MAX = 1,
  PERCENT_REDUCTION_DEFAULT_MIN = 0.1,
  PERCENT_REDUCTION_DEFAULT_MAX = 0.5,

  # Treatment group change
  TREATMENT_CHANGE_MIN = 0,
  TREATMENT_CHANGE_MAX = 10,
  TREATMENT_CHANGE_DEFAULT_MIN = 0,
  TREATMENT_CHANGE_DEFAULT_MAX = 6,

  # Dropout and drop-in
  DROPOUT_MIN = 0,
  DROPOUT_MAX = 0.5,
  DROPOUT_DEFAULT = 0.1,
  DROPOUT_STEP = 0.05,

  DROPIN_MIN = 0,
  DROPIN_MAX = 0.4,
  DROPIN_DEFAULT = 0,
  DROPIN_STEP = 0.05,

  # Type I error
  TYPE1_MIN = 0.01,
  TYPE1_MAX = 0.2,
  TYPE1_DEFAULT = 0.05,
  TYPE1_STEP = 0.005,

  # Allocation ratio (Active:Control)
  RATIO_MIN = 0.5,
  RATIO_MAX = 5,
  RATIO_DEFAULT = 1,
  RATIO_STEP = 0.5,

  # Default placebo parameters
  PLACEBO_CHANGE_DEFAULT = 10,
  PLACEBO_SD_DEFAULT = 10,

  # Power analysis
  EFFECT_SIZE_SEQ_LENGTH = 16,  # Number of points in power curve
  POWER_TARGET = 0.8,           # Target power (80%)
  POWER_CURVE_HEIGHT = "300px", # Chart height for scrolling table

  # Visualization colors
  POWER_CURVE_COLOR = "#1f77b4",     # Professional blue
  POWER_REFERENCE_COLOR = "#d62728", # Professional red

  # Visualization line styles
  POWER_VLINE_STYLE = "dotted",
  POWER_HLINE_STYLE = "dashed",

  # Table formatting
  TABLE_PAGE_LENGTH = 10,
  TABLE_DECIMAL_PLACES = 3,

  # Two Proportions (pwr.2p2n.test) - Continuous proportion parameters
  PROPORTION_MIN = 0.01,
  PROPORTION_MAX = 0.99,
  PROPORTION_DEFAULT_1 = 0.5,
  PROPORTION_DEFAULT_2 = 0.3,

  # Proportion difference parameters
  PROP_DIFF_MIN = -0.5,
  PROP_DIFF_MAX = 0.5,
  PROP_DIFF_DEFAULT_MIN = -0.3,
  PROP_DIFF_DEFAULT_MAX = -0.05,

  # Odds ratio parameters
  ODDS_RATIO_MIN = 0.1,
  ODDS_RATIO_MAX = 10,
  ODDS_RATIO_DEFAULT_MIN = 1.2,
  ODDS_RATIO_DEFAULT_MAX = 3,

  # Relative risk parameters
  RELATIVE_RISK_MIN = 0.1,
  RELATIVE_RISK_MAX = 10,
  RELATIVE_RISK_DEFAULT_MIN = 1.2,
  RELATIVE_RISK_DEFAULT_MAX = 3,

  # Two proportions specific parameters
  BASELINE_PROPORTION_DEFAULT = 0.5,
  SAMPLE_SIZE_MAX_PROP = 1000,  # Larger max for proportion studies
  SAMPLE_SIZE_DEFAULT_PROP = 200  # Larger default for binary outcomes
)
