expect_true(exists("ZZPOWER_CONSTANTS"))
consts <- ZZPOWER_CONSTANTS

# Sample size constraints
expect_true(consts$SAMPLE_SIZE_MIN > 0)
expect_true(consts$SAMPLE_SIZE_MAX > consts$SAMPLE_SIZE_MIN)
expect_true(consts$SAMPLE_SIZE_DEFAULT > consts$SAMPLE_SIZE_MIN)
expect_true(consts$SAMPLE_SIZE_DEFAULT < consts$SAMPLE_SIZE_MAX)

# Effect size constraints
expect_true(consts$COHENS_D_DEFAULT_MIN < consts$COHENS_D_DEFAULT_MAX)
expect_equal(consts$POWER_TARGET, 0.8)

# Power analysis
expect_true(consts$EFFECT_SIZE_SEQ_LENGTH > 0)
expect_true(is.numeric(consts$POWER_TARGET))
expect_true(consts$POWER_TARGET > 0 && consts$POWER_TARGET < 1)


consts <- ZZPOWER_CONSTANTS

# Sample size inputs
expect_equal(consts$SAMPLE_SIZE_MIN, 20)
expect_equal(consts$SAMPLE_SIZE_MAX, 500)
expect_equal(consts$SAMPLE_SIZE_DEFAULT, 100)

# Dropout rate inputs
expect_equal(consts$DROPOUT_MAX, 0.5)
expect_equal(consts$DROPOUT_DEFAULT, 0.1)

# Type I error inputs
expect_equal(consts$TYPE1_MIN, 0.01)
expect_equal(consts$TYPE1_MAX, 0.2)
expect_equal(consts$TYPE1_DEFAULT, 0.05)


consts <- ZZPOWER_CONSTANTS

# Test effect size sequence generation
expected_length <- consts$EFFECT_SIZE_SEQ_LENGTH
test_seq <- seq(0.2, 1.0, length.out = expected_length)

expect_length(test_seq, expected_length)
expect_equal(test_seq[1], 0.2)
expect_equal(test_seq[expected_length], 1.0)


consts <- ZZPOWER_CONSTANTS

expect_true(is.character(consts$POWER_CURVE_COLOR))
expect_true(is.character(consts$POWER_REFERENCE_COLOR))
expect_true(is.character(consts$POWER_VLINE_STYLE))
expect_true(is.character(consts$POWER_HLINE_STYLE))

# Colors should be hex or named colors
expect_true(grepl("^#|^[a-z]", consts$POWER_CURVE_COLOR))
expect_true(grepl("^#|^[a-z]", consts$POWER_REFERENCE_COLOR))


consts <- ZZPOWER_CONSTANTS

expect_true(consts$TABLE_PAGE_LENGTH > 0)
expect_true(consts$TABLE_DECIMAL_PLACES >= 0)
expect_true(is.character(consts$POWER_CURVE_HEIGHT))

# Check height format (pixels)
expect_true(grepl("px", consts$POWER_CURVE_HEIGHT))
