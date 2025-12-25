# zzpower

<!-- badges: start -->
<!-- badges: end -->

The `zzpower` package provides a comprehensive 'Shiny' application for conducting power analysis and sample size calculations for two-group parallel clinical trial designs.

## Features

- **Multiple Effect Size Methods**: Support for Cohen's d, percentage reductions, difference in change scores, and treatment group changes
- **Comprehensive Design Considerations**: Accounts for dropout rates, drop-in rates, and unequal group allocation ratios  
- **Interactive Visualizations**: Real-time power curves and detailed results tables
- **Report Generation**: Downloadable reports in PDF, HTML, or Word formats
- **Advanced Settings**: Configurable Type I error rates and one-sided vs two-sided testing

## Installation

You can install the development version of zzpower from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rythomas/zzpower")
```

## Usage

To launch the interactive application:

```r
library(zzpower)
launch_zzpower()
```

The application will open in your default web browser and provide an interactive interface for:

1. **Sample Size Planning**: Set total sample size and expected dropout rates
2. **Effect Size Specification**: Choose from multiple methods to specify the expected treatment effect
3. **Advanced Configuration**: Adjust randomization ratios, Type I error rates, and testing approaches
4. **Results Visualization**: View interactive power curves and detailed results tables
5. **Report Generation**: Download comprehensive reports in multiple formats

## Effect Size Methods

The application supports four different approaches to specifying effect sizes:

- **Standard Deviation Units (Cohen's d)**: Direct specification of standardized effect sizes
- **Percent Reduction**: Effect size specified as percentage reduction from placebo  
- **Difference in Change Scores**: Absolute difference between treatment groups in outcome units
- **Change in Active Group**: Specifying the treatment group change directly

## Example Workflow

1. Launch the application with `launch_zzpower()`
2. Set your total planned sample size using the slider
3. Specify expected dropout rate
4. Choose your preferred effect size method
5. Set the range of effect sizes to evaluate
6. Review the power curve and identify the detectable effect size at 80% power
7. Download a report documenting your power analysis

## Dependencies

This package builds on several excellent R packages:

- `shiny` - Web application framework
- `bslib` - Bootstrap themes and components  
- `bsicons` - Bootstrap icons
- `pwr` - Power analysis calculations
- `ggplot2` - Data visualization
- `DT` - Interactive tables
- `rmarkdown` - Report generation

## Supported Statistical Tests

The application includes 5 statistical tests via a scalable plugin architecture:

| Test | Use Case | Effect Size |
|:-----|:---------|:------------|
| Two-Group t-test | Parallel RCT designs | Cohen's d, % reduction |
| Paired t-test | Before-after designs | Standardized difference |
| One-Sample t-test | Single-group vs reference | Cohen's d |
| Two Proportions | Binary outcomes | Proportions, OR, RR |
| Correlation | Bivariate relationships | Correlation r |

## Documentation

- `vignette("quickstart")` - Quick start guide

## Reproducibility

This package is developed using the zzcollab framework for reproducible
research. To reproduce the development environment:

```bash
git clone https://github.com/rgt47/zzpower.git
cd zzpower
make r  # Enter Docker container with all dependencies
```

## License

GPL-3

## Author

Ronald (Ryy) G. Thomas (rgthomas@ucsd.edu)