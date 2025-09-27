# Technical Documentation: zzpower R Package

## Abstract

The `zzpower` package provides a comprehensive Shiny-based application for conducting power analysis and sample size calculations for two-group parallel clinical trial designs. This document presents a detailed technical analysis of the package architecture, statistical methodology, and implementation details.

## 1. Package Architecture

### 1.1 Core Components

The package follows standard R package conventions with the following structure:

```
zzpower/
├── DESCRIPTION          # Package metadata and dependencies
├── NAMESPACE           # Exported functions and imports
├── R/                  # Source code directory
│   ├── launch_zzpower.R       # Main application launcher
│   ├── ui_components.R        # User interface components
│   ├── server_logic.R         # Server-side reactive logic
│   └── zzpower-package.R      # Package documentation
├── man/                # Generated documentation files
├── tests/              # Test suite
└── README.md           # Package overview
```

### 1.2 Dependency Management

The package implements selective imports rather than wholesale namespace imports, ensuring minimal dependency footprint. Core dependencies include:

- **Statistical Computing**: `pwr` (≥1.3.0) for power calculations
- **Web Framework**: `shiny` (≥1.7.0) for application infrastructure
- **User Interface**: `bslib` (≥0.4.0) and `bsicons` (≥0.1.0) for modern Bootstrap-based UI
- **Data Visualization**: `ggplot2` (≥3.4.0) for interactive plotting
- **Data Presentation**: `DT` (≥0.20) for interactive tables

### 1.3 Function Architecture

The package exports a single public function (`launch_zzpower()`) that orchestrates the application launch, while maintaining internal functions for UI component creation and server logic.

## 2. Statistical Methodology

### 2.1 Power Analysis Framework

The application implements two-sample t-test power analysis using the `pwr.t2n.test()` function from the `pwr` package. The statistical model assumes:

- Independent samples from two populations
- Normal distribution of outcome variables
- Homoscedasticity (equal variances)
- Continuous outcome measures

### 2.2 Effect Size Parameterization

The application supports four distinct methods for effect size specification:

#### 2.2.1 Cohen's d (Standard Deviation Units)
Direct specification of standardized effect size:
```
d = (μ₁ - μ₂) / σ_pooled
```

#### 2.2.2 Percentage Reduction
Effect size based on proportional change from placebo:
```
d = (percent_reduction × μ₀) / σ₀
```
where μ₀ is the placebo group mean and σ₀ is the pooled standard deviation.

#### 2.2.3 Difference in Change Scores
Effect size based on absolute difference in outcome units:
```
d = difference / σ_pooled
```

#### 2.2.4 Active Group Change
Effect size based on treatment group outcome specification:
```
d = (μ₀ - active_change) / σ₀
```

### 2.3 Sample Size Calculations

The application accounts for study design complexities through adjusted sample size calculations:

#### 2.3.1 Dropout Adjustment
Completer sample sizes are calculated as:
```
n_completer = n_ITT × (1 - dropout_rate - dropin_rate)
```

#### 2.3.2 Unequal Allocation
For allocation ratio r (active:control):
```
n₁ = (r × N_total) / (r + 1)
n₂ = N_total / (r + 1)
```

#### 2.3.3 Type I Error Adjustment
For one-sided versus two-sided testing:
```
α_adjusted = α_nominal / sided
```
where `sided = 1` for one-sided tests and `sided = 2` for two-sided tests.

## 3. User Interface Design

### 3.1 Layout Architecture

The application employs a `page_sidebar` layout using the `bslib` package, providing:

- **Sidebar Panel**: Parameter input controls organized into logical sections
- **Main Panel**: Results visualization with interactive plots and tables
- **Card-based Layout**: Modular content organization with full-screen capabilities

### 3.2 Input Validation

The interface implements several validation mechanisms:

- **Range Constraints**: Numeric inputs constrained to valid ranges
- **Conditional Panels**: Dynamic UI elements based on user selections
- **Reactive Updates**: Automatic parameter updates based on dependencies

### 3.3 Responsive Design

The interface utilizes Bootstrap 5 framework through `bslib` for responsive design across device types, with configurable themes and modern UI components.

## 4. Server-Side Logic

### 4.1 Reactive Programming Model

The server logic implements Shiny's reactive programming paradigm with structured reactive expressions:

```r
effect_sizes() -> cohens_d() -> study_parameters() -> power_results()
```

### 4.2 Computational Flow

1. **Input Processing**: User inputs processed through reactive expressions
2. **Effect Size Conversion**: Multiple parameterizations converted to Cohen's d
3. **Sample Size Calculation**: Dropout and allocation adjustments applied
4. **Power Computation**: Vectorized power calculations across effect size range
5. **Results Presentation**: Data formatted for visualization and tabular display

### 4.3 Error Handling

Robust error handling implemented through:
- **Try-catch blocks** for power calculations
- **Input validation** at reactive expression level
- **Graceful degradation** for invalid parameter combinations

## 5. Visualization Components

### 5.1 Power Curves

Interactive power curves generated using `ggplot2` with:
- **Effect size range** on x-axis (method-specific scaling)
- **Statistical power** on y-axis (0-1 scale)
- **Reference lines** at 80% power threshold
- **Detectable effect markers** for minimum detectable effects

### 5.2 Results Tables

Interactive tables implemented via `DT` package providing:
- **Sortable columns** for effect size and power values
- **Precision formatting** with controlled decimal places
- **Scrollable content** for large datasets

## 6. Testing Framework

### 6.1 Test Coverage

Comprehensive test suite implemented using `testthat` framework covering:

- **Function existence and structure validation**
- **Statistical calculation accuracy**
- **UI component generation**
- **Error handling scenarios**
- **Edge case behavior**

### 6.2 Test Categories

Tests organized into logical categories:

- `test-launch_zzpower.R`: Main function and component validation
- `test-server-logic.R`: Reactive logic and calculations
- `test-effect-sizes.R`: Effect size conversion accuracy
- `test-power-calculations.R`: Statistical computation validation
- `test-ui-components.R`: Interface component functionality
- `test-edge-cases.R`: Boundary condition handling

## 7. Performance Considerations

### 7.1 Computational Efficiency

- **Vectorized calculations** for power analysis across effect size ranges
- **Selective reactive updates** to minimize unnecessary computations
- **Efficient data structures** for results storage and manipulation

### 7.2 Memory Management

- **Minimal object retention** in reactive expressions
- **Garbage collection** considerations for large datasets
- **Session state management** for multi-user deployments

## 8. Deployment Architecture

### 8.1 Standalone Application

The package supports standalone deployment through:
- **Local execution** via `launch_zzpower()`
- **Custom port specification** for development environments
- **Browser control** options for automated testing

### 8.2 Server Deployment

Application suitable for server deployment through:
- **Shiny Server** for institutional hosting
- **ShinyApps.io** for cloud deployment
- **Docker containerization** for scalable deployment

## 9. Extensibility Framework

### 9.1 Modular Design

The architecture supports extension through:
- **Component-based UI functions** for interface modifications
- **Parameterized server logic** for calculation method additions
- **Plugin architecture** for additional statistical methods

### 9.2 Configuration Management

Future enhancements supported through:
- **Configuration files** for default parameters
- **Theme customization** options
- **Localization support** for international deployment

## 10. Quality Assurance

### 10.1 Code Standards

The package adheres to:
- **R Package Development Guidelines** (R Core Team)
- **CRAN Repository Policies** for distribution
- **Tidyverse Style Guide** for code consistency

### 10.2 Documentation Standards

Comprehensive documentation provided through:
- **roxygen2** for function documentation
- **vignettes** for usage examples
- **Technical specifications** for statistical methodology

## 11. Future Development Roadmap

### 11.1 Statistical Enhancements

Potential extensions include:
- **Bayesian power analysis** methods
- **Non-parametric alternatives** for non-normal data
- **Adaptive trial designs** for sequential testing

### 11.2 Interface Improvements

Planned enhancements encompass:
- **Advanced reporting capabilities** with R Markdown integration
- **Data import functionality** for external parameter specification
- **Collaboration features** for multi-user project management

## References

1. Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum Associates.

2. Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package version 1.3-0.

3. Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2021). shiny: Web Application Framework for R.

4. Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K., Wilke, C., Woo, K., Yutani, H., & Dunnington, D. (2021). ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics.

5. Xie, Y., Cheng, J., & Tan, X. (2021). DT: A Wrapper of the JavaScript Library 'DataTables'.