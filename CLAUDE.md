# zzpower R Package Development

This document describes the development process and structure of the `zzpower` R package, created with assistance from Claude Code.

## Package Overview

**Package Name:** zzpower  
**Version:** 0.1.0  
**Author:** Ronald "Ryy" G. Thomas  
**License:** GPL-3  
**Purpose:** Interactive power analysis calculator for two-group parallel clinical trial designs

## Development Process

### Original Codebase
The package was refactored from existing Shiny applications:
- `app.R` - Original power calculator app
- `app_claude.R` - Enhanced modular version 
- `chat_app.R` - Chat-based interface version

### Refactoring Approach
1. **Package Structure Creation:** Standard R package directories (`R/`, `man/`, `tests/`, etc.)
2. **Code Modularization:** Separated UI components, server logic, and main launch function
3. **Documentation:** Added comprehensive roxygen2 documentation
4. **Testing:** Implemented testthat test suite
5. **CRAN Compliance:** Ensured all R CMD check requirements are met

## Package Architecture

### Core Functions

#### `launch_zzpower()`
- **File:** `R/launch_zzpower.R`
- **Purpose:** Main exported function that launches the Shiny application
- **Parameters:** 
  - `...` - Additional arguments passed to `shiny::runApp`
  - `launch.browser` - Whether to open in browser (default: TRUE)
  - `host` - IP address to listen on (default: "127.0.0.1")
  - `port` - Port number (default: NULL for random port)

#### Internal UI Functions
- **File:** `R/ui_components.R`
- **Functions:**
  - `create_ui()` - Main UI assembly
  - `create_sample_size_inputs()` - Sample size input controls
  - `create_effect_size_inputs()` - Effect size method selection
  - `create_advanced_settings()` - Advanced parameter controls

#### Server Logic
- **File:** `R/server_logic.R`
- **Function:** `create_server()` - Complete server-side reactive logic

## Key Features

### Effect Size Methods
1. **Standard Deviation Units (Cohen's d)** - Direct standardized effect sizes
2. **Percent Reduction** - Effect as percentage reduction from placebo
3. **Difference in Change Scores** - Absolute difference in outcome units
4. **Change in Active Group** - Direct specification of treatment group change

### Design Considerations
- Dropout rates and drop-in rates
- Unequal group allocation ratios
- Type I error rate configuration
- One-sided vs two-sided testing

### Outputs
- Interactive power curves with ggplot2
- Detailed results tables with DT
- Study design summaries
- Downloadable reports (placeholder implementation)

## Dependencies

### Core Dependencies
- `shiny` (>= 1.7.0) - Web application framework
- `bslib` (>= 0.4.0) - Bootstrap themes and components
- `bsicons` (>= 0.1.0) - Bootstrap icons
- `pwr` (>= 1.3.0) - Statistical power calculations
- `ggplot2` (>= 3.4.0) - Data visualization
- `DT` (>= 0.20) - Interactive tables
- `magrittr` - Pipe operator
- `rlang` - Tidy evaluation support

### Development Dependencies
- `testthat` (>= 3.0.0) - Testing framework
- `devtools` - Package development tools
- `roxygen2` - Documentation generation

## CRAN Compliance

### R CMD Check Results
- ✅ 0 errors
- ✅ 0 warnings  
- ✅ 0 notes

### Key Compliance Actions
1. **Proper NAMESPACE:** Specific imports instead of wholesale package imports
2. **Documentation:** Complete roxygen2 documentation for all exported functions
3. **Testing:** Comprehensive test coverage
4. **ASCII Compliance:** Removed non-ASCII characters from source code
5. **Dependency Management:** Only declared necessary imports
6. **File Structure:** Clean package directory without extraneous files

## Testing Strategy

### Test Coverage
- **File:** `tests/testthat/test-launch_zzpower.R`
- **Coverage:**
  - Function existence and structure
  - UI component generation
  - Server logic creation
  - Package dependency availability
  - Proper object classes and types

## Usage Examples

### Basic Usage
```r
# Install and load the package
library(zzpower)

# Launch the interactive application
launch_zzpower()
```

### Advanced Usage
```r
# Launch on specific port
launch_zzpower(port = 3838)

# Launch without opening browser
launch_zzpower(launch.browser = FALSE)
```

## Development Tools Used

### R Packages
- `devtools` - Package development workflow
- `roxygen2` - Documentation generation
- `testthat` - Unit testing
- `usethis` - Package development utilities (indirectly)

### Development Commands
```r
# Documentation generation
devtools::document()

# Package checking
devtools::check()

# Installation testing
devtools::install()

# CRAN submission preparation
devtools::release()
```

## File Structure

```
zzpower/
├── DESCRIPTION          # Package metadata
├── NAMESPACE           # Generated namespace file
├── README.md           # Package overview
├── NEWS.md            # Version changelog
├── CLAUDE.md          # This development documentation
├── R/                 # R source code
│   ├── launch_zzpower.R      # Main function
│   ├── ui_components.R       # UI functions
│   ├── server_logic.R        # Server logic
│   └── zzpower-package.R     # Package documentation
├── man/               # Generated documentation
├── tests/             # Test suite
│   ├── testthat.R
│   └── testthat/
│       └── test-launch_zzpower.R
└── zzpower_0.1.0.tar.gz     # Built package
```

## Power Analysis Capabilities

### Statistical Methods
- Two-sample t-tests for parallel group designs
- Unequal sample size support via `pwr::pwr.t2n.test()`
- Configurable Type I error rates
- One-sided and two-sided testing options

### Interactive Features
- Real-time power curve updates
- Multiple effect size parameterizations
- Sample size calculations with dropout adjustments
- Visual identification of detectable effect sizes

## Future Development

### Potential Enhancements
1. **Report Generation:** Full rmarkdown report implementation
2. **Additional Designs:** Support for crossover, factorial designs
3. **Bayesian Methods:** Prior-based power analysis options
4. **Simulation Studies:** Monte Carlo power validation
5. **Export Options:** More data export formats

### Maintenance Notes
- Keep dependency versions updated
- Monitor CRAN policy changes
- Respond to user feedback and bug reports
- Consider community contributions

## Repository Information
- **URL:** https://github.com/rythomas/zzpower
- **Bug Reports:** https://github.com/rythomas/zzpower/issues

---

*This package was developed with assistance from Claude Code, an AI-powered development tool by Anthropic.*