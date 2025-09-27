# User Manual: zzpower Package

## Table of Contents

1. [Introduction](#1-introduction)
2. [Installation](#2-installation)
3. [Getting Started](#3-getting-started)
4. [Interface Overview](#4-interface-overview)
5. [Parameter Specification](#5-parameter-specification)
6. [Effect Size Methods](#6-effect-size-methods)
7. [Advanced Settings](#7-advanced-settings)
8. [Results Interpretation](#8-results-interpretation)
9. [Report Generation](#9-report-generation)
10. [Troubleshooting](#10-troubleshooting)
11. [Best Practices](#11-best-practices)
12. [Frequently Asked Questions](#12-frequently-asked-questions)

## 1. Introduction

The `zzpower` package provides an interactive web-based application for conducting power analysis and sample size calculations for two-group parallel clinical trial designs. This manual provides comprehensive guidance for using the application effectively in clinical research planning.

### 1.1 Purpose and Scope

The application addresses the following research scenarios:

- **Clinical trial planning**: Sample size determination for regulatory submissions
- **Grant applications**: Power analysis documentation for funding proposals
- **Feasibility studies**: Resource requirement assessment
- **Protocol development**: Statistical planning support

### 1.2 Target Audience

This manual serves:

- **Clinical investigators** conducting clinical trials
- **Statisticians** involved in study design
- **Research coordinators** managing study planning
- **Students** learning power analysis concepts

## 2. Installation

### 2.1 Prerequisites

Ensure you have the following installed:

- **R version 4.0.0 or higher**
- **RStudio (recommended)** for enhanced user experience
- **Web browser** with JavaScript enabled

### 2.2 Package Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install zzpower package
devtools::install_github("rythomas/zzpower")
```

### 2.3 Dependency Installation

The package will automatically install required dependencies:

- `shiny` - Web application framework
- `bslib` - Bootstrap themes and components
- `bsicons` - Bootstrap icons
- `pwr` - Power analysis calculations
- `ggplot2` - Data visualization
- `DT` - Interactive tables

### 2.4 Verification

Verify successful installation:

```r
library(zzpower)
packageVersion("zzpower")
```

## 3. Getting Started

### 3.1 Launching the Application

Start the interactive application:

```r
library(zzpower)
launch_zzpower()
```

The application will open in your default web browser.

### 3.2 Alternative Launch Options

**Specify custom port**:
```r
launch_zzpower(port = 3838)
```

**Launch without browser**:
```r
launch_zzpower(launch.browser = FALSE)
```

**Custom host specification**:
```r
launch_zzpower(host = "0.0.0.0", port = 8080)
```

### 3.3 First-Time Setup

Upon first launch:

1. **Browser compatibility check**: Ensure JavaScript is enabled
2. **Interface familiarization**: Review layout and navigation
3. **Parameter exploration**: Experiment with default values

## 4. Interface Overview

### 4.1 Layout Structure

The application features a modern, responsive layout:

- **Sidebar Panel**: Parameter input controls
- **Main Panel**: Results visualization and tables
- **Header**: Application title and navigation

### 4.2 Input Controls

**Sample Size Section**:
- Total sample size slider
- Dropout rate specification
- Sample size display (ITT and completers)

**Effect Size Section**:
- Method selection radio buttons
- Method-specific parameter inputs
- Range specifications

**Advanced Settings**:
- Group allocation ratios
- Type I error rates
- Test directionality options

### 4.3 Output Displays

**Power Curve Plot**:
- Interactive visualization with zoom capabilities
- Reference lines for 80% power threshold
- Method-specific x-axis scaling

**Results Table**:
- Sortable columns for effect size and power
- Exportable data format
- Precision-controlled display

**Study Summary**:
- Design parameter overview
- Key results highlighting
- Interpretation guidance

## 5. Parameter Specification

### 5.1 Sample Size Parameters

#### Total Sample Size
- **Range**: 20 to 500 participants
- **Default**: 100 participants
- **Increment**: 10 participants
- **Consideration**: Represents combined sample across both groups

#### Dropout Rate
- **Range**: 0% to 50%
- **Default**: 10%
- **Increment**: 5%
- **Definition**: Proportion of enrolled participants expected to withdraw

### 5.2 Sample Size Display

The interface displays both:

**Intention-to-Treat (ITT) Sizes**:
- Total enrolled participants per group
- Accounts for randomization ratio

**Completer Sizes**:
- Expected participants completing study
- Adjusted for dropout and drop-in rates

## 6. Effect Size Methods

### 6.1 Standard Deviation Units (Cohen's d)

**Description**: Direct specification of standardized effect size

**Input Parameters**:
- Effect size range (0.0 to 2.0)
- Default range: 0.2 to 1.0

**Interpretation**:
- 0.2 = Small effect
- 0.5 = Medium effect
- 0.8 = Large effect

**Use Cases**:
- Meta-analysis planning
- Replication studies
- Comparative effectiveness research

### 6.2 Percentage Reduction

**Description**: Effect size as proportional improvement from control

**Input Parameters**:
- Percentage reduction range (0% to 100%)
- Placebo group mean
- Pooled standard deviation

**Calculation**:
```
Cohen's d = (percentage × placebo_mean) / standard_deviation
```

**Use Cases**:
- Symptom scale studies
- Quality of life measures
- Biomarker investigations

### 6.3 Difference in Change Scores

**Description**: Absolute difference between groups in original units

**Input Parameters**:
- Difference range (measurement units)
- Pooled standard deviation

**Calculation**:
```
Cohen's d = difference / standard_deviation
```

**Use Cases**:
- Regulatory submissions
- Clinical meaningfulness assessment
- Stakeholder communication

### 6.4 Change in Active Group

**Description**: Treatment group outcome specification

**Input Parameters**:
- Active group change range
- Placebo group change
- Pooled standard deviation

**Calculation**:
```
Cohen's d = (placebo_change - active_change) / standard_deviation
```

**Use Cases**:
- Target product profile development
- Dose-response studies
- Benefit-risk assessment

## 7. Advanced Settings

### 7.1 Group Allocation Ratios

**Description**: Randomization ratio between active and control groups

**Options**:
- Range: 0.5:1 to 5:1 (active:control)
- Default: 1:1 (balanced allocation)
- Increment: 0.5

**Considerations**:
- Balanced allocation maximizes power
- Unequal allocation may be required for:
  - Resource constraints
  - Ethical considerations
  - Dose-escalation studies

### 7.2 Drop-in Rate

**Description**: Proportion of control participants receiving active treatment

**Parameters**:
- Range: 0% to 40%
- Default: 0%
- Impact: Reduces effective sample size

**Clinical Context**:
- Rescue medication use
- Protocol violations
- Crossover events

### 7.3 Type I Error Rate

**Description**: Probability of false positive finding

**Options**:
- Range: 0.01 to 0.20
- Default: 0.05 (conventional threshold)
- Common alternatives: 0.01, 0.025

**Regulatory Considerations**:
- FDA guidance: α = 0.05 standard
- Multiple testing adjustments
- Interim analysis planning

### 7.4 Test Directionality

**One-sided Tests**:
- Hypothesis: Treatment > Control
- Increased power for specific direction
- Regulatory acceptance varies

**Two-sided Tests**:
- Hypothesis: Treatment ≠ Control
- Standard for most clinical trials
- Conservative approach

## 8. Results Interpretation

### 8.1 Power Curve Analysis

**Visual Elements**:
- **Blue line**: Power across effect size range
- **Red dashed line**: 80% power threshold
- **Red dotted line**: Minimum detectable effect size

**Interpretation Guidelines**:
- **Above 80%**: Adequate power achieved
- **Below 80%**: Insufficient power for detection
- **Steep curves**: Rapid power increase with effect size
- **Flat curves**: Insufficient sample size

### 8.2 Key Metrics

**Maximum Power**:
- Highest power achieved in tested range
- Indicates study capability

**Minimum Detectable Effect**:
- Smallest effect achieving 80% power
- Critical for feasibility assessment

**Power Range**:
- Power variation across effect sizes
- Sensitivity indicator

### 8.3 Clinical Interpretation

**Effect Size Selection**:
- Consider clinical meaningfulness
- Review literature benchmarks
- Consult clinical experts
- Assess patient importance

**Sample Size Adequacy**:
- Compare with resource availability
- Consider recruitment feasibility
- Evaluate retention strategies
- Plan contingency scenarios

## 9. Report Generation

### 9.1 Export Options

The application provides three report formats:

**PDF Reports**:
- Professional formatting
- Regulatory submission quality
- Printable documentation

**HTML Reports**:
- Interactive elements
- Web-based sharing
- Multimedia support

**Word Documents**:
- Collaborative editing
- Template customization
- Track changes capability

### 9.2 Report Contents

Standard reports include:

**Study Parameters**:
- Sample size specifications
- Effect size methods
- Statistical assumptions

**Results Summary**:
- Power analysis findings
- Key metrics identification
- Interpretation guidance

**Technical Details**:
- Calculation methods
- Software information
- Reproducibility data

### 9.3 Customization Options

**Parameter Documentation**:
- Rationale for effect size selection
- Sample size justification
- Assumption validation

**Clinical Context**:
- Study objectives alignment
- Patient population description
- Outcome measure rationale

## 10. Troubleshooting

### 10.1 Common Issues

**Application Won't Launch**:
- Verify R and package installation
- Check browser JavaScript settings
- Restart R session
- Clear browser cache

**Slow Performance**:
- Close other browser tabs
- Reduce effect size range
- Restart application
- Check system resources

**Invalid Results**:
- Verify parameter ranges
- Check for negative values
- Ensure logical consistency
- Review error messages

### 10.2 Error Messages

**"Invalid parameters for power calculation"**:
- Check sample sizes > 0
- Verify effect sizes > 0
- Ensure α ∈ (0,1)

**"Effect size range too narrow"**:
- Increase range span
- Check minimum > 0
- Verify maximum > minimum

### 10.3 Performance Optimization

**Faster Calculations**:
- Reduce effect size points
- Simplify parameter ranges
- Close unnecessary applications
- Use local deployment

## 11. Best Practices

### 11.1 Study Planning Workflow

1. **Literature Review**: Establish expected effect sizes
2. **Clinical Input**: Confirm meaningful differences
3. **Parameter Exploration**: Test multiple scenarios
4. **Sensitivity Analysis**: Vary key assumptions
5. **Documentation**: Generate comprehensive reports

### 11.2 Effect Size Selection

**Evidence-Based Approach**:
- Systematic literature review
- Meta-analysis findings
- Previous trial results
- Clinical expert consensus

**Conservative Planning**:
- Use lower effect size estimates
- Account for population differences
- Consider measurement variability
- Plan interim analyses

### 11.3 Sample Size Considerations

**Recruitment Feasibility**:
- Site capacity assessment
- Population availability
- Competitive studies
- Seasonal variations

**Retention Strategies**:
- Historical dropout rates
- Protocol burden assessment
- Incentive planning
- Follow-up procedures

## 12. Frequently Asked Questions

### 12.1 General Questions

**Q: Can I use this for cluster randomized trials?**
A: No, the current version supports individual randomized parallel designs only.

**Q: Does the application handle multiple endpoints?**
A: No, power calculations assume a single primary endpoint.

**Q: Can I save my work session?**
A: Parameters are not automatically saved. Use the report generation feature for documentation.

### 12.2 Statistical Questions

**Q: How does the application handle unequal variances?**
A: The current implementation assumes equal variances (homoscedasticity).

**Q: What if my data isn't normally distributed?**
A: The t-test is robust to moderate non-normality, especially with adequate sample sizes.

**Q: Can I specify different dropout rates per group?**
A: The current version applies uniform dropout rates across groups.

### 12.3 Technical Questions

**Q: Which browsers are supported?**
A: All modern browsers with JavaScript enabled (Chrome, Firefox, Safari, Edge).

**Q: Can I deploy this on my organization's server?**
A: Yes, the package supports Shiny Server deployment for institutional use.

**Q: Is the application validated for regulatory submissions?**
A: The underlying statistical methods are standard, but validation responsibility remains with the user.

## Contact and Support

For additional assistance:

- **Package Issues**: https://github.com/rythomas/zzpower/issues
- **Statistical Questions**: Consult with qualified statisticians
- **Technical Support**: R community forums and documentation

---

*This manual provides comprehensive guidance for using the zzpower package effectively. For the most current information, please refer to the package documentation and GitHub repository.*