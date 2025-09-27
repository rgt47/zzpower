# Statistical Methodology: zzpower Package

## Abstract

This document provides a comprehensive technical specification of the statistical methods implemented in the `zzpower` package for power analysis and sample size determination in two-group parallel clinical trial designs. The methodology encompasses effect size parameterization, power calculation procedures, and sample size adjustments for realistic trial conditions.

## 1. Theoretical Foundation

### 1.1 Statistical Power Framework

Statistical power (1 - β) represents the probability of rejecting a false null hypothesis, where β denotes the Type II error rate. For two-group comparisons, power depends on four fundamental parameters:

- **Effect size (δ)**: Magnitude of the true difference between groups
- **Sample size (n)**: Number of observations per group
- **Significance level (α)**: Type I error rate
- **Test directionality**: One-sided versus two-sided hypothesis testing

### 1.2 Two-Sample t-Test Framework

The package implements power analysis for the two-sample t-test comparing means between independent groups:

**Null hypothesis**: H₀: μ₁ = μ₂
**Alternative hypothesis**: H₁: μ₁ ≠ μ₂ (two-sided) or μ₁ > μ₂ (one-sided)

The test statistic follows:

```
t = (x̄₁ - x̄₂) / (sp√(1/n₁ + 1/n₂))
```

where sp represents the pooled standard deviation:

```
sp = √[((n₁-1)s₁² + (n₂-1)s₂²) / (n₁+n₂-2)]
```

### 1.3 Non-Central t-Distribution

Power calculations utilize the non-central t-distribution with non-centrality parameter:

```
λ = δ√(n₁n₂/(n₁+n₂))
```

where δ represents Cohen's d effect size:

```
δ = (μ₁ - μ₂) / σ
```

## 2. Effect Size Parameterization

### 2.1 Cohen's d (Standardized Mean Difference)

Direct specification of standardized effect size following Cohen's (1988) conventions:

```
d = (μ₁ - μ₂) / σpooled
```

**Interpretation guidelines**:
- Small effect: d = 0.2
- Medium effect: d = 0.5
- Large effect: d = 0.8

### 2.2 Percentage Reduction Method

Effect size specification based on proportional change from control group mean:

```
d = (percentage_reduction × μ₀) / σ₀
```

where:
- μ₀ = control group mean
- σ₀ = pooled standard deviation
- percentage_reduction ∈ [0, 1]

**Clinical relevance**: Particularly applicable for symptom scales, biomarkers, and quality-of-life measures where proportional improvements have direct clinical interpretation.

### 2.3 Difference in Change Scores

Effect size based on absolute difference in outcome units:

```
d = difference / σpooled
```

where difference represents the absolute difference between group means in original measurement units.

**Application context**: Enables direct clinical interpretation and facilitates regulatory submission requirements where absolute thresholds are specified.

### 2.4 Active Group Change Specification

Effect size derived from treatment group outcome specification:

```
d = (μ₀ - active_outcome) / σ₀
```

where:
- μ₀ = control group mean
- active_outcome = specified treatment group outcome
- σ₀ = pooled standard deviation

**Utility**: Facilitates clinical target setting and dose-response modeling applications.

## 3. Power Calculation Procedures

### 3.1 Core Algorithm

Power calculations utilize the `pwr.t2n.test()` function from the `pwr` package, implementing the exact solution for two-sample t-tests with unequal sample sizes.

**Mathematical formulation**:

```
Power = P(|T| > t_{α/2,df} | λ)
```

where:
- T follows a non-central t-distribution with df = n₁ + n₂ - 2 degrees of freedom
- t_{α/2,df} represents the critical value from the central t-distribution
- λ = non-centrality parameter

### 3.2 Vectorized Calculations

The application computes power across effect size ranges using vectorized operations:

```r
effect_range <- seq(d_min, d_max, length.out = 16)
power_values <- sapply(effect_range, function(d) {
  pwr.t2n.test(n1 = n1, n2 = n2, sig.level = alpha, d = d)$power
})
```

### 3.3 Error Handling

Robust error handling addresses:
- **Invalid sample sizes** (n ≤ 0)
- **Extreme effect sizes** causing numerical instability
- **Boundary conditions** (α ∈ (0,1), power ∈ [0,1])

```r
power_values <- sapply(d_values, function(d) {
  tryCatch({
    pwr.t2n.test(n1 = n1_comp, n2 = n2_comp, sig.level = sig_level, d = d)$power
  }, error = function(e) NA)
})
```

## 4. Sample Size Adjustments

### 4.1 Dropout Rate Modeling

The application adjusts planned sample sizes for expected attrition:

**Completer sample size**:
```
n_completer = n_ITT × (1 - dropout_rate - dropin_rate)
```

where:
- n_ITT = intention-to-treat sample size
- dropout_rate = proportion of participants withdrawing
- dropin_rate = proportion of control participants receiving treatment

### 4.2 Unequal Allocation Ratios

For allocation ratio r (active:control):

**Group sample sizes**:
```
n₁ = (r × N_total) / (r + 1)  # Active group
n₂ = N_total / (r + 1)        # Control group
```

**Efficiency considerations**: Optimal allocation for equal group sizes (r = 1) maximizes statistical power for fixed total sample size.

### 4.3 Type I Error Adjustment

**Two-sided testing**:
```
α_effective = α_nominal / 2
```

**One-sided testing**:
```
α_effective = α_nominal
```

Implementation:
```r
sided <- if(one_sided) 1 else 2
sig_level <- type1_error / sided
```

## 5. Numerical Considerations

### 5.1 Precision and Accuracy

The implementation maintains numerical precision through:

- **Double-precision arithmetic** for all calculations
- **Validated algorithms** from the `pwr` package
- **Boundary condition handling** for extreme parameters
- **Convergence criteria** for iterative procedures

### 5.2 Computational Complexity

Power calculations exhibit O(1) complexity per effect size value, with vectorized operations providing O(n) scaling for n effect sizes.

### 5.3 Numerical Stability

The non-central t-distribution calculations remain stable across the practical parameter range:
- Effect sizes: d ∈ [0, 5]
- Sample sizes: n ∈ [2, 10,000]
- Significance levels: α ∈ [0.001, 0.5]

## 6. Validation Procedures

### 6.1 Analytical Verification

The implementation validates against known analytical results:

**Large sample approximation**:
For large n, power approximates:
```
Φ(δ√(n/2) - z_{α/2})
```
where Φ represents the standard normal cumulative distribution function.

### 6.2 Benchmark Comparisons

Systematic validation against established software:
- **SAS PROC POWER**: Cross-validation for standard scenarios
- **G*Power**: Verification of Cohen's d calculations
- **R power.t.test()**: Consistency checking for equal sample sizes

### 6.3 Monte Carlo Validation

Selected scenarios validated through simulation studies:

```r
# Simulation framework
simulate_power <- function(n1, n2, delta, alpha, nsim = 10000) {
  p_values <- replicate(nsim, {
    group1 <- rnorm(n1, mean = delta, sd = 1)
    group2 <- rnorm(n2, mean = 0, sd = 1)
    t.test(group1, group2)$p.value
  })
  mean(p_values < alpha)
}
```

## 7. Limitations and Assumptions

### 7.1 Distributional Assumptions

The methodology assumes:
- **Normality**: Outcome variables follow normal distributions
- **Homoscedasticity**: Equal variances across groups
- **Independence**: Observations are statistically independent

### 7.2 Design Constraints

Current implementation limitations:
- **Parallel designs only**: No crossover or cluster randomized trials
- **Continuous outcomes**: No binary or time-to-event endpoints
- **Fixed sample sizes**: No adaptive or sequential designs

### 7.3 Practical Considerations

**Robustness**: t-tests demonstrate robustness to moderate violations of normality, particularly with balanced designs and adequate sample sizes (n ≥ 30 per group).

**Clinical significance**: Statistical power calculations require specification of clinically meaningful effect sizes, necessitating domain expertise and stakeholder input.

## 8. Extensions and Future Developments

### 8.1 Alternative Test Statistics

Potential enhancements include:
- **Welch's t-test**: Unequal variance adjustment
- **Mann-Whitney U**: Non-parametric alternative
- **Bayesian tests**: Prior information incorporation

### 8.2 Advanced Designs

Future methodology extensions:
- **Equivalence testing**: Non-inferiority and bioequivalence
- **Multiple comparisons**: Family-wise error rate control
- **Interim analyses**: Group sequential and adaptive designs

### 8.3 Sensitivity Analysis

Enhanced functionality for:
- **Parameter uncertainty**: Confidence intervals for power
- **Assumption violations**: Robustness assessment
- **Multi-scenario planning**: Range-based sample size determination

## References

1. Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum Associates.

2. Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press.

3. Julious, S. A. (2004). Sample sizes for clinical trials with normal data. Statistics in Medicine, 23(12), 1921-1986.

4. Lachin, J. M. (1981). Introduction to sample size determination and power analysis for clinical trials. Controlled Clinical Trials, 2(2), 93-113.

5. Owen, D. B. (1965). A special case of a bivariate non-central t-distribution. Biometrika, 52(3-4), 437-446.

6. Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package version 1.3-0.

7. Welch, B. L. (1947). The generalization of 'Student's' problem when several different population variances are involved. Biometrika, 34(1-2), 28-35.