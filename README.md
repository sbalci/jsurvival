# jsurvival

[![R-CMD-check](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![jamovi](https://img.shields.io/badge/jamovi-module-blue)](https://www.jamovi.org)
[![ClinicoPath](https://img.shields.io/badge/ClinicoPath-survival-orange)](https://www.serdarbalci.com/ClinicoPathJamoviModule/)

## Abstract

**jsurvival** is a comprehensive time-to-event and clinical survival analysis module for jamovi and R. As the survival analysis engine of the **ClinicoPath** ecosystem, it bridges advanced biostatistical methods and clinical research practice. It provides publication-ready survival curves, Cox proportional hazards modeling, regularized high-dimensional feature selection (LASSO-Cox), date interval calculators, and natural language clinical interpretations—all without requiring programming expertise.

---

## 🎯 Key Features & Analysis Suite (9 Analyses)

jsurvival provides **9 dedicated analyses** under the **Survival** menu in jamovi:

| Analysis | Function | Category | Key Clinical Features |
| :--- | :--- | :--- | :--- |
| **Survival Analysis** | `survival` | Core Time-to-Event | Kaplan-Meier estimation, log-rank comparisons between groups, Cox proportional hazards regression, median survival with CIs, 1-, 3-, and 5-year survival rates, person-time calculations, and publication-ready risk tables. |
| **Single Arm Survival** | `singlearm` | Cohort Follow-up | Cohort-wide survival analysis for single-arm clinical trials or registry cohorts without an explanatory group; includes guided setup, milestone survival rates, and person-time metrics. |
| **Multivariable Survival** | `multisurvival` | Advanced Modeling | Multivariable Cox proportional hazards modeling with covariate adjustment, hazard ratio forest plots, model diagnostics, and adjusted survival curve visualization. |
| **Continuous Survival** | `survivalcont` | Biomarker Thresholds | Survival analysis for continuous variables and biomarkers; features automated optimal cut-point detection (maxstat), median/tertile/quartile splits, and stratified Kaplan-Meier plots. |
| **Odds Ratio Analysis** | `oddsratio` | Association Studies | Binary outcome evaluation with 2x2 contingency tables, odds ratio calculations with exact/Wald confidence intervals, and publication-ready forest plots. |
| **LASSO-Cox Regression** | `lassocox` | High-Dimensional Modeling | L1-penalized Cox regression via glmnet for high-dimensional feature selection (clinical, molecular, genomic), k-fold cross-validation, optimal lambda selection, and coefficient path plots. |
| **Time Interval Calculator** | `timeinterval` | Clinical Data Prep | Robust calculation of follow-up durations from diagnosis and event/censoring dates, supporting days, months, and years with date sequence and consistency checks. |
| **DateTime Converter** | `datetimeconverter` | Clinical Data Prep | Flexible date and timestamp parsing, standardization, and conversion into standardized temporal formats required for time-to-event analysis. |
| **Outcome Organizer** | `outcomeorganizer` | Endpoint Derivation | Clinical endpoint mapping and standardization, transforming disparate event statuses and dates into standardized time-to-event and censoring indicators (OS, DFS, PFS). |

---

## 🚀 Installation

### In jamovi (Recommended)

1. Open **jamovi** (>= 2.6).
2. Click the **+** button in the top-right corner → **jamovi library**.
3. Search for **jsurvival** (or browse under **Survival**).
4. Click **Install**.

### As an R Package

```r
# Install from GitHub
remotes::install_github("sbalci/jsurvival")
```

---

## 💡 Quick Start (R Interface)

```r
library(jsurvival)

# Load included melanoma survival dataset
data("melanoma", package = "jsurvival")

# 1. Univariate Kaplan-Meier & Cox survival analysis
fit <- jsurvival::survival(
  data = melanoma,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "1",
  explanatory = "sex"
)

# 2. Continuous biomarker threshold survival analysis
cut_fit <- jsurvival::survivalcont(
  data = melanoma,
  elapsedtime = "time",
  outcome = "status",
  outcomeLevel = "1",
  contexplan = "age"
)

# 3. High-dimensional LASSO-Cox regression
data("lassocox_breast_cancer", package = "jsurvival")
lasso_fit <- jsurvival::lassocox(
  data = lassocox_breast_cancer,
  time = "time",
  status = "status",
  predictors = vars(ER, PR, HER2, Grade, NodeStatus, Ki67)
)
```

---

## 📖 Documentation & Resources

- **Module Website & Vignettes**: [https://www.serdarbalci.com/jsurvival/](https://www.serdarbalci.com/jsurvival/)
- **ClinicoPath Umbrella Ecosystem**: [https://www.serdarbalci.com/ClinicoPathJamoviModule/](https://www.serdarbalci.com/ClinicoPathJamoviModule/)
- **GitHub Repository**: [https://github.com/sbalci/jsurvival/](https://github.com/sbalci/jsurvival/)
- **Issue Tracker**: [GitHub Issues](https://github.com/sbalci/ClinicoPathJamoviModule/issues)

---

## 📝 Citation

If you use jsurvival in your research or publications, please cite:

```bibtex
@manual{balci2026clinicopath,
  title  = {ClinicoPath: jamovi Module for Clinicopathological Research},
  author = {Serdar Balci},
  year   = {2026},
  url    = {https://www.serdarbalci.com/ClinicoPathJamoviModule/},
  doi    = {10.5281/zenodo.3997188}
}
```

## 📄 License

This project is licensed under the GPL (>= 2) License — see the [LICENSE.md](LICENSE.md) file for details.
