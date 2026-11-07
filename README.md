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
| **Single Arm Survival** | `singlearm` | Cohort Follow-up | Cohort-wide survival analysis for single-arm clinical trials or registry cohorts without an explanatory group; includes milestone survival rates, and person-time metrics. |
| **Multivariable Survival** | `multisurvival` | Advanced Modeling | Multivariable Cox proportional hazards modeling with covariate adjustment, Fine-Gray regression for competing events, hazard ratio forest plots, model diagnostics, and adjusted survival curves. |
| **Continuous Survival** | `survivalcont` | Biomarker Thresholds | Survival analysis for continuous variables and biomarkers; features a data-derived cut-off (maximally selected rank statistic, with a p-value adjusted for the search; it needs independent validation), multiple cut-offs by quantile, recursive, tree or minimum-p methods, and Kaplan-Meier plots by group. |
| **Odds Ratio Analysis** | `oddsratio` | Association Studies | Logistic-regression odds ratios with profile-likelihood confidence intervals, optional Firth penalized estimation, forest plot, prediction nomogram and optional diagnostic metrics. Both outcome levels are named explicitly. |
| **LASSO-Cox Regression** | `lassocox` | High-Dimensional Modeling | L1-penalized Cox regression via glmnet for high-dimensional feature selection (clinical, molecular, genomic), k-fold cross-validation, optimal lambda selection, and coefficient path plots. |
| **Time Interval Calculator** | `timeinterval` | Clinical Data Prep | Robust calculation of follow-up durations from diagnosis and event/censoring dates, supporting days, weeks, months, and years with date sequence and consistency checks. |
| **DateTime Converter** | `datetimeconverter` | Clinical Data Prep | Flexible date and timestamp parsing, standardization, and conversion into standardized temporal formats required for time-to-event analysis. |
| **Outcome Organizer** | `outcomeorganizer` | Endpoint Derivation | Clinical endpoint mapping and standardization, recoding event statuses into analysis-ready indicators for OS, cause-specific, competing-risk, RFS, PFS, DFS, TTP and multistate analyses. |

---

## 🚀 Installation

### In jamovi (Recommended)

1. Open **jamovi** (>= 2.7.27).
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
data("melanoma", package = "jsurvival")
# status: 1 = died of melanoma, 2 = alive, 3 = died of other causes. time is in days.
# Only status 1 is an event here, so other-cause deaths are censored (cause-specific).

# 1. Kaplan-Meier, log-rank and Cox by group
fit <- jsurvival::survival(
  data = melanoma, elapsedtime = "time",
  outcome = "status", outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = "sex",
  timetypeoutput = "days", cutp = "365, 1095, 1825"
)

# 2. Continuous variable, with a data-derived cut-off
cut_fit <- jsurvival::survivalcont(
  data = melanoma, elapsedtime = "time",
  outcome = "status", outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  contexpl = "age", findcut = TRUE,
  timetypeoutput = "days", cutp = "365, 1095, 1825"
)

# 3. LASSO Cox
data("lassocox_breast_cancer", package = "jsurvival")
lasso_fit <- jsurvival::lassocox(
  data = lassocox_breast_cancer, elapsedtime = "survival_months",
  outcome = "death", outcomeLevel = "Dead", censorLevel = "Alive",
  explanatory = c("age", "tumor_size_cm", "grade", "stage", "lymph_nodes_positive",
                  "er_status", "pr_status", "her2_status", "ki67_percent", "lvi"),
  lambda = "lambda.min"
)
```

## Example data

In jamovi, open ☰ → Open → Data Library → jsurvival → *histopathology including analysis*. It holds a
simulated cohort, with invented patient names, and a saved analysis: Time Elapsed = OverallTime (months),
Outcome = Outcome, Event Level = 1. From R, `histopathology`, `melanoma` and six `lassocox_*` datasets are
available.

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
