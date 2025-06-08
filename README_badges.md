# README Badges and Widgets for jsurvival

## Status Badges

### Build and Quality
[![R-CMD-check](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

### jamovi Module Specific
[![jamovi](https://img.shields.io/badge/jamovi-module-blue)](https://www.jamovi.org)
[![jamovi version](https://img.shields.io/badge/jamovi-%E2%89%A5%201.8.1-blue)](https://www.jamovi.org)
[![ClinicoPath](https://img.shields.io/badge/ClinicoPath-survival-orange)](https://sbalci.github.io/ClinicoPathJamoviModule/)

### Package Information
[![CRAN status](https://www.r-pkg.org/badges/version/jsurvival)](https://CRAN.R-project.org/package=jsurvival)
[![R version](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue)](https://www.r-project.org/)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)

### Documentation and Support
[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue)](https://www.serdarbalci.com/jsurvival/)
[![Website](https://img.shields.io/badge/website-ClinicoPath-orange)](https://sbalci.github.io/ClinicoPathJamoviModule/)

### Download and Usage Stats
[![GitHub release](https://img.shields.io/github/release/sbalci/jsurvival.svg)](https://GitHub.com/sbalci/jsurvival/releases/)
[![GitHub commits](https://img.shields.io/github/commits-since/sbalci/jsurvival/v0.0.3.21.svg)](https://GitHub.com/sbalci/jsurvival/commit/)
[![GitHub issues](https://img.shields.io/github/issues/sbalci/jsurvival.svg)](https://GitHub.com/sbalci/jsurvival/issues/)

## Installation Widgets

### jamovi Installation
```markdown
## Installation in jamovi

### Option 1: jamovi Library (Recommended)
1. Open jamovi
2. Click on the **+** button in the top-right corner
3. Select **jamovi library**
4. Search for "ClinicoPath" or "jsurvival"
5. Click **Install**

### Option 2: Manual Installation
1. Download the latest `.jmo` file from [releases](https://github.com/sbalci/jsurvival/releases)
2. Open jamovi
3. Click **+** → **Sideload** → Select the `.jmo` file
```

### R Package Installation
```markdown
## Installation as R Package

### From GitHub (Development Version)
```r
# Install devtools if you haven't already
install.packages("devtools")

# Install jsurvival
devtools::install_github("sbalci/jsurvival")
```

### Dependencies
This package requires R ≥ 4.1.0 and depends on:
- survival, survminer, finalfit
- ggplot2, dplyr, tidyr
- jmvcore, R6
```

## Feature Highlights

### Analysis Types Available
```markdown
## 📊 Available Analyses

| Analysis | Description | Use Case |
|----------|-------------|----------|
| **Single Arm Survival** | Overall survival for entire cohort | Population-level survival estimates |
| **Survival Analysis** | Univariate Cox regression & Kaplan-Meier | Group comparisons, risk factors |
| **Continuous Variable Survival** | Cut-off analysis for continuous predictors | Biomarker threshold analysis |
| **Multivariable Survival** | Adjusted Cox regression models | Multiple risk factors |
| **Odds Ratio Analysis** | Binary outcome analysis | Case-control studies |
| **Time Interval Calculator** | Calculate time differences | Data preparation |
```

### Output Features
```markdown
## 🎯 Key Features

- **📈 Kaplan-Meier Curves** with risk tables and confidence intervals
- **📊 Survival Tables** with 1, 3, 5-year survival rates
- **🔢 Natural Language Summaries** of results
- **📋 Hazard Ratio Tables** with confidence intervals
- **⚕️ Clinical Research Focus** designed for medical studies
- **🌐 Multi-language Support** (English, Turkish, German)
```

## Example Usage Widget

```markdown
## 🚀 Quick Start Example

### In jamovi:
1. Load your survival data
2. Go to **Survival** → **ClinicoPath Survival** → **Survival Analysis**
3. Set **Time Elapsed** variable (time to event)
4. Set **Explanatory** variable (grouping factor)
5. Set **Outcome** variable (event indicator)
6. Click **Run**

### Sample Output:
- Median survival times with 95% CI
- Log-rank test p-values
- Hazard ratios with confidence intervals
- Kaplan-Meier survival curves
- Risk tables at specified time points
```

## Citation Widget

```markdown
## 📝 Citation

If you use jsurvival in your research, please cite:

```
Balci, S. (2024). jsurvival: Survival Module of ClinicoPath for jamovi. 
R package version 0.0.3.21. 
https://github.com/sbalci/jsurvival
```

**BibTeX:**
```bibtex
@Manual{jsurvival,
  title = {jsurvival: Survival Module of ClinicoPath for jamovi},
  author = {Serdar Balci},
  year = {2024},
  note = {R package version 0.0.3.21},
  url = {https://github.com/sbalci/jsurvival},
}
```
```

## Related Projects Widget

```markdown
## 🔗 Related Projects

| Project | Description | Link |
|---------|-------------|------|
| **ClinicoPath** | Main jamovi module suite | [![ClinicoPath](https://img.shields.io/badge/GitHub-ClinicoPath-blue)](https://github.com/sbalci/ClinicoPathJamoviModule) |
| **jamovi** | Statistical software platform | [![jamovi](https://img.shields.io/badge/jamovi.org-platform-blue)](https://www.jamovi.org) |
| **Survival Analysis Tutorial** | Learn survival analysis | [![Tutorial](https://img.shields.io/badge/tutorial-survival-green)](https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html) |
```

## Support Widget

```markdown
## 💬 Support & Community

- 🐛 **Report Issues**: [GitHub Issues](https://github.com/sbalci/ClinicoPathJamoviModule/issues/)
- 📧 **Contact**: serdarbalci@serdarbalci.com
- 🌐 **Website**: [ClinicoPath Documentation](https://sbalci.github.io/ClinicoPathJamoviModule/)
- 📖 **Package Documentation**: [jsurvival docs](https://www.serdarbalci.com/jsurvival/)

[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--7852--3851-green)](https://orcid.org/0000-0002-7852-3851)
```