# jsurvival

[![R-CMD-check](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sbalci/jsurvival/actions/workflows/pkgdown.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![jamovi](https://img.shields.io/badge/jamovi-module-blue)](https://www.jamovi.org)
[![ClinicoPath](https://img.shields.io/badge/ClinicoPath-survival-orange)](https://sbalci.github.io/ClinicoPathJamoviModule/)

## Abstract

**jsurvival** is a comprehensive survival analysis module for jamovi that bridges the gap between advanced statistical methods and clinical research accessibility. As part of the ClinicoPath statistical suite, it transforms complex survival analyses into intuitive, publication-ready outputs with natural language interpretations. The module implements state-of-the-art survival analysis techniques including Kaplan-Meier estimation, Cox proportional hazards regression, and time-dependent analyses, while maintaining a user-friendly interface designed for medical researchers. By automating person-time calculations, providing automated statistical summaries in plain language, and generating high-quality visualizations, jsurvival enables clinicians and researchers to conduct sophisticated survival analyses without extensive programming knowledge, ultimately accelerating the translation of clinical data into actionable insights.

## 🎯 Key Features

### Core Survival Analysis Capabilities
- **Kaplan-Meier Analysis**: Generate survival curves with confidence intervals, risk tables, and median survival times
- **Cox Proportional Hazards Models**: Both univariate and multivariable regression with hazard ratios and forest plots
- **Person-Time Calculations**: Automated computation of person-years at risk with incidence rate calculations
- **Cut-point Analysis**: Optimal threshold determination for continuous biomarkers using multiple methods
- **Time-Dependent Analyses**: Support for time-varying covariates and landmark analysis
- **Competing Risks**: Handle multiple event types with cause-specific hazard functions

### Clinical Research Features
- **Natural Language Summaries**: Automated generation of plain-English interpretations of results
- **Clinical Trial Metrics**: 1-, 3-, and 5-year survival rates with confidence intervals
- **Stage Migration Analysis**: Evaluate the Will Rogers phenomenon in cancer staging with advanced TNM staging validation
- **Treatment Pathway Visualization**: Alluvial plots for treatment sequences over time
- **Subgroup Forest Plots**: Systematic evaluation of treatment effects across patient subgroups
- **Educational Explanations**: Built-in HTML explanations for key analysis concepts and methods

### Advanced Statistical Methods
- **Restricted Mean Survival Time (RMST)**: Alternative to median survival for skewed distributions
- **Time-Dependent ROC Curves**: Evaluate biomarker performance over time
- **LASSO-Cox Regression**: Variable selection for high-dimensional survival data
- **Integrated Discrimination Improvement (IDI)**: Compare predictive models
- **Schoenfeld Residual Diagnostics**: Test proportional hazards assumptions

### User Experience Enhancements
- **Intuitive GUI**: Point-and-click interface within jamovi, no coding required
- **Clinical Analysis Presets**: Pre-configured settings for common study types (overall survival, disease-free survival, treatment effectiveness, post-surgical outcomes)
- **Guided Setup Mode**: Step-by-step guidance for users new to survival analysis
- **Smart Defaults**: Evidence-based default settings for common analyses
- **Educational Tooltips**: Context-sensitive help explaining statistical concepts
- **Export Options**: Publication-ready tables and figures in multiple formats
- **Reproducible Reports**: Generate complete analysis reports with code

## 📊 Available Analysis Modules

| Module | Description | Key Features |
|--------|-------------|--------------|
| **Single Arm Survival** | Analyze survival in a single cohort | Clinical presets, guided mode, overall survival rates, median survival, person-time calculations |
| **Survival Analysis** | Compare survival between groups | Educational explanations, log-rank test, Cox regression, stratified analysis |
| **Continuous Survival** | Analyze continuous predictors | Educational explanations, optimal cut-point detection, tertile/quartile analysis |
| **Multivariable Survival** | Adjust for multiple factors | Educational explanations, stepwise selection, interaction terms, adjusted curves |
| **Odds Ratio Analysis** | Binary outcome analysis | Educational explanations, 2x2 tables, forest plots, Mantel-Haenszel methods |
| **Stage Migration** | Will Rogers phenomenon | Advanced TNM staging, stage-specific survival, migration matrices, trend analysis |

## 🚀 Installation

### In jamovi (Recommended)
1. Open jamovi
2. Click the **+** button → **jamovi library**
3. Search for "ClinicoPath" or "jsurvival"
4. Click **Install**

### As R Package
```r
# Install from GitHub
devtools::install_github("sbalci/jsurvival")

# Load the package
library(jsurvival)
```

### System Requirements
- jamovi ≥ 1.8.1
- R ≥ 4.1.0
- Dependencies: survival, survminer, finalfit, ggplot2, dplyr

## 📖 Documentation

- **Package Documentation**: https://www.serdarbalci.com/jsurvival/
- **ClinicoPath Suite**: https://sbalci.github.io/ClinicoPathJamoviModule/
- **Tutorials**: Available in the `vignettes/` directory
- **Example Data**: Included datasets for learning and testing

## 💡 Quick Example

### In jamovi:
1. Load your survival data
2. Navigate to **Survival** → **ClinicoPath Survival** → **Survival Analysis**
3. Set variables:
   - **Time Elapsed**: Time to event variable
   - **Outcome**: Event indicator (0/1)
   - **Explanatory**: Grouping variable
4. Click **Run**

### In R:
```r
# Load example data
data("melanoma", package = "jsurvival")

# Run survival analysis
result <- jsurvival::survival(
    data = melanoma,
    elapsedtime = "time",
    outcome = "status", 
    explanatory = "sex"
)

# View results
result$run()
```

## 📝 Citation

If you use jsurvival in your research, please cite the main ClinicoPath project:

```
Serdar Balci (2025). ClinicoPath jamovi Module. doi:10.5281/zenodo.3997188
[R package]. Retrieved from https://github.com/sbalci/ClinicoPathJamoviModule
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## 📄 License

This project is licensed under the GPL (≥ 2) License - see the [LICENSE.md](LICENSE.md) file for details.

## 💬 Support

- **Bug Reports**: [GitHub Issues](https://github.com/sbalci/ClinicoPathJamoviModule/issues/)
- **Questions**: [Discussions](https://github.com/sbalci/jsurvival/discussions)
- **Email**: serdarbalci@serdarbalci.com
- **ORCID**: [0000-0002-7852-3851](https://orcid.org/0000-0002-7852-3851)

## 🙏 Acknowledgments

This project builds upon the excellent work of the R survival analysis community, particularly the authors of the survival, survminer, and finalfit packages. Special thanks to the jamovi team for creating an accessible statistical platform.

---

Part of the [ClinicoPath](https://sbalci.github.io/ClinicoPathJamoviModule/) suite of statistical modules for biomedical research.