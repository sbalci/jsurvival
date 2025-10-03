# jsurvival 0.0.31.84 (2025-10-03)

## New Features

### Educational Explanations
- Added `showExplanations` option to survival, survivalcont, multisurvival, and oddsratio modules
- Built-in HTML explanations for key analysis concepts and statistical methods
- Context-sensitive educational content to help users understand their analyses

### Single Arm Survival Enhancements
- **Clinical Analysis Presets**: Pre-configured settings for common study types
  - Overall survival analysis (most common)
  - Disease-free survival analysis
  - Treatment effectiveness study
  - Post-surgical outcomes
  - Custom analysis (advanced users)
- **Guided Setup Mode**: Step-by-step guidance for users new to survival analysis
- Enhanced person-time calculations and natural language summaries

### Stage Migration Analysis
- Advanced TNM staging validation and trend analysis
- Improved robustness and explanations for stagemigration module
- Multifactorial analysis capabilities
- Granular table controls and debug outputs

### Survival Analysis Module
- Enhanced educational explanations for univariate survival
- Improved Cox regression output and interpretation
- Additional visualization options

### Continuous Survival Analysis
- Educational explanations for cut-point analysis
- Enhanced optimal threshold determination methods
- Improved output tables and visualizations

### Multivariable Survival
- Educational explanations for multivariable models
- Enhanced model diagnostics and output

### Odds Ratio Analysis
- Educational explanations for odds ratio interpretation
- Improved forest plots and summary tables

## Module Removals

- Removed `outcomeorganizer` module (functionality integrated into other modules)
- Removed `timeinterval` module (functionality integrated into other modules)

## Bug Fixes and Improvements

- Enhanced diagnostic test utility functions
- Improved tree analysis for survival models
- Better handling of missing data
- Updated documentation across all modules

## Documentation

- Updated package documentation and vignettes
- Enhanced inline help and tooltips
- Improved example datasets and use cases

---

# jsurvival 0.0.3.90 (2024-07-31)

## Major Changes

- Initial implementation of educational explanations system
- Refactored analysis modules for better user experience
- Enhanced natural language summary generation

## New Features

- Stage migration analysis for cancer staging studies
- Advanced survival model options
- Diagnostic test utility functions

---

# jsurvival 0.0.3.0 and earlier

For changes in earlier versions, please refer to the git commit history.
