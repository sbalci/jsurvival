# jsurvival 0.0.32.60 (2025-12-28)

## Major Updates

### New Modules
- **Date Validator**: Comprehensive date and datetime validation tool.
  - Validates and diagnoses messy date/datetime formats using multiple methods (datefixR, anytime, lubridate).
  - Provides detailed audit tables and quality assessment reports.

### Re-introduced and Enhanced Modules
- **Outcome Organizer**: Re-introduced with significant enhancements.
  - Improved data cleaning and label handling.
  - Contextual validation for different survival analysis types (OS, Cause-specific, etc.).
  - Diagnostic messages and glossary for educational support.
- **Time Interval**: Re-introduced as a comprehensive calculator.
  - Robust time interval calculations with landmark analysis support.
  - Person-time calculation for epidemiological studies.
  - Data quality assessment for time intervals.

## Enhancements

### Survival Analysis
- **Parametric Models**: Improved handling of covariates and formula construction.
- **Plots**: Added logic to skip certain plots (e.g., PH assumption, residuals) for competing risk analysis to avoid errors.
- **Diagnostics**: Enhanced model summaries and p-value calculations.

### General
- **Notice System**: Improved notice generation and handling across modules.
- **Documentation**: Updated documentation for all modules.

---

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
