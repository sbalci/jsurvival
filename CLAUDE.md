# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **jsurvival**, a jamovi module for survival analysis that is part of the ClinicoPath statistical analysis suite. It provides survival analysis functions with natural language summaries, Kaplan-Meier plots, Cox regression, and various survival-related calculations for medical research.

## Development Commands

### Package Development Workflow
```r
# Documentation and building
devtools::document()          # Generate documentation from roxygen comments
devtools::check()            # Run R CMD check 
devtools::build()            # Build package
devtools::install()          # Install package locally

# Website documentation
pkgdown::build_site()        # Build documentation website locally
```

### Testing
- No formal test suite currently exists
- To add testing: `usethis::use_testthat()` and `usethis::use_test("function-name")`

## Architecture

### jamovi Module Structure
- **R functions**: Located in `/R/` with paired `.h.R` (header/options) and `.b.R` (body/implementation) files
- **jamovi configuration**: YAML files in `/jamovi/` define module interface and menu structure
- **Analysis types**: 
  - `singlearm`: Single arm survival analysis
  - `survival`: Univariate survival analysis with Cox regression
  - `survivalcont`: Survival analysis for continuous variables with cut-offs
  - `multisurvival`: Multivariable Cox regression
  - `oddsratio`: Odds ratio calculations for binary outcomes
  - `timeinterval`: Time interval calculations

### Key Dependencies
- **Core**: jmvcore (jamovi framework), R6 (class system)
- **Survival analysis**: survival, survminer, finalfit, rms, KMunicate
- **Data manipulation**: dplyr, tidyr, purrr, janitor
- **Visualization**: ggplot2, scales

### File Patterns
- Analysis functions: `{name}.h.R` (options/header) + `{name}.b.R` (implementation)
- jamovi config: `{name}.a.yaml` (analysis), `{name}.r.yaml` (results), `{name}.u.yaml` (UI)
- Documentation: Roxygen2 comments with markdown support

## CI/CD and Automation

- **GitHub Actions**: Runs R CMD check on multiple platforms, skips "WIP" commits
- **Documentation**: Auto-deployed to https://www.serdarbalci.com/jsurvival/ via pkgdown
- **Package checking**: Multi-platform testing (macOS, Windows, Ubuntu)

## Development Notes

- Uses devtools workflow with roxygen2 documentation
- No formal testing framework - consider adding testthat
- All exported functions should have proper roxygen documentation
- Follow existing code patterns for new analyses (header + body structure)
- Version managed in DESCRIPTION file (currently 0.0.3.21)