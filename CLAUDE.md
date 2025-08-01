# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**jsurvival** is a jamovi module for survival analysis, part of the ClinicoPath statistical analysis suite. It provides comprehensive survival analysis functions with natural language summaries, Kaplan-Meier plots, Cox regression models, and various survival-related calculations optimized for medical research.

- **Main website**: https://www.serdarbalci.com/jsurvival/
- **ClinicoPath suite**: https://sbalci.github.io/ClinicoPathJamoviModule/
- **Current version**: 0.0.3.90

## Development Commands

### Core Development Workflow
```r
# Build and check workflow
devtools::document()         # Generate documentation from roxygen2
devtools::check()           # Run R CMD check locally
devtools::build()           # Build package tarball
devtools::install()         # Install package locally for testing

# Build jamovi module
jmvtools::install()         # Build and install as jamovi module

# Documentation website
pkgdown::build_site()       # Build docs site locally
pkgdown::preview_site()     # Preview in browser
```

### Testing Commands
```r
# Run existing tests (limited to stagemigration currently)
devtools::test()            # Run all tests
devtools::test_active_file() # Run current test file

# Run single test
testthat::test_file("tests/testthat/test-stagemigration.R")

# To add new tests
usethis::use_test("function-name")  # Create test file template
```

### jamovi Module Building
```bash
# Build jamovi module file (.jmo)
R -e "jmvtools::install()"

# The .jmo file will be created in the build directory
# Current build: jsurvival_0.0.3.90-mac.jmo
```

## Architecture

### jamovi Module Structure

The project follows jamovi's R6-based architecture with paired files:

1. **R Functions** (`/R/`):
   - `.h.R` files: Auto-generated headers defining analysis options
   - `.b.R` files: Implementation bodies containing analysis logic
   - Pattern: Each analysis has both `{name}.h.R` and `{name}.b.R`

2. **jamovi Configuration** (`/jamovi/`):
   - `0000.yaml`: Module metadata and menu structure
   - `.a.yaml` files: Analysis definitions
   - `.r.yaml` files: Results specifications  
   - `.u.yaml` files: UI configurations

3. **Analysis Modules**:
   - `singlearm`: Single arm survival analysis (whole cohort)
   - `survival`: Univariate survival with group comparisons
   - `survivalcont`: Continuous variable survival with cut-off analysis
   - `multisurvival`: Multivariable Cox regression
   - `oddsratio`: Odds ratio for binary outcomes
   - `timeinterval`: Time interval calculations (hidden in menu)
   - `stagemigration`: Stage migration analysis (in development)
   - `outcomeorganizer`: Outcome data organization (in development)

### Key Dependencies

- **Core Framework**: jmvcore (jamovi), R6 (classes)
- **Survival Analysis**: survival, survminer, finalfit, rms, KMunicate
- **Time-dependent**: pammtools, mgcv, timeROC
- **Utilities**: dplyr, tidyr, purrr, janitor, glue
- **Visualization**: ggplot2, scales
- **Validation**: checkmate, boot, pROC

### Code Patterns

1. **Analysis Implementation**:
   ```r
   # In {name}.b.R
   {name}Class <- R6::R6Class(
       "{name}Class",
       inherit = {name}Base,
       private = list(
           .run = function() { ... },
           .plot = function() { ... }
       )
   )
   ```

2. **Natural Language Summaries**: 
   - Generated using glue templates
   - Stored in `self$results$text$setContent()`

3. **Plot Generation**:
   - ggplot2-based plots
   - Saved via `self$results$plot$setState()`

## CI/CD

### GitHub Actions
- **R-CMD-check**: Multi-platform testing (macOS, Windows, Ubuntu)
- **pkgdown**: Auto-deploys documentation to GitHub Pages
- **Skip trigger**: Commits with "WIP" in message skip CI

### Release Process
1. Update version in `DESCRIPTION`
2. Run `devtools::check()` locally
3. Build with `jmvtools::install()`
4. Create GitHub release with `.jmo` file

## Development Guidelines

### Adding New Analysis
1. Create jamovi YAML definitions in `/jamovi/`
2. Generate R files: `jmvtools::install()`
3. Implement analysis logic in `.b.R` file
4. Add roxygen documentation
5. Create tests if applicable
6. Update module menu in `0000.yaml`

### Code Style
- Use tidyverse style guide
- Leverage existing utility functions in `utils.R`
- Include informative error messages via `jmvcore::reject()`
- Add natural language summaries for user interpretation

### Common Pitfalls
- jamovi requires specific R6 class structure - don't modify `.h.R` files
- Person-time calculations are critical for accurate survival estimates
- Always validate input data types and ranges
- Test with missing data scenarios

## Debugging

### Common Issues
- **Module not loading**: Check `0000.yaml` syntax and version compatibility
- **Analysis errors**: Enable debug mode with `options(jmv.debug = TRUE)`
- **Plot issues**: Verify ggplot2 object is properly constructed before setState()

### Useful Debug Commands
```r
# Enable jamovi debug mode
options(jmv.debug = TRUE)

# Test analysis directly
analysis <- jsurvival::survival(
    data = your_data,
    elapsedtime = "time_var",
    outcome = "status_var",
    explanatory = "group_var"
)
analysis$run()
```

## Testing Data
The package includes several test datasets in `/data/`:
- `histopathology.rda`: Example pathology data with survival
- `melanoma.rda`: Melanoma survival dataset
- `stagemigration_*.rda`: Various stage migration test cases

Load test data with: `data("dataset_name", package = "jsurvival")`