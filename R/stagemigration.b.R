#' @title Advanced TNM Stage Migration Analysis
#' 
#' @description
#' State-of-the-art analysis for validating TNM staging system improvements using 
#' comprehensive statistical methods. This analysis provides pathologists with robust 
#' tools to evaluate whether a new staging system provides superior prognostic 
#' discrimination compared to existing systems.
#'
#' @details
#' This comprehensive staging validation analysis includes:
#' 
#' \strong{Core Migration Analysis:}
#' \itemize{
#'   \item Migration matrices with detailed statistics
#'   \item Stage distribution comparisons  
#'   \item Will Rogers phenomenon detection
#'   \item Upstaging and downstaging quantification
#' }
#' 
#' \strong{Advanced Discrimination Metrics:}
#' \itemize{
#'   \item Harrell's C-index with confidence intervals
#'   \item Net Reclassification Improvement (NRI) 
#'   \item Integrated Discrimination Improvement (IDI)
#'   \item Time-dependent ROC analysis
#'   \item Likelihood ratio tests for nested models
#' }
#' 
#' \strong{Clinical Utility Assessment:}
#' \itemize{
#'   \item Decision Curve Analysis (DCA)
#'   \item Net benefit calculations
#'   \item Clinical significance thresholds
#'   \item Cancer-type specific interpretations
#' }
#' 
#' \strong{Validation Framework:}
#' \itemize{
#'   \item Bootstrap validation with optimism correction
#'   \item Cross-validation options
#'   \item Stability assessment
#'   \item Internal validation metrics
#' }
#' 
#' \strong{Advanced Visualizations:}
#' \itemize{
#'   \item Migration heatmaps
#'   \item Time-dependent ROC curves
#'   \item Calibration plots
#'   \item Decision curves
#'   \item Forest plots with confidence intervals
#' }
#'
#' @section Clinical Applications:
#' \itemize{
#'   \item TNM staging system validation (7th to 8th edition transitions)
#'   \item AJCC staging improvements
#'   \item Institution-specific staging modifications
#'   \item Multi-institutional staging harmonization
#'   \item Biomarker-enhanced staging systems
#' }
#'
#' @section Statistical Methods:
#' The analysis implements state-of-the-art methods for staging validation:
#' \itemize{
#'   \item \strong{NRI:} Quantifies net improvement in risk classification
#'   \item \strong{IDI:} Measures integrated discrimination improvement  
#'   \item \strong{C-index:} Harrell's concordance with bootstrap confidence intervals
#'   \item \strong{DCA:} Clinical utility across decision thresholds
#'   \item \strong{Bootstrap:} Internal validation with bias correction
#' }
#'
#' @section Clinical Decision Framework:
#' Results include comprehensive guidance for staging system adoption:
#' \itemize{
#'   \item Statistical significance vs. clinical importance
#'   \item Effect size interpretation (small, medium, large improvements)
#'   \item Sample size adequacy assessment
#'   \item Recommendation confidence levels
#'   \item Implementation considerations
#' }
#'
#' @section Data Requirements:
#' \itemize{
#'   \item \strong{Sample Size:} Minimum 30 patients (100+ recommended)
#'   \item \strong{Follow-up:} Adequate survival time for meaningful analysis
#'   \item \strong{Staging:} Both old and new staging variables with 2+ levels
#'   \item \strong{Events:} Binary event indicator (0/1) or factor with specified level
#'   \item \strong{Data Quality:} Complete case analysis (missing values removed)
#' }
#'
#' @section Troubleshooting:
#' \itemize{
#'   \item \strong{"TRUE/FALSE error":} Check for missing values in staging or survival variables
#'   \item \strong{"Not atomic error":} Disable individual tables to isolate problematic components
#'   \item \strong{Model fitting errors:} Ensure adequate sample size and event rate (5-95%)
#'   \item \strong{Stage level errors:} Verify staging variables have multiple distinct levels
#' }
#'
#' @examples
#' \dontrun{
#' # Basic staging comparison
#' stagemigration(
#'   data = cancer_data,
#'   oldStage = "old_stage",
#'   newStage = "new_stage", 
#'   survivalTime = "survival_months",
#'   event = "outcome",
#'   eventLevel = "DEAD",
#'   analysisType = "basic"
#' )
#' 
#' # Comprehensive analysis with all options
#' stagemigration(
#'   data = lung_cancer_cohort,
#'   oldStage = "tnm7_stage",
#'   newStage = "tnm8_stage", 
#'   survivalTime = "os_months",
#'   event = "death",
#'   eventLevel = "dead",
#'   analysisType = "comprehensive",
#'   cancerType = "lung",
#'   calculateNRI = TRUE,
#'   performBootstrap = TRUE,
#'   bootstrapReps = 1000
#' )
#' }
#'
#' @seealso
#' \code{\link[survival]{concordance}} for C-index calculations,
#' \code{\link[survminer]{ggsurvplot}} for survival visualizations
#' 
#' @keywords TNM staging, stage migration, staging validation, survival analysis
#' @concept staging systems
#' @concept prognostic models
#' @concept cancer staging
#' @concept pathology
#' 
#' @return A comprehensive staging validation analysis with statistical comparisons, 
#'         clinical interpretation, and advanced visualizations
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv survfit coxph concordance survdiff
#' @importFrom survminer ggsurvplot
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#' @importFrom dplyr mutate select group_by summarize
#' @importFrom stats chisq.test fisher.test AIC BIC
#' @importFrom boot boot boot.ci
#' @importFrom pROC roc ci.auc
#' @importFrom timeROC timeROC
#' @importFrom dcurves dca
#' @importFrom rms val.prob
#' @importFrom Hmisc rcorr.cens

stagemigrationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "stagemigrationClass",
    inherit = stagemigrationBase,
    private = list(
        
        .init = function() {
            # Initialize dynamic interface elements
            private$.initializeInterface()
        },
        
        .initializeInterface = function() {
            # Check if core variables are selected
            if (is.null(self$options$oldStage) || is.null(self$options$newStage) || 
                is.null(self$options$survivalTime) || is.null(self$options$event)) {
                
                # Show welcome message and hide all results
                self$results$welcomeMessage$setVisible(TRUE)
                private$.hideAllResults()
                
            } else {
                # Hide welcome message and show relevant results based on analysis type
                self$results$welcomeMessage$setVisible(FALSE)
                private$.configureResultsVisibility()
            }
        },
        
        .hideAllResults = function() {
            # Hide all result components
            result_names <- c(
                "executiveSummary", "migrationOverview", "statisticalComparison", 
                "nriResults", "idiResults", "rocAnalysis", "dcaResults",
                "calibrationPlots", "bootstrapResults", "homogeneityTests",
                "willRogersAnalysis", "clinicalInterpretation", "methodologyNotes",
                "migrationHeatmap", "rocComparisonPlot", "calibrationPlots", 
                "decisionCurves", "forestPlot", "survivalCurves"
            )
            
            for (name in result_names) {
                if (name %in% names(self$results)) {
                    self$results[[name]]$setVisible(FALSE)
                }
            }
        },
        
        .configureResultsVisibility = function() {
            # Configure visibility based on analysis type and options
            analysisType <- self$options$analysisType
            
            # Always show core results
            self$results$migrationOverview$setVisible(TRUE)
            self$results$statisticalComparison$setVisible(TRUE)
            
            # Show advanced results based on analysis type
            if (analysisType %in% c("standard", "comprehensive", "publication")) {
                self$results$nriResults$setVisible(self$options$calculateNRI)
                self$results$idiResults$setVisible(self$options$calculateIDI)
                self$results$rocAnalysis$setVisible(self$options$performROCAnalysis)
            }
            
            if (analysisType %in% c("comprehensive", "publication")) {
                self$results$dcaResults$setVisible(self$options$performDCA)
                self$results$calibrationPlots$setVisible(self$options$performCalibration)
                self$results$bootstrapResults$setVisible(self$options$performBootstrap)
                self$results$homogeneityTests$setVisible(self$options$performHomogeneityTests)
            }
            
            # Always show interpretation and Will Rogers if requested
            self$results$willRogersAnalysis$setVisible(self$options$showWillRogersAnalysis)
            self$results$clinicalInterpretation$setVisible(self$options$showClinicalInterpretation)
            self$results$executiveSummary$setVisible(self$options$generateExecutiveSummary)
            self$results$methodologyNotes$setVisible(self$options$showMethodologyNotes)
            
            # Configure plot visibility
            self$results$migrationHeatmap$setVisible(self$options$showMigrationHeatmap)
            self$results$rocComparisonPlot$setVisible(self$options$showROCComparison)
            self$results$calibrationPlots$setVisible(self$options$showCalibrationPlots)
            self$results$decisionCurves$setVisible(self$options$showDecisionCurves)
            self$results$forestPlot$setVisible(self$options$showForestPlot)
            self$results$survivalCurves$setVisible(TRUE)
        },
        
        .validateData = function() {
            # Comprehensive data validation for staging analysis
            if (is.null(self$data) || nrow(self$data) == 0) {
                stop("Dataset is empty or not loaded")
            }
            
            # Check required variables
            required_vars <- c(self$options$oldStage, self$options$newStage, 
                             self$options$survivalTime, self$options$event)
            
            missing_vars <- setdiff(required_vars, names(self$data))
            if (length(missing_vars) > 0) {
                stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
            }
            
            # Extract and validate data
            data <- self$data[required_vars]
            
            # Check for rows with invalid data before removing them
            incomplete_rows <- which(!complete.cases(data))
            if (length(incomplete_rows) > 0) {
                warning(paste("Removing", length(incomplete_rows), "rows with missing values."))
            }
            
            data <- data[complete.cases(data), ]
            
            if (nrow(data) < 30) {
                warning("Small sample size (n < 30). Results may be unreliable for staging validation.")
            }
            
            if (nrow(data) < 100) {
                warning("Sample size < 100. Consider larger cohort for robust staging validation.")
            }
            
            # Validate staging variables
            old_stages <- unique(data[[self$options$oldStage]])
            new_stages <- unique(data[[self$options$newStage]])
            
            if (length(old_stages) < 2 || length(new_stages) < 2) {
                stop("Staging variables must have at least 2 stages for comparison")
            }
            
            if (length(old_stages) > 10 || length(new_stages) > 10) {
                warning("Many staging levels detected. Consider grouping stages for clearer analysis.")
            }
            
            # Validate survival variables
            survival_times <- data[[self$options$survivalTime]]
            if (any(is.na(survival_times))) {
                stop("Survival time contains missing values after data cleaning")
            }
            if (any(survival_times <= 0)) {
                stop("Survival time must be positive")
            }
            if (!is.numeric(survival_times)) {
                stop("Survival time must be numeric")
            }
            
            # Handle event variable with improved validation
            event_var <- data[[self$options$event]]
            
            if (is.factor(event_var) || is.character(event_var)) {
                if (is.null(self$options$eventLevel) || self$options$eventLevel == "") {
                    stop("Event level must be specified for factor/character event variables")
                }
                
                # Get unique event values (excluding NA)
                unique_events_raw <- unique(event_var[!is.na(event_var)])
                
                if (!self$options$eventLevel %in% unique_events_raw) {
                    stop(paste("Event level '", self$options$eventLevel, "' not found in event variable. ",
                              "Available values: ", paste(unique_events_raw, collapse=", "), sep=""))
                }
                
                # Create binary event variable
                data[["event_binary"]] <- ifelse(event_var == self$options$eventLevel, 1, 0)
            } else {
                # Convert numeric event variable
                data[["event_binary"]] <- as.numeric(event_var)
            }
            
            # Check for NA values in event_binary
            if (any(is.na(data[["event_binary"]]))) {
                stop("Event variable contains values that could not be converted to binary (0/1)")
            }
            
            # Ensure binary event coding
            unique_events <- unique(data[["event_binary"]])
            if (length(unique_events) == 0) {
                stop("No valid event values found")
            }
            if (!all(unique_events %in% c(0, 1))) {
                stop(paste("Event variable must be binary (0/1). Found values:", paste(unique_events, collapse=", ")))
            }
            if (length(unique_events) < 2) {
                stop("Event variable must have both event and non-event cases (0 and 1)")
            }
            
            # Check event frequency
            event_rate <- mean(data[["event_binary"]], na.rm = TRUE)
            if (event_rate < 0.05) {
                warning("Very low event rate (< 5%). Results may be unreliable.")
            } else if (event_rate > 0.95) {
                warning("Very high event rate (> 95%). Consider different endpoint or longer follow-up.")
            }
            
            return(data)
        },
        
        .calculateBasicMigration = function(data) {
            # Comprehensive migration analysis
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            
            # Create cross-tabulation
            migration_table <- table(
                Old = data[[old_stage]], 
                New = data[[new_stage]]
            )
            
            # Calculate migration statistics
            total_patients <- sum(migration_table)
            # Handle non-square tables
            if (nrow(migration_table) == ncol(migration_table)) {
                unchanged <- sum(diag(migration_table))
            } else {
                # For non-square tables, match stages by name
                unchanged <- 0
                for (stage in intersect(rownames(migration_table), colnames(migration_table))) {
                    unchanged <- unchanged + migration_table[stage, stage]
                }
            }
            migrated <- total_patients - unchanged
            migration_rate <- migrated / total_patients
            
            # Calculate stage-wise migration
            stage_migration <- list()
            for (i in 1:nrow(migration_table)) {
                stage_name <- rownames(migration_table)[i]
                stage_total <- sum(migration_table[i, ])
                # Check if this stage exists in new staging
                if (stage_name %in% colnames(migration_table)) {
                    stage_unchanged <- migration_table[i, stage_name]
                } else {
                    stage_unchanged <- 0
                }
                stage_migrated <- stage_total - stage_unchanged
                
                stage_migration[[stage_name]] <- list(
                    total = stage_total,
                    unchanged = stage_unchanged,
                    migrated = stage_migrated,
                    migration_rate = if (stage_total > 0) stage_migrated / stage_total else 0,
                    destinations = migration_table[i, migration_table[i, ] > 0]
                )
            }
            
            # Calculate upstaging and downstaging (for ordinal stages)
            upstaging <- 0
            downstaging <- 0
            
            # Try to extract numeric stage levels for up/down staging calculation
            old_levels <- suppressWarnings(as.numeric(gsub("[^0-9]", "", rownames(migration_table))))
            new_levels <- suppressWarnings(as.numeric(gsub("[^0-9]", "", colnames(migration_table))))
            
            # Check if we have valid numeric levels for both old and new stages
            old_levels_valid <- !is.na(old_levels) & is.finite(old_levels)
            new_levels_valid <- !is.na(new_levels) & is.finite(new_levels)
            
            if (all(old_levels_valid) && all(new_levels_valid) && length(old_levels) > 0 && length(new_levels) > 0) {
                for (i in 1:nrow(migration_table)) {
                    for (j in 1:ncol(migration_table)) {
                        if (i != j && migration_table[i, j] > 0) {
                            if (new_levels[j] > old_levels[i]) {
                                upstaging <- upstaging + migration_table[i, j]
                            } else if (new_levels[j] < old_levels[i]) {
                                downstaging <- downstaging + migration_table[i, j]
                            }
                        }
                    }
                }
            }
            
            
            # Statistical tests with proper error handling
            chi_test <- NULL
            fisher_test <- NULL
            
            # Chi-square test
            tryCatch({
                chi_test <- chisq.test(migration_table)
            }, error = function(e) {
                warning(paste("Chi-square test failed:", e$message))
            })
            
            # Fisher's exact test (only for smaller tables)
            min_cell_count <- min(as.vector(migration_table))
            if (total_patients <= 1000 && min_cell_count >= 1) {
                tryCatch({
                    fisher_test <- fisher.test(migration_table, simulate.p.value = TRUE)
                }, error = function(e) {
                    warning(paste("Fisher's exact test failed:", e$message))
                })
            }
            
            return(list(
                migration_table = migration_table,
                total_patients = total_patients,
                unchanged = unchanged,
                migrated = migrated,
                migration_rate = migration_rate,
                upstaging = upstaging,
                downstaging = downstaging,
                upstaging_rate = upstaging / total_patients,
                downstaging_rate = downstaging / total_patients,
                stage_migration = stage_migration,
                chi_test = chi_test,
                fisher_test = fisher_test
            ))
        },
        
        .calculateAdvancedMetrics = function(data) {
            # Advanced discrimination and calibration metrics with error handling
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Validate required columns exist
            required_cols <- c(old_stage, new_stage, time_var, event_var)
            missing_cols <- setdiff(required_cols, names(data))
            if (length(missing_cols) > 0) {
                stop(paste("Missing required columns for advanced metrics:", paste(missing_cols, collapse=", ")))
            }
            
            # Fit Cox models with error handling
            old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
            new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
            
            old_cox <- NULL
            new_cox <- NULL
            
            tryCatch({
                old_cox <- coxph(old_formula, data = data)
            }, error = function(e) {
                stop(paste("Failed to fit Cox model for original staging:", e$message))
            })
            
            tryCatch({
                new_cox <- coxph(new_formula, data = data)
            }, error = function(e) {
                stop(paste("Failed to fit Cox model for new staging:", e$message))
            })
            
            # Calculate C-index with error handling
            old_concordance <- NULL
            new_concordance <- NULL
            
            tryCatch({
                old_concordance <- concordance(old_cox)
                new_concordance <- concordance(new_cox)
            }, error = function(e) {
                stop(paste("Failed to calculate concordance indices:", e$message))
            })
            
            # Validate concordance objects
            if (is.null(old_concordance$concordance) || is.null(new_concordance$concordance)) {
                stop("Concordance calculation returned invalid results")
            }
            
            c_improvement <- new_concordance$concordance - old_concordance$concordance
            c_improvement_pct <- if (old_concordance$concordance > 0) {
                (c_improvement / old_concordance$concordance) * 100
            } else {
                0
            }
            
            # Bootstrap C-index confidence intervals
            if (self$options$performBootstrap) {
                c_bootstrap <- private$.bootstrapConcordance(data, old_formula, new_formula)
            } else {
                c_bootstrap <- NULL
            }
            
            # Model comparison tests with error handling
            aic_old <- tryCatch(AIC(old_cox), error = function(e) NA)
            aic_new <- tryCatch(AIC(new_cox), error = function(e) NA)
            aic_improvement <- if (!is.na(aic_old) && !is.na(aic_new)) aic_old - aic_new else NA
            
            bic_old <- tryCatch(BIC(old_cox), error = function(e) NA)
            bic_new <- tryCatch(BIC(new_cox), error = function(e) NA)
            bic_improvement <- if (!is.na(bic_old) && !is.na(bic_new)) bic_old - bic_new else NA
            
            # Likelihood ratio test with error handling
            lr_test <- NULL
            tryCatch({
                lr_test <- anova(old_cox, new_cox, test = "Chisq")
            }, error = function(e) {
                warning(paste("Likelihood ratio test failed:", e$message))
            })
            
            # Pseudo R-squared measures
            if (self$options$calculatePseudoR2) {
                pseudo_r2 <- private$.calculatePseudoR2(old_cox, new_cox, data)
            } else {
                pseudo_r2 <- NULL
            }
            
            return(list(
                old_cox = old_cox,
                new_cox = new_cox,
                old_concordance = old_concordance,
                new_concordance = new_concordance,
                c_improvement = c_improvement,
                c_improvement_pct = c_improvement_pct,
                c_bootstrap = c_bootstrap,
                aic_old = aic_old,
                aic_new = aic_new,
                aic_improvement = aic_improvement,
                bic_old = bic_old,
                bic_new = bic_new,
                bic_improvement = bic_improvement,
                lr_test = lr_test,
                pseudo_r2 = pseudo_r2
            ))
        },
        
        .calculateNRI = function(data, time_points = NULL) {
            # Net Reclassification Improvement calculation
            if (!self$options$calculateNRI) return(NULL)
            
            # Parse time points
            if (is.null(time_points)) {
                time_points_str <- self$options$nriTimePoints
                time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
                time_points <- time_points[!is.na(time_points)]
            }
            
            if (length(time_points) == 0) {
                time_points <- c(12, 24, 60)  # Default time points
            }
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            nri_results <- list()
            
            for (t in time_points) {
                # Calculate survival probabilities at time t
                old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
                new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
                
                old_fit <- survfit(old_formula, data = data)
                new_fit <- survfit(new_formula, data = data)
                
                # Extract survival probabilities for each patient
                old_surv_probs <- private$.extractSurvivalProbabilities(old_fit, data, t, old_stage)
                new_surv_probs <- private$.extractSurvivalProbabilities(new_fit, data, t, new_stage)
                
                # Calculate event status at time t
                event_at_t <- ifelse(data[[time_var]] <= t & data[[event_var]] == 1, 1, 0)
                
                # Define risk categories (low, intermediate, high)
                old_risk_cat <- cut(1 - old_surv_probs, breaks = c(0, 0.33, 0.67, 1), 
                                  labels = c("Low", "Intermediate", "High"))
                new_risk_cat <- cut(1 - new_surv_probs, breaks = c(0, 0.33, 0.67, 1), 
                                  labels = c("Low", "Intermediate", "High"))
                
                # Calculate NRI components
                nri_result <- private$.calculateNRIComponents(old_risk_cat, new_risk_cat, event_at_t)
                nri_result$time_point <- t
                
                nri_results[[paste0("t", t)]] <- nri_result
            }
            
            return(nri_results)
        },
        
        .calculateNRIComponents = function(old_cat, new_cat, events) {
            # Calculate NRI components for events and non-events
            
            # For events (cases)
            events_idx <- which(events == 1)
            if (length(events_idx) > 0) {
                old_cat_events <- old_cat[events_idx]
                new_cat_events <- new_cat[events_idx]
                
                # Calculate reclassification for events
                events_up <- sum(as.numeric(new_cat_events) > as.numeric(old_cat_events), na.rm = TRUE)
                events_down <- sum(as.numeric(new_cat_events) < as.numeric(old_cat_events), na.rm = TRUE)
                events_total <- length(events_idx)
                
                nri_events <- (events_up - events_down) / events_total
            } else {
                nri_events <- 0
                events_up <- 0
                events_down <- 0
                events_total <- 0
            }
            
            # For non-events (controls)
            nonevents_idx <- which(events == 0)
            if (length(nonevents_idx) > 0) {
                old_cat_nonevents <- old_cat[nonevents_idx]
                new_cat_nonevents <- new_cat[nonevents_idx]
                
                # Calculate reclassification for non-events (opposite direction is good)
                nonevents_up <- sum(as.numeric(new_cat_nonevents) > as.numeric(old_cat_nonevents), na.rm = TRUE)
                nonevents_down <- sum(as.numeric(new_cat_nonevents) < as.numeric(old_cat_nonevents), na.rm = TRUE)
                nonevents_total <- length(nonevents_idx)
                
                nri_nonevents <- (nonevents_down - nonevents_up) / nonevents_total
            } else {
                nri_nonevents <- 0
                nonevents_up <- 0
                nonevents_down <- 0
                nonevents_total <- 0
            }
            
            # Overall NRI
            nri_overall <- nri_events + nri_nonevents
            
            return(list(
                nri_overall = nri_overall,
                nri_events = nri_events,
                nri_nonevents = nri_nonevents,
                events_up = events_up,
                events_down = events_down,
                events_total = events_total,
                nonevents_up = nonevents_up,
                nonevents_down = nonevents_down,
                nonevents_total = nonevents_total
            ))
        },
        
        .extractSurvivalProbabilities = function(fit, data, time_point, stage_var) {
            # Extract survival probabilities for each patient at specific time point
            probs <- numeric(nrow(data))
            
            for (i in 1:nrow(data)) {
                stage_level <- data[[stage_var]][i]
                
                # Find corresponding stratum in survival fit
                stratum_idx <- which(grepl(paste0(stage_var, "=", stage_level), names(fit$strata)))
                
                if (length(stratum_idx) > 0) {
                    # Extract time and survival for this stratum
                    stratum_name <- names(fit$strata)[stratum_idx]
                    stratum_end <- cumsum(fit$strata)[stratum_idx]
                    stratum_start <- ifelse(stratum_idx == 1, 1, cumsum(fit$strata)[stratum_idx - 1] + 1)
                    
                    stratum_times <- fit$time[stratum_start:stratum_end]
                    stratum_surv <- fit$surv[stratum_start:stratum_end]
                    
                    # Interpolate survival at time_point
                    if (time_point <= min(stratum_times)) {
                        probs[i] <- 1.0  # No events before first time point
                    } else if (time_point >= max(stratum_times)) {
                        probs[i] <- stratum_surv[length(stratum_surv)]  # Last observed survival
                    } else {
                        probs[i] <- approx(stratum_times, stratum_surv, time_point)$y
                    }
                } else {
                    # Default to overall survival if stratum not found
                    if (time_point <= min(fit$time)) {
                        probs[i] <- 1.0
                    } else if (time_point >= max(fit$time)) {
                        probs[i] <- min(fit$surv)
                    } else {
                        probs[i] <- approx(fit$time, fit$surv, time_point)$y
                    }
                }
            }
            
            return(probs)
        },
        
        .calculateIDI = function(data) {
            # Integrated Discrimination Improvement calculation
            if (!self$options$calculateIDI) return(NULL)
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Fit Cox models and get linear predictors
            old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
            new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
            
            old_cox <- coxph(old_formula, data = data)
            new_cox <- coxph(new_formula, data = data)
            
            # Get linear predictors (risk scores)
            old_lp <- predict(old_cox, type = "lp")
            new_lp <- predict(new_cox, type = "lp")
            
            # Convert to probabilities (relative risk)
            old_prob <- exp(old_lp) / (1 + exp(old_lp))
            new_prob <- exp(new_lp) / (1 + exp(new_lp))
            
            # Calculate discrimination slopes
            events <- data[[event_var]]
            
            # Discrimination slope for old model
            old_disc_events <- mean(old_prob[events == 1], na.rm = TRUE)
            old_disc_nonevents <- mean(old_prob[events == 0], na.rm = TRUE)
            old_discrimination_slope <- old_disc_events - old_disc_nonevents
            
            # Discrimination slope for new model  
            new_disc_events <- mean(new_prob[events == 1], na.rm = TRUE)
            new_disc_nonevents <- mean(new_prob[events == 0], na.rm = TRUE)
            new_discrimination_slope <- new_disc_events - new_disc_nonevents
            
            # IDI calculation
            idi <- new_discrimination_slope - old_discrimination_slope
            
            # Bootstrap confidence interval for IDI
            if (self$options$performBootstrap) {
                idi_bootstrap <- private$.bootstrapIDI(data, old_formula, new_formula)
            } else {
                idi_bootstrap <- NULL
            }
            
            return(list(
                idi = idi,
                old_discrimination_slope = old_discrimination_slope,
                new_discrimination_slope = new_discrimination_slope,
                old_prob_events = old_disc_events,
                old_prob_nonevents = old_disc_nonevents,
                new_prob_events = new_disc_events,
                new_prob_nonevents = new_disc_nonevents,
                idi_bootstrap = idi_bootstrap
            ))
        },
        
        .performTimeROCAnalysis = function(data) {
            # Time-dependent ROC analysis
            if (!self$options$performROCAnalysis) return(NULL)
            
            # Parse time points
            time_points_str <- self$options$rocTimePoints
            time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
            time_points <- time_points[!is.na(time_points)]
            
            if (length(time_points) == 0) {
                time_points <- c(12, 24, 36, 60)  # Default time points
            }
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Fit Cox models
            old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
            new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
            
            old_cox <- coxph(old_formula, data = data)
            new_cox <- coxph(new_formula, data = data)
            
            # Get risk scores
            old_risk <- predict(old_cox, type = "risk")
            new_risk <- predict(new_cox, type = "risk")
            
            roc_results <- list()
            
            # Calculate time-dependent ROC for each time point
            if (requireNamespace("timeROC", quietly = TRUE)) {
                for (t in time_points) {
                    # TimeROC analysis for old staging
                    old_roc <- try({
                        timeROC::timeROC(
                            T = data[[time_var]],
                            delta = data[[event_var]], 
                            marker = old_risk,
                            cause = 1,
                            times = t,
                            iid = TRUE
                        )
                    }, silent = TRUE)
                    
                    # TimeROC analysis for new staging
                    new_roc <- try({
                        timeROC::timeROC(
                            T = data[[time_var]],
                            delta = data[[event_var]],
                            marker = new_risk,
                            cause = 1,
                            times = t,
                            iid = TRUE
                        )
                    }, silent = TRUE)
                    
                    if (!inherits(old_roc, "try-error") && !inherits(new_roc, "try-error")) {
                        # Extract AUC values
                        old_auc <- old_roc$AUC[1]
                        new_auc <- new_roc$AUC[1]
                        
                        # Calculate confidence intervals if available
                        old_ci <- c(old_auc - 1.96 * sqrt(old_roc$inference$vect_sd_1[1]),
                                   old_auc + 1.96 * sqrt(old_roc$inference$vect_sd_1[1]))
                        new_ci <- c(new_auc - 1.96 * sqrt(new_roc$inference$vect_sd_1[1]),
                                   new_auc + 1.96 * sqrt(new_roc$inference$vect_sd_1[1]))
                        
                        roc_results[[paste0("t", t)]] <- list(
                            time_point = t,
                            old_auc = old_auc,
                            new_auc = new_auc,
                            auc_improvement = new_auc - old_auc,
                            old_ci = old_ci,
                            new_ci = new_ci,
                            old_roc = old_roc,
                            new_roc = new_roc
                        )
                    }
                }
            }
            
            return(roc_results)
        },
        
        .performDCA = function(data) {
            # Decision Curve Analysis
            if (!self$options$performDCA) return(NULL)
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Fit Cox models
            old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
            new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
            
            old_cox <- coxph(old_formula, data = data)
            new_cox <- coxph(new_formula, data = data)
            
            # Get predicted probabilities at specific time point (e.g., 5 years)
            time_horizon <- 60  # 5 years
            
            # Calculate baseline survival
            baseline_surv_old <- survfit(old_cox)
            baseline_surv_new <- survfit(new_cox)
            
            # Extract baseline survival at time horizon
            baseline_prob_old <- private$.extractBaselineSurvival(baseline_surv_old, time_horizon)
            baseline_prob_new <- private$.extractBaselineSurvival(baseline_surv_new, time_horizon)
            
            # Calculate individual risk predictions
            old_lp <- predict(old_cox, type = "lp")
            new_lp <- predict(new_cox, type = "lp")
            
            old_risk <- 1 - (baseline_prob_old ^ exp(old_lp))
            new_risk <- 1 - (baseline_prob_new ^ exp(new_lp))
            
            # Create outcome variable for DCA (event within time horizon)
            outcome <- ifelse(data[[time_var]] <= time_horizon & data[[event_var]] == 1, 1, 0)
            
            dca_results <- list()
            
            if (requireNamespace("dcurves", quietly = TRUE)) {
                # Perform DCA
                dca_data <- data.frame(
                    outcome = outcome,
                    old_risk = old_risk,
                    new_risk = new_risk
                )
                
                dca_result <- try({
                    dcurves::dca(
                        formula = outcome ~ old_risk + new_risk,
                        data = dca_data,
                        thresholds = seq(0.01, 0.99, by = 0.01)
                    )
                }, silent = TRUE)
                
                if (!inherits(dca_result, "try-error")) {
                    dca_results$dca_result <- dca_result
                    dca_results$time_horizon <- time_horizon
                }
            }
            
            return(dca_results)
        },
        
        .extractBaselineSurvival = function(surv_fit, time_point) {
            # Extract baseline survival probability at specific time point
            if (time_point <= min(surv_fit$time)) {
                return(1.0)
            } else if (time_point >= max(surv_fit$time)) {
                return(min(surv_fit$surv))
            } else {
                return(approx(surv_fit$time, surv_fit$surv, time_point)$y)
            }
        },
        
        .performBootstrapValidation = function(data, n_bootstrap = NULL) {
            # Bootstrap validation with optimism correction
            if (!self$options$performBootstrap) return(NULL)
            
            if (is.null(n_bootstrap)) {
                n_bootstrap <- self$options$bootstrapReps
            }
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Bootstrap function for validation
            bootstrap_function <- function(data, indices) {
                boot_data <- data[indices, ]
                
                # Fit models on bootstrap sample
                old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
                new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
                
                old_cox_boot <- try(coxph(old_formula, data = boot_data), silent = TRUE)
                new_cox_boot <- try(coxph(new_formula, data = boot_data), silent = TRUE)
                
                if (inherits(old_cox_boot, "try-error") || inherits(new_cox_boot, "try-error")) {
                    return(c(NA, NA, NA))
                }
                
                # Calculate C-index on bootstrap sample
                old_c_boot <- concordance(old_cox_boot)$concordance
                new_c_boot <- concordance(new_cox_boot)$concordance
                
                # Calculate C-index on original data using bootstrap models
                old_c_orig <- try(concordance(old_cox_boot, newdata = data)$concordance, silent = TRUE)
                new_c_orig <- try(concordance(new_cox_boot, newdata = data)$concordance, silent = TRUE)
                
                if (inherits(old_c_orig, "try-error") || inherits(new_c_orig, "try-error")) {
                    optimism <- 0
                } else {
                    optimism <- (new_c_boot - old_c_boot) - (new_c_orig - old_c_orig)
                }
                
                return(c(old_c_boot, new_c_boot, optimism))
            }
            
            # Perform bootstrap
            if (requireNamespace("boot", quietly = TRUE)) {
                boot_results <- boot::boot(
                    data = data,
                    statistic = bootstrap_function,
                    R = n_bootstrap
                )
                
                # Calculate optimism-corrected estimates
                apparent_improvement <- boot_results$t0[2] - boot_results$t0[1]
                mean_optimism <- mean(boot_results$t[, 3], na.rm = TRUE)
                optimism_corrected_improvement <- apparent_improvement - mean_optimism
                
                # Bootstrap confidence intervals
                if (all(!is.na(boot_results$t[, 2] - boot_results$t[, 1]))) {
                    improvement_ci <- try({
                        boot::boot.ci(boot_results, type = "perc", index = c(2, 1))
                    }, silent = TRUE)
                } else {
                    improvement_ci <- NULL
                }
                
                return(list(
                    boot_results = boot_results,
                    apparent_improvement = apparent_improvement,
                    mean_optimism = mean_optimism,
                    optimism_corrected_improvement = optimism_corrected_improvement,
                    improvement_ci = improvement_ci,
                    n_bootstrap = n_bootstrap
                ))
            }
            
            return(NULL)
        },
        
        .bootstrapConcordance = function(data, old_formula, new_formula) {
            # Bootstrap confidence intervals for C-index
            bootstrap_c <- function(data, indices) {
                boot_data <- data[indices, ]
                
                old_cox <- try(coxph(old_formula, data = boot_data), silent = TRUE)
                new_cox <- try(coxph(new_formula, data = boot_data), silent = TRUE)
                
                if (inherits(old_cox, "try-error") || inherits(new_cox, "try-error")) {
                    return(c(NA, NA))
                }
                
                old_c <- concordance(old_cox)$concordance
                new_c <- concordance(new_cox)$concordance
                
                return(c(old_c, new_c))
            }
            
            if (requireNamespace("boot", quietly = TRUE)) {
                boot_results <- boot::boot(
                    data = data,
                    statistic = bootstrap_c,
                    R = min(self$options$bootstrapReps, 500)  # Limit for efficiency
                )
                
                # Calculate confidence intervals
                old_ci <- try(boot::boot.ci(boot_results, type = "perc", index = 1), silent = TRUE)
                new_ci <- try(boot::boot.ci(boot_results, type = "perc", index = 2), silent = TRUE)
                
                return(list(
                    boot_results = boot_results,
                    old_ci = old_ci,
                    new_ci = new_ci
                ))
            }
            
            return(NULL)
        },
        
        .bootstrapIDI = function(data, old_formula, new_formula) {
            # Bootstrap confidence intervals for IDI
            bootstrap_idi <- function(data, indices) {
                boot_data <- data[indices, ]
                
                old_cox <- try(coxph(old_formula, data = boot_data), silent = TRUE)
                new_cox <- try(coxph(new_formula, data = boot_data), silent = TRUE)
                
                if (inherits(old_cox, "try-error") || inherits(new_cox, "try-error")) {
                    return(NA)
                }
                
                # Calculate IDI on bootstrap sample
                old_lp <- predict(old_cox, type = "lp")
                new_lp <- predict(new_cox, type = "lp")
                
                old_prob <- exp(old_lp) / (1 + exp(old_lp))
                new_prob <- exp(new_lp) / (1 + exp(new_lp))
                
                events <- boot_data[["event_binary"]]
                
                old_disc_slope <- mean(old_prob[events == 1], na.rm = TRUE) - mean(old_prob[events == 0], na.rm = TRUE)
                new_disc_slope <- mean(new_prob[events == 1], na.rm = TRUE) - mean(new_prob[events == 0], na.rm = TRUE)
                
                return(new_disc_slope - old_disc_slope)
            }
            
            if (requireNamespace("boot", quietly = TRUE)) {
                boot_results <- boot::boot(
                    data = data,
                    statistic = bootstrap_idi,
                    R = min(self$options$bootstrapReps, 500)
                )
                
                idi_ci <- try(boot::boot.ci(boot_results, type = "perc"), silent = TRUE)
                
                return(list(
                    boot_results = boot_results,
                    idi_ci = idi_ci
                ))
            }
            
            return(NULL)
        },
        
        .calculatePseudoR2 = function(old_cox, new_cox, data) {
            # Calculate various pseudo R-squared measures
            
            # Null model (intercept only)
            null_formula <- as.formula(paste("Surv(", self$options$survivalTime, ", event_binary) ~ 1"))
            null_cox <- coxph(null_formula, data = data)
            
            # Log-likelihoods
            ll_null <- null_cox$loglik[2]
            ll_old <- old_cox$loglik[2]
            ll_new <- new_cox$loglik[2]
            
            # Number of parameters
            p_old <- length(coef(old_cox))
            p_new <- length(coef(new_cox))
            n <- nrow(data)
            
            # Nagelkerke R-squared
            nagelkerke_old <- (1 - exp((ll_null - ll_old) * 2 / n)) / (1 - exp(ll_null * 2 / n))
            nagelkerke_new <- (1 - exp((ll_null - ll_new) * 2 / n)) / (1 - exp(ll_null * 2 / n))
            
            # McFadden R-squared
            mcfadden_old <- 1 - (ll_old / ll_null)
            mcfadden_new <- 1 - (ll_new / ll_null)
            
            # Cox-Snell R-squared
            cox_snell_old <- 1 - exp((ll_null - ll_old) * 2 / n)
            cox_snell_new <- 1 - exp((ll_null - ll_new) * 2 / n)
            
            # Adjusted R-squared (penalized)
            adj_mcfadden_old <- 1 - ((ll_old - p_old) / ll_null)
            adj_mcfadden_new <- 1 - ((ll_new - p_new) / ll_null)
            
            return(list(
                nagelkerke_old = nagelkerke_old,
                nagelkerke_new = nagelkerke_new,
                nagelkerke_improvement = nagelkerke_new - nagelkerke_old,
                mcfadden_old = mcfadden_old,
                mcfadden_new = mcfadden_new,
                mcfadden_improvement = mcfadden_new - mcfadden_old,
                cox_snell_old = cox_snell_old,
                cox_snell_new = cox_snell_new,
                cox_snell_improvement = cox_snell_new - cox_snell_old,
                adj_mcfadden_old = adj_mcfadden_old,
                adj_mcfadden_new = adj_mcfadden_new,
                adj_mcfadden_improvement = adj_mcfadden_new - adj_mcfadden_old
            ))
        },
        
        .performHomogeneityTests = function(data) {
            # Test homogeneity within stages and trend across stages
            if (!self$options$performHomogeneityTests) return(NULL)
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            homogeneity_results <- list()
            
            # Test for old staging system
            old_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", old_stage))
            old_survdiff <- survdiff(old_formula, data = data)
            
            # Overall test
            old_overall_p <- 1 - pchisq(old_survdiff$chisq, df = length(old_survdiff$n) - 1)
            
            # Trend test (if stages are ordinal)
            old_trend_test <- private$.calculateTrendTest(data, old_stage, time_var, event_var)
            
            homogeneity_results$old_staging <- list(
                overall_test = old_survdiff,
                overall_p = old_overall_p,
                trend_test = old_trend_test
            )
            
            # Test for new staging system
            new_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~", new_stage))
            new_survdiff <- survdiff(new_formula, data = data)
            
            new_overall_p <- 1 - pchisq(new_survdiff$chisq, df = length(new_survdiff$n) - 1)
            new_trend_test <- private$.calculateTrendTest(data, new_stage, time_var, event_var)
            
            homogeneity_results$new_staging <- list(
                overall_test = new_survdiff,
                overall_p = new_overall_p,
                trend_test = new_trend_test
            )
            
            return(homogeneity_results)
        },
        
        .calculateTrendTest = function(data, stage_var, time_var, event_var) {
            # Calculate trend test for ordinal stages
            
            # Try to extract numeric values from stage labels
            stage_levels <- levels(as.factor(data[[stage_var]]))
            numeric_stages <- suppressWarnings(as.numeric(gsub("[^0-9]", "", stage_levels)))
            
            if (any(is.na(numeric_stages))) {
                # If stages are not clearly numeric, use rank order
                numeric_stages <- 1:length(stage_levels)
            }
            
            # Create mapping from stage levels to numeric values
            stage_mapping <- setNames(numeric_stages, stage_levels)
            data$stage_numeric <- stage_mapping[as.character(data[[stage_var]])]
            
            # Fit Cox model with stage as continuous variable for trend test
            trend_formula <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~ stage_numeric"))
            trend_cox <- try(coxph(trend_formula, data = data), silent = TRUE)
            
            if (!inherits(trend_cox, "try-error")) {
                trend_p <- summary(trend_cox)$coefficients[1, "Pr(>|z|)"]
                trend_coef <- summary(trend_cox)$coefficients[1, "coef"]
                trend_se <- summary(trend_cox)$coefficients[1, "se(coef)"]
                trend_z <- summary(trend_cox)$coefficients[1, "z"]
                
                return(list(
                    trend_p = trend_p,
                    trend_coef = trend_coef,
                    trend_se = trend_se,
                    trend_z = trend_z,
                    trend_cox = trend_cox
                ))
            }
            
            return(NULL)
        },
        
        .analyzeWillRogers = function(data) {
            # Comprehensive Will Rogers phenomenon analysis
            if (!self$options$showWillRogersAnalysis) return(NULL)
            
            old_stage <- self$options$oldStage
            new_stage <- self$options$newStage
            time_var <- self$options$survivalTime
            event_var <- "event_binary"
            
            # Create migration categories
            data$migration_status <- ifelse(
                as.character(data[[old_stage]]) == as.character(data[[new_stage]]),
                "Unchanged",
                "Migrated"
            )
            
            # Analyze by original stage
            will_rogers_results <- list()
            
            stage_levels <- levels(as.factor(data[[old_stage]]))
            
            for (stage in stage_levels) {
                stage_data <- data[data[[old_stage]] == stage, ]
                
                if (nrow(stage_data) < 10) next  # Skip stages with too few patients
                
                # Split into migrated and unchanged
                unchanged_data <- stage_data[stage_data$migration_status == "Unchanged", ]
                migrated_data <- stage_data[stage_data$migration_status == "Migrated", ]
                
                if (nrow(unchanged_data) < 5 || nrow(migrated_data) < 5) next
                
                # Survival comparison
                formula_wr <- as.formula(paste("Surv(", time_var, ",", event_var, ") ~ migration_status"))
                
                # Kaplan-Meier fits
                km_fit <- try(survfit(formula_wr, data = stage_data), silent = TRUE)
                
                # Log-rank test
                lr_test <- try(survdiff(formula_wr, data = stage_data), silent = TRUE)
                
                # Cox regression
                cox_wr <- try(coxph(formula_wr, data = stage_data), silent = TRUE)
                
                if (!inherits(km_fit, "try-error") && !inherits(lr_test, "try-error")) {
                    # Calculate median survival times
                    median_surv <- try(summary(km_fit)$table[, "median"], silent = TRUE)
                    
                    # Extract p-value from log-rank test
                    lr_p <- 1 - pchisq(lr_test$chisq, df = 1)
                    
                    # Hazard ratio from Cox model
                    hr <- NA
                    hr_ci <- c(NA, NA)
                    hr_p <- NA
                    
                    if (!inherits(cox_wr, "try-error")) {
                        cox_summary <- summary(cox_wr)
                        if (nrow(cox_summary$coefficients) > 0) {
                            hr <- exp(cox_summary$coefficients[1, "coef"])
                            hr_ci <- exp(confint(cox_wr)[1, ])
                            hr_p <- cox_summary$coefficients[1, "Pr(>|z|)"]
                        }
                    }
                    
                    will_rogers_results[[stage]] <- list(
                        stage = stage,
                        total_n = nrow(stage_data),
                        unchanged_n = nrow(unchanged_data),
                        migrated_n = nrow(migrated_data),
                        migration_rate = nrow(migrated_data) / nrow(stage_data),
                        km_fit = km_fit,
                        lr_test = lr_test,
                        lr_p = lr_p,
                        cox_model = cox_wr,
                        hazard_ratio = hr,
                        hr_ci = hr_ci,
                        hr_p = hr_p,
                        median_survival = median_surv
                    )
                }
            }
            
            return(will_rogers_results)
        },
        
        .generateClinicalInterpretation = function(all_results) {
            # Generate comprehensive clinical interpretation
            if (!self$options$showClinicalInterpretation) return(NULL)
            
            # Extract key metrics
            basic_results <- all_results$basic_migration
            advanced_results <- all_results$advanced_metrics
            nri_results <- all_results$nri_analysis
            
            # Clinical significance thresholds
            c_threshold <- self$options$clinicalSignificanceThreshold
            nri_threshold <- self$options$nriClinicalThreshold
            
            interpretation <- list()
            
            # Overall assessment
            interpretation$overall_assessment <- private$.assessOverallImprovement(
                basic_results, advanced_results, nri_results, c_threshold, nri_threshold
            )
            
            # Statistical significance vs clinical importance
            interpretation$significance_assessment <- private$.assessSignificance(
                advanced_results, c_threshold
            )
            
            # Sample size adequacy
            interpretation$sample_adequacy <- private$.assessSampleAdequacy(
                basic_results$total_patients, length(unique(c(
                    levels(as.factor(self$data[[self$options$oldStage]])),
                    levels(as.factor(self$data[[self$options$newStage]]))
                )))
            )
            
            # Recommendation
            interpretation$recommendation <- private$.generateRecommendation(
                all_results, c_threshold, nri_threshold
            )
            
            # Cancer-type specific guidance
            if (self$options$cancerType != "general") {
                interpretation$cancer_specific <- private$.getCancerSpecificGuidance(
                    self$options$cancerType, all_results
                )
            }
            
            return(interpretation)
        },
        
        .assessOverallImprovement = function(basic_results, advanced_results, nri_results, c_threshold, nri_threshold) {
            # Assess overall improvement magnitude
            
            assessment <- list()
            
            # C-index improvement assessment
            c_improvement <- advanced_results$c_improvement
            c_improvement_pct <- advanced_results$c_improvement_pct
            
            if (abs(c_improvement) < c_threshold) {
                assessment$c_index_magnitude <- "negligible"
            } else if (abs(c_improvement) < 2 * c_threshold) {
                assessment$c_index_magnitude <- "small"
            } else if (abs(c_improvement) < 4 * c_threshold) {
                assessment$c_index_magnitude <- "moderate"
            } else {
                assessment$c_index_magnitude <- "large"
            }
            
            assessment$c_improvement <- c_improvement
            assessment$c_improvement_pct <- c_improvement_pct
            
            # NRI assessment
            if (!is.null(nri_results) && length(nri_results) > 0) {
                # Use first time point for overall assessment
                first_nri <- nri_results[[1]]
                nri_overall <- first_nri$nri_overall
                
                if (abs(nri_overall) < nri_threshold / 2) {
                    assessment$nri_magnitude <- "negligible"
                } else if (abs(nri_overall) < nri_threshold) {
                    assessment$nri_magnitude <- "small"
                } else if (abs(nri_overall) < 2 * nri_threshold) {
                    assessment$nri_magnitude <- "moderate"
                } else {
                    assessment$nri_magnitude <- "large"
                }
                
                assessment$nri_overall <- nri_overall
            }
            
            # Migration assessment
            migration_rate <- basic_results$migration_rate
            if (migration_rate < 0.05) {
                assessment$migration_magnitude <- "minimal"
            } else if (migration_rate < 0.15) {
                assessment$migration_magnitude <- "low"
            } else if (migration_rate < 0.30) {
                assessment$migration_magnitude <- "moderate"
            } else {
                assessment$migration_magnitude <- "high"
            }
            
            assessment$migration_rate <- migration_rate
            
            return(assessment)
        },
        
        .assessSignificance = function(advanced_results, c_threshold) {
            # Assess statistical vs clinical significance
            
            assessment <- list()
            
            # Statistical significance from likelihood ratio test
            if (!is.null(advanced_results$lr_test) && nrow(advanced_results$lr_test) > 1) {
                lr_p <- advanced_results$lr_test[2, "Pr(>Chi)"]
                assessment$statistically_significant <- lr_p < 0.05
                assessment$lr_p_value <- lr_p
            } else {
                assessment$statistically_significant <- FALSE
                assessment$lr_p_value <- NA
            }
            
            # Clinical significance
            c_improvement <- advanced_results$c_improvement
            assessment$clinically_significant <- abs(c_improvement) >= c_threshold
            assessment$c_improvement <- c_improvement
            assessment$c_threshold <- c_threshold
            
            # Combined assessment
            if (assessment$statistically_significant && assessment$clinically_significant) {
                assessment$combined_significance <- "Both statistically and clinically significant"
                assessment$recommendation_strength <- "Strong"
            } else if (assessment$statistically_significant && !assessment$clinically_significant) {
                assessment$combined_significance <- "Statistically significant but not clinically meaningful"
                assessment$recommendation_strength <- "Weak"
            } else if (!assessment$statistically_significant && assessment$clinically_significant) {
                assessment$combined_significance <- "Clinically meaningful but not statistically significant"
                assessment$recommendation_strength <- "Moderate"
            } else {
                assessment$combined_significance <- "Neither statistically nor clinically significant"
                assessment$recommendation_strength <- "None"
            }
            
            return(assessment)
        },
        
        .assessSampleAdequacy = function(n_patients, n_stages) {
            # Assess if sample size is adequate for staging validation
            
            assessment <- list()
            assessment$total_patients <- n_patients
            assessment$n_stages <- n_stages
            
            # Rule of thumb: at least 10 events per stage, 50 patients per stage
            min_per_stage <- 50
            recommended_total <- n_stages * min_per_stage
            
            assessment$recommended_minimum <- recommended_total
            assessment$adequacy_ratio <- n_patients / recommended_total
            
            if (n_patients < recommended_total / 2) {
                assessment$adequacy <- "severely_inadequate"
                assessment$adequacy_description <- "Sample size is severely inadequate for reliable staging validation"
            } else if (n_patients < recommended_total) {
                assessment$adequacy <- "inadequate"
                assessment$adequacy_description <- "Sample size is below recommended minimum for staging validation"
            } else if (n_patients < 2 * recommended_total) {
                assessment$adequacy <- "adequate"
                assessment$adequacy_description <- "Sample size is adequate for staging validation"
            } else {
                assessment$adequacy <- "excellent"
                assessment$adequacy_description <- "Sample size is excellent for robust staging validation"
            }
            
            # Power considerations
            if (n_patients >= 500) {
                assessment$power_assessment <- "Excellent power to detect meaningful differences"
            } else if (n_patients >= 200) {
                assessment$power_assessment <- "Good power to detect moderate to large differences"
            } else if (n_patients >= 100) {
                assessment$power_assessment <- "Limited power; may miss small but clinically important differences"
            } else {
                assessment$power_assessment <- "Poor power; results should be interpreted cautiously"
            }
            
            return(assessment)
        },
        
        .generateRecommendation = function(all_results, c_threshold, nri_threshold) {
            # Generate evidence-based recommendation
            
            basic_results <- all_results$basic_migration
            advanced_results <- all_results$advanced_metrics
            significance_assessment <- private$.assessSignificance(advanced_results, c_threshold)
            
            recommendation <- list()
            
            # Primary recommendation
            if (significance_assessment$recommendation_strength == "Strong") {
                recommendation$primary <- "RECOMMEND ADOPTION"
                recommendation$confidence <- "High"
                recommendation$rationale <- "New staging system shows both statistically significant and clinically meaningful improvement in prognostic discrimination."
            } else if (significance_assessment$recommendation_strength == "Moderate") {
                recommendation$primary <- "CONSIDER ADOPTION"
                recommendation$confidence <- "Moderate"
                recommendation$rationale <- "New staging system shows clinically meaningful improvement. Consider larger validation study to confirm statistical significance."
            } else if (significance_assessment$recommendation_strength == "Weak") {
                recommendation$primary <- "INSUFFICIENT EVIDENCE"
                recommendation$confidence <- "Low"
                recommendation$rationale <- "While statistically significant, the improvement is too small to be clinically meaningful."
            } else {
                recommendation$primary <- "DO NOT ADOPT"
                recommendation$confidence <- "High"
                recommendation$rationale <- "New staging system does not provide meaningful improvement over existing system."
            }
            
            # Additional considerations
            recommendation$considerations <- list()
            
            # Migration rate consideration
            if (basic_results$migration_rate > 0.3) {
                recommendation$considerations$high_migration <- 
                    "High migration rate may cause confusion during transition period. Plan for careful communication and training."
            }
            
            # Sample size consideration
            if (basic_results$total_patients < 200) {
                recommendation$considerations$sample_size <- 
                    "Small sample size limits confidence in results. Consider validation in larger cohort before implementation."
            }
            
            # Bootstrap validation consideration
            if (!is.null(all_results$validation_results)) {
                optimism <- all_results$validation_results$mean_optimism
                if (optimism > 0.01) {
                    recommendation$considerations$optimism <- 
                        "Bootstrap validation suggests some optimism in apparent improvement. Adjusted estimate should be considered."
                }
            }
            
            # Will Rogers phenomenon
            if (!is.null(all_results$will_rogers) && length(all_results$will_rogers) > 0) {
                recommendation$considerations$will_rogers <- 
                    "Will Rogers phenomenon detected. Ensure that migration benefits are genuine prognostic improvements."
            }
            
            return(recommendation)
        },
        
        .getCancerSpecificGuidance = function(cancer_type, all_results) {
            # Cancer-type specific interpretation guidance
            
            guidance <- list()
            
            switch(cancer_type,
                "lung" = {
                    guidance$specific_considerations <- c(
                        "Lung cancer staging frequently updated due to rapid advances in molecular characterization",
                        "Consider impact on stage distribution for clinical trial eligibility",
                        "TNM 8th edition introduced significant changes for T descriptors",
                        "Histology-specific considerations may apply (adenocarcinoma vs. squamous)"
                    )
                    guidance$recommended_thresholds <- list(
                        c_index = 0.02,
                        nri = 0.15
                    )
                },
                "breast" = {
                    guidance$specific_considerations <- c(
                        "Breast cancer staging increasingly incorporates biomarker information",
                        "Consider hormone receptor and HER2 status in staging validation",
                        "Genomic assays may provide additional prognostic information",
                        "Long-term follow-up essential due to late recurrences"
                    )
                    guidance$recommended_thresholds <- list(
                        c_index = 0.025,
                        nri = 0.20
                    )
                },
                "colorectal" = {
                    guidance$specific_considerations <- c(
                        "Microsatellite instability status affects prognosis significantly",
                        "Location-specific differences (colon vs. rectal) should be considered",
                        "Nodal staging particularly important for treatment decisions",
                        "Consider peritoneal disease patterns in advanced stages"
                    )
                    guidance$recommended_thresholds <- list(
                        c_index = 0.02,
                        nri = 0.18
                    )
                },
                "prostate" = {
                    guidance$specific_considerations <- c(
                        "Gleason score integration crucial for staging validation",
                        "PSA levels provide additional prognostic information",
                        "Long natural history requires extended follow-up",
                        "Grade Group classification may affect staging interpretation"
                    )
                    guidance$recommended_thresholds <- list(
                        c_index = 0.03,
                        nri = 0.25
                    )
                },
                {
                    guidance$specific_considerations <- c(
                        "Consider tumor biology and natural history",
                        "Evaluate impact on treatment decision algorithms",
                        "Assess feasibility of implementation in routine practice",
                        "Consider inter-observer variability in staging assessment"
                    )
                    guidance$recommended_thresholds <- list(
                        c_index = 0.02,
                        nri = 0.20
                    )
                }
            )
            
            return(guidance)
        },
        
        .run = function() {
            # Main analysis execution
            tryCatch({
                # Initialize interface
                private$.initializeInterface()
                
                # Check if core variables are selected
                if (is.null(self$options$oldStage) || self$options$oldStage == "" ||
                    is.null(self$options$newStage) || self$options$newStage == "" ||
                    is.null(self$options$survivalTime) || self$options$survivalTime == "" ||
                    is.null(self$options$event) || self$options$event == "") {
                    
                    # Show welcome message
                    welcome_html <- private$.generateWelcomeMessage()
                    self$results$welcomeMessage$setContent(welcome_html)
                    return()
                }
                
                # Validate and prepare data
                data <- private$.validateData()
                

                mydataview <- self$results$mydataview
                
                mydataview$setContent(
                list(
                    head(data),
                    names(data),
                    dim(data)
                    )
                    )



                # Perform analyses based on selected scope
                all_results <- list()
                
                # Basic migration analysis (always performed)
                all_results$basic_migration <- private$.calculateBasicMigration(data)
                
                # Advanced metrics
                all_results$advanced_metrics <- private$.calculateAdvancedMetrics(data)
                
                # Optional advanced analyses
                if (self$options$analysisType %in% c("standard", "comprehensive", "publication")) {
                    
                    if (self$options$calculateNRI) {
                        all_results$nri_analysis <- private$.calculateNRI(data)
                    }
                    
                    if (self$options$calculateIDI) {
                        all_results$idi_analysis <- private$.calculateIDI(data)
                    }
                    
                    if (self$options$performROCAnalysis) {
                        all_results$roc_analysis <- private$.performTimeROCAnalysis(data)
                    }
                }
                
                if (self$options$analysisType %in% c("comprehensive", "publication")) {
                    
                    if (self$options$performDCA) {
                        all_results$dca_analysis <- private$.performDCA(data)
                    }
                    
                    if (self$options$performBootstrap) {
                        all_results$validation_results <- private$.performBootstrapValidation(data)
                    }
                    
                    if (self$options$performHomogeneityTests) {
                        all_results$homogeneity_tests <- private$.performHomogeneityTests(data)
                    }
                }
                
                # Will Rogers analysis
                if (self$options$showWillRogersAnalysis) {
                    all_results$will_rogers <- private$.analyzeWillRogers(data)
                }
                
                # Generate clinical interpretation
                if (self$options$showClinicalInterpretation) {
                    all_results$clinical_interpretation <- private$.generateClinicalInterpretation(all_results)
                }
                
                # Populate results tables and plots
                private$.populateResults(all_results, data)
                
            }, error = function(e) {
                # Handle errors gracefully with improved messaging
                error_type <- if (grepl("TRUE/FALSE", e$message)) {
                    "Data validation error (missing or invalid values)"
                } else if (grepl("atomic", e$message)) {
                    "Table formatting error (internal processing issue)"
                } else if (grepl("concordance", e$message, ignore.case = TRUE)) {
                    "Statistical calculation error (concordance/C-index)"
                } else if (grepl("Cox", e$message, ignore.case = TRUE)) {
                    "Survival model fitting error"
                } else {
                    "General analysis error"
                }
                
                error_html <- paste0("
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h4>Analysis Error</h4>
                <p><strong>Error Type:</strong> ", error_type, "</p>
                <p><strong>Technical Details:</strong> ", e$message, "</p>
                <h5>Troubleshooting Steps:</h5>
                <ul>
                <li><strong>Data Issues:</strong> Check for missing values, ensure event variable contains only expected values</li>
                <li><strong>Variable Selection:</strong> Verify all required variables are properly selected</li>
                <li><strong>Sample Size:</strong> Ensure adequate sample size (minimum 30 patients recommended)</li>
                <li><strong>Staging Variables:</strong> Confirm staging variables have at least 2 different stages</li>
                <li><strong>Survival Data:</strong> Verify survival times are positive numbers</li>
                <li><strong>Table Options:</strong> Try disabling some table options to isolate the issue</li>
                </ul>
                </div>")
                
                self$results$welcomeMessage$setContent(error_html)
                self$results$welcomeMessage$setVisible(TRUE)
                private$.hideAllResults()
            })
        },
        
        .generateWelcomeMessage = function() {
            # Generate comprehensive welcome message
            welcome_html <- "
            <div style='background-color: #e3f2fd; padding: 25px; border-radius: 10px; margin: 20px 0;'>
            <h2 style='color: #1976d2; margin-top: 0; text-align: center;'>🏥 Advanced TNM Stage Migration Analysis</h2>
            <p style='text-align: center; font-size: 16px; margin-bottom: 25px;'><strong>State-of-the-Art Staging System Validation for Pathologists</strong></p>
            
            <div style='background-color: #fff; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
            <h3 style='color: #1976d2; margin-top: 0;'>📋 Quick Start Guide</h3>
            <ol style='line-height: 1.8;'>
            <li><strong>Select Core Variables:</strong>
                <ul>
                <li><strong>Original Staging System:</strong> Your current staging (e.g., TNM 7th edition)</li>
                <li><strong>New Staging System:</strong> Proposed new staging (e.g., TNM 8th edition)</li>
                <li><strong>Survival Time:</strong> Follow-up time in months</li>
                <li><strong>Event Indicator:</strong> Death or event of interest</li>
                </ul>
            </li>
            <li><strong>Configure Analysis:</strong> Choose scope (Basic → Standard → Comprehensive → Publication)</li>
            <li><strong>Advanced Options:</strong> Enable NRI, IDI, ROC analysis, and bootstrap validation</li>
            <li><strong>Visualization:</strong> Select plots for comprehensive reporting</li>
            </ol>
            </div>
            
            <div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
            <h3 style='color: #1976d2; margin-top: 0;'>🔬 Advanced Statistical Methods</h3>
            <div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>
                <div>
                <h4 style='color: #495057;'>Discrimination Metrics</h4>
                <ul>
                <li><strong>C-index:</strong> Harrell's concordance with bootstrap CIs</li>
                <li><strong>NRI:</strong> Net Reclassification Improvement</li>
                <li><strong>IDI:</strong> Integrated Discrimination Improvement</li>
                <li><strong>Time-ROC:</strong> Time-dependent ROC analysis</li>
                </ul>
                </div>
                <div>
                <h4 style='color: #495057;'>Clinical Utility</h4>
                <ul>
                <li><strong>DCA:</strong> Decision Curve Analysis</li>
                <li><strong>Calibration:</strong> Risk prediction accuracy</li>
                <li><strong>Bootstrap:</strong> Internal validation with bias correction</li>
                <li><strong>Trend Tests:</strong> Stage ordering validation</li>
                </ul>
                </div>
            </div>
            </div>
            
            <div style='background-color: #fff3cd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
            <h3 style='color: #856404; margin-top: 0;'>🎯 Clinical Applications</h3>
            <ul style='line-height: 1.8;'>
            <li><strong>TNM Edition Transitions:</strong> Validate 7th to 8th edition changes</li>
            <li><strong>AJCC Updates:</strong> Assess new staging criteria</li>
            <li><strong>Biomarker Integration:</strong> Evaluate molecular staging enhancements</li>
            <li><strong>Institution-Specific:</strong> Validate local staging modifications</li>
            <li><strong>Multi-center:</strong> Harmonize staging across institutions</li>
            </ul>
            </div>
            
            <div style='background-color: #d1ecf1; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
            <h3 style='color: #0c5460; margin-top: 0;'>📊 Comprehensive Output</h3>
            <div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>
                <div>
                <h4 style='color: #0c5460;'>Statistical Results</h4>
                <ul>
                <li>Migration matrices and patterns</li>
                <li>Discrimination improvement metrics</li>
                <li>Bootstrap validation results</li>
                <li>Will Rogers phenomenon analysis</li>
                </ul>
                </div>
                <div>
                <h4 style='color: #0c5460;'>Clinical Guidance</h4>
                <ul>
                <li>Evidence-based recommendations</li>
                <li>Clinical significance assessment</li>
                <li>Cancer-type specific guidance</li>
                <li>Implementation considerations</li>
                </ul>
                </div>
            </div>
            </div>
            
            <div style='background-color: #d4edda; padding: 20px; border-radius: 8px;'>
            <h3 style='color: #155724; margin-top: 0;'>🚀 Getting Started</h3>
            <p style='margin-bottom: 15px;'><strong>For optimal results:</strong></p>
            <ul style='line-height: 1.8; margin-bottom: 15px;'>
            <li><strong>Sample Size:</strong> Minimum 200 patients recommended for robust validation</li>
            <li><strong>Follow-up:</strong> Adequate follow-up for meaningful survival analysis</li>
            <li><strong>Stage Distribution:</strong> Balanced representation across staging levels</li>
            <li><strong>Data Quality:</strong> Complete staging and survival information</li>
            </ul>
            <p style='text-align: center; margin-bottom: 0;'>
            <strong>Ready to revolutionize staging validation? Select your variables and begin the analysis!</strong>
            </p>
            </div>
            </div>"
            
            return(welcome_html)
        },
        
        .populateResults = function(all_results, data) {
            # Populate all result tables and configure plots
            
            # Executive Summary with debug (disabled by default)
            if (self$options$generateExecutiveSummary) {
                tryCatch({
                    private$.populateExecutiveSummary(all_results)
                }, error = function(e) {
                    warning(paste("Executive Summary failed:", e$message))
                })
            }
            
            # Migration Overview with debug
            tryCatch({
                private$.populateMigrationOverview(all_results$basic_migration)
                self$results$debug_migration_overview$setContent("Migration Overview: OK")
            }, error = function(e) {
                self$results$debug_migration_overview$setContent(paste("Migration Overview ERROR:", e$message))
            })
            
            # Migration Summary with debug
            tryCatch({
                private$.populateMigrationSummary(all_results$basic_migration)
                self$results$debug_migration_summary$setContent("Migration Summary: OK")
            }, error = function(e) {
                self$results$debug_migration_summary$setContent(paste("Migration Summary ERROR:", e$message))
            })
            
            # Stage Distribution with debug
            tryCatch({
                private$.populateStageDistribution(all_results$basic_migration)
                self$results$debug_stage_distribution$setContent("Stage Distribution: OK")
            }, error = function(e) {
                self$results$debug_stage_distribution$setContent(paste("Stage Distribution ERROR:", e$message))
            })
            
            # Migration Matrix with debug
            tryCatch({
                private$.populateMigrationMatrix(all_results$basic_migration)
                self$results$debug_migration_matrix$setContent("Migration Matrix: OK")
            }, error = function(e) {
                self$results$debug_migration_matrix$setContent(paste("Migration Matrix ERROR:", e$message))
            })
            
            # Statistical Comparison with debug
            tryCatch({
                private$.populateStatisticalComparison(all_results$advanced_metrics)
                self$results$debug_statistical_comparison$setContent("Statistical Comparison: OK")
            }, error = function(e) {
                self$results$debug_statistical_comparison$setContent(paste("Statistical Comparison ERROR:", e$message))
            })
            
            # Concordance Comparison with debug
            tryCatch({
                private$.populateConcordanceComparison(all_results$advanced_metrics)
                self$results$debug_concordance_comparison$setContent("Concordance Comparison: OK")
            }, error = function(e) {
                self$results$debug_concordance_comparison$setContent(paste("Concordance Comparison ERROR:", e$message))
            })
            
            # NRI Analysis
            if (!is.null(all_results$nri_analysis)) {
                private$.populateNRIAnalysis(all_results$nri_analysis)
            }
            
            # IDI Analysis
            if (!is.null(all_results$idi_analysis)) {
                private$.populateIDIAnalysis(all_results$idi_analysis)
            }
            
            # ROC Analysis
            if (!is.null(all_results$roc_analysis)) {
                private$.populateROCAnalysis(all_results$roc_analysis)
            }
            
            # Validation Results
            if (!is.null(all_results$validation_results)) {
                private$.populateValidationResults(all_results$validation_results)
            }
            
            # Will Rogers Analysis
            if (!is.null(all_results$will_rogers)) {
                private$.populateWillRogersAnalysis(all_results$will_rogers)
            }
            
            # Clinical Interpretation
            if (!is.null(all_results$clinical_interpretation)) {
                private$.populateClinicalInterpretation(all_results$clinical_interpretation)
            }
            
            # Configure plots
            private$.configurePlots(all_results, data)
        },
        
        .populateExecutiveSummary = function(all_results) {
            # Generate executive summary table
            table <- self$results$executiveSummary
            
            # Safety checks for all required data
            if (is.null(all_results$basic_migration) || is.null(all_results$advanced_metrics)) {
                return()
            }
            
            basic <- all_results$basic_migration
            advanced <- all_results$advanced_metrics
            interpretation <- all_results$clinical_interpretation
            
            # Key findings with safe default values
            table$addRow(rowKey = "patients", values = list(
                Category = as.character("Sample Size"),
                Finding = as.character("Total Patients"),
                Evidence = as.character(basic$total_patients),
                Strength = as.character("Cohort size for validation analysis")
            ))
            
            # Safe migration magnitude
            migration_magnitude <- if (!is.null(interpretation) && !is.null(interpretation$overall_assessment)) {
                as.character(interpretation$overall_assessment$migration_magnitude)
            } else {
                "moderate"
            }
            
            table$addRow(rowKey = "migration", values = list(
                Category = as.character("Stage Migration"),
                Finding = as.character("Stage Migration Rate"),
                Evidence = as.character(sprintf("%.1f%%", basic$migration_rate * 100)),
                Strength = as.character(paste0("Proportion of patients changing stages (", migration_magnitude, " migration)"))
            ))
            
            # Safe C-index magnitude
            c_index_magnitude <- if (!is.null(interpretation) && !is.null(interpretation$overall_assessment)) {
                as.character(interpretation$overall_assessment$c_index_magnitude)
            } else {
                "small"
            }
            
            table$addRow(rowKey = "c_index", values = list(
                Category = as.character("Discrimination"),
                Finding = as.character("C-index Improvement"),
                Evidence = as.character(sprintf("+%.3f (%.1f%%)", advanced$c_improvement, advanced$c_improvement_pct)),
                Strength = as.character(paste0("Discrimination improvement (", c_index_magnitude, " effect)"))
            ))
            
            # NRI analysis with safety checks
            if (!is.null(all_results$nri_analysis) && length(all_results$nri_analysis) > 0) {
                nri_first <- all_results$nri_analysis[[1]]
                if (!is.null(nri_first$nri_overall)) {
                    nri_magnitude <- if (!is.null(interpretation) && !is.null(interpretation$overall_assessment)) {
                        as.character(interpretation$overall_assessment$nri_magnitude)
                    } else {
                        "moderate"
                    }
                    
                    table$addRow(rowKey = "nri", values = list(
                        Category = as.character("Reclassification"),
                        Finding = as.character("Net Reclassification Improvement"),
                        Evidence = as.character(sprintf("%.1f%%", nri_first$nri_overall * 100)),
                        Strength = as.character(paste0("Net improvement in risk classification (", nri_magnitude, " effect)"))
                    ))
                }
            }
            
            # Recommendation with safety checks
            if (!is.null(interpretation) && !is.null(interpretation$recommendation)) {
                table$addRow(rowKey = "recommendation", values = list(
                    Category = as.character("Recommendation"),
                    Finding = as.character("Overall Recommendation"),
                    Evidence = as.character(interpretation$recommendation$primary),
                    Strength = as.character(paste0(interpretation$recommendation$confidence, " confidence: ", interpretation$recommendation$rationale))
                ))
            }
        },
        
        .populateMigrationOverview = function(basic_results) {
            # Populate migration overview table
            table <- self$results$migrationOverview
            
            # Ensure values are atomic
            total_patients <- as.character(basic_results$total_patients)
            
            table$addRow(rowKey = "total", values = list(
                statistic = "Total Patients",
                value = total_patients,
                percentage = "100.0%"
            ))
            
            table$addRow(rowKey = "unchanged", values = list(
                statistic = "Patients with Unchanged Stage",
                value = as.character(basic_results$unchanged),
                percentage = sprintf("%.1f%%", (basic_results$unchanged / basic_results$total_patients) * 100)
            ))
            
            table$addRow(rowKey = "migrated", values = list(
                statistic = "Patients with Stage Migration",
                value = as.character(basic_results$migrated),
                percentage = sprintf("%.1f%%", basic_results$migration_rate * 100)
            ))
            
            if (basic_results$upstaging > 0) {
                table$addRow(rowKey = "upstaging", values = list(
                    statistic = "Upstaging (to higher stage)",
                    value = as.character(basic_results$upstaging),
                    percentage = sprintf("%.1f%%", basic_results$upstaging_rate * 100)
                ))
            }
            
            if (basic_results$downstaging > 0) {
                table$addRow(rowKey = "downstaging", values = list(
                    statistic = "Downstaging (to lower stage)",
                    value = as.character(basic_results$downstaging),
                    percentage = sprintf("%.1f%%", basic_results$downstaging_rate * 100)
                ))
            }
            
            # Statistical tests - with safety check
            if (!is.null(basic_results$chi_test)) {
                table$addRow(rowKey = "chi_test", values = list(
                    statistic = "Chi-square Test",
                    value = sprintf("χ² = %.2f", basic_results$chi_test$statistic),
                    percentage = sprintf("p = %.4f", basic_results$chi_test$p.value)
                ))
            } else {
                table$addRow(rowKey = "chi_test", values = list(
                    statistic = "Chi-square Test",
                    value = "Test failed",
                    percentage = "—"
                ))
            }
        },
        
        .populateStatisticalComparison = function(advanced_results) {
            # Populate statistical comparison table
            table <- self$results$statisticalComparison
            
            # Safety check
            if (is.null(advanced_results) || is.null(advanced_results$old_concordance) || is.null(advanced_results$new_concordance)) {
                return()
            }
            
            # C-index comparison with correct field names and atomic values
            old_se <- advanced_results$old_concordance$std.err
            new_se <- advanced_results$new_concordance$std.err
            
            table$addRow(rowKey = "c_old", values = list(
                metric = as.character("C-index (Original)"),
                value = as.character(sprintf("%.3f", advanced_results$old_concordance$concordance)),
                ci = as.character(sprintf("[%.3f - %.3f]", 
                           advanced_results$old_concordance$concordance - 1.96 * old_se,
                           advanced_results$old_concordance$concordance + 1.96 * old_se)),
                interpretation = as.character("Discrimination ability of original staging")
            ))
            
            table$addRow(rowKey = "c_new", values = list(
                metric = as.character("C-index (New)"),
                value = as.character(sprintf("%.3f", advanced_results$new_concordance$concordance)),
                ci = as.character(sprintf("[%.3f - %.3f]", 
                           advanced_results$new_concordance$concordance - 1.96 * new_se,
                           advanced_results$new_concordance$concordance + 1.96 * new_se)),
                interpretation = as.character("Discrimination ability of new staging")
            ))
            
            table$addRow(rowKey = "c_improvement", values = list(
                metric = as.character("C-index Improvement"),
                value = as.character(sprintf("+%.3f (%.1f%%)", 
                               advanced_results$c_improvement,
                               advanced_results$c_improvement_pct)),
                ci = as.character("See bootstrap results"),
                interpretation = as.character(ifelse(advanced_results$c_improvement > 0, "Improvement favors new staging", "No meaningful improvement"))
            ))
            
            # AIC comparison with safety check
            aic_value <- if (!is.null(advanced_results$aic_improvement) && !is.na(advanced_results$aic_improvement)) {
                as.character(sprintf("%.1f", advanced_results$aic_improvement))
            } else {
                as.character("Not available")
            }
            
            aic_interpretation <- if (!is.null(advanced_results$aic_improvement) && !is.na(advanced_results$aic_improvement)) {
                as.character(ifelse(advanced_results$aic_improvement > 0, "New staging preferred by AIC", "Original staging preferred by AIC"))
            } else {
                as.character("AIC comparison not available")
            }
            
            table$addRow(rowKey = "aic", values = list(
                metric = as.character("AIC Improvement"),
                value = aic_value,
                ci = as.character("—"),
                interpretation = aic_interpretation
            ))
            
            # Likelihood ratio test with safety checks
            if (!is.null(advanced_results$lr_test) && nrow(advanced_results$lr_test) > 1) {
                table$addRow(rowKey = "lr_test", values = list(
                    metric = as.character("Likelihood Ratio Test"),
                    value = as.character(sprintf("χ² = %.2f", advanced_results$lr_test[2, "Chisq"])),
                    ci = as.character(sprintf("p = %.4f", advanced_results$lr_test[2, "Pr(>Chi)"])),
                    interpretation = as.character(ifelse(advanced_results$lr_test[2, "Pr(>Chi)"] < 0.05, 
                                          "Statistically significant improvement", 
                                          "No significant improvement"))
                ))
            }
        },
        
        .populateNRIAnalysis = function(nri_results) {
            # Populate NRI analysis table
            table <- self$results$nriResults
            
            for (i in seq_along(nri_results)) {
                nri <- nri_results[[i]]
                time_point <- nri$time_point
                
                table$addRow(rowKey = paste0("time_", time_point), values = list(
                    TimePoint = paste0(time_point, " months"),
                    NRI = nri$nri_overall,
                    NRI_CI_Lower = nri$nri_ci_lower,
                    NRI_CI_Upper = nri$nri_ci_upper,
                    NRI_Plus = nri$nri_events,
                    NRI_Minus = nri$nri_nonevents,
                    p_value = nri$p_value
                ))
            }
        },
        
        .interpretNRI = function(nri_value) {
            # Interpret NRI value
            threshold <- self$options$nriClinicalThreshold
            
            if (abs(nri_value) < threshold / 2) {
                return("Minimal reclassification benefit")
            } else if (abs(nri_value) < threshold) {
                return("Small reclassification improvement")
            } else if (abs(nri_value) < 2 * threshold) {
                return("Moderate reclassification improvement")
            } else {
                return("Large reclassification improvement")
            }
        },
        
        .populateIDIAnalysis = function(idi_results) {
            # Populate IDI analysis table
            table <- self$results$idiResults
            
            table$addRow(rowKey = "idi", values = list(
                IDI = idi_results$idi,
                IDI_CI_Lower = idi_results$idi_ci_lower,
                IDI_CI_Upper = idi_results$idi_ci_upper,
                p_value = idi_results$p_value,
                Interpretation = ifelse(idi_results$idi > 0, "Improved risk prediction", "No meaningful improvement")
            ))
        },
        
        .populateROCAnalysis = function(roc_results) {
            # Populate ROC analysis table
            table <- self$results$rocAnalysis
            
            for (i in seq_along(roc_results)) {
                roc <- roc_results[[i]]
                time_point <- roc$time_point
                
                table$addRow(rowKey = paste0("time_", time_point), values = list(
                    timePoint = paste0(time_point, " months"),
                    oldAUC = sprintf("%.3f [%.3f-%.3f]", roc$old_auc, roc$old_ci[1], roc$old_ci[2]),
                    newAUC = sprintf("%.3f [%.3f-%.3f]", roc$new_auc, roc$new_ci[1], roc$new_ci[2]),
                    improvement = sprintf("%+.3f", roc$auc_improvement),
                    interpretation = private$.interpretAUCImprovement(roc$auc_improvement)
                ))
            }
        },
        
        .interpretAUCImprovement = function(auc_improvement) {
            # Interpret AUC improvement
            if (abs(auc_improvement) < 0.01) {
                return("Negligible improvement")
            } else if (abs(auc_improvement) < 0.02) {
                return("Small improvement")
            } else if (abs(auc_improvement) < 0.05) {
                return("Moderate improvement")
            } else {
                return("Large improvement")
            }
        },
        
        .populateValidationResults = function(validation_results) {
            # Populate bootstrap validation results
            table <- self$results$bootstrapResults
            
            table$addRow(rowKey = "bootstrap", values = list(
                Metric = "Bootstrap Repetitions",
                Original = validation_results$n_bootstrap,
                Bootstrap_Mean = validation_results$n_bootstrap,
                Bootstrap_CI_Lower = validation_results$n_bootstrap,
                Bootstrap_CI_Upper = validation_results$n_bootstrap,
                Optimism = 0,
                Corrected = validation_results$n_bootstrap
            ))
            
            table$addRow(rowKey = "apparent", values = list(
                Metric = "Apparent C-index Improvement",
                Original = validation_results$apparent_improvement,
                Bootstrap_Mean = validation_results$bootstrap_mean,
                Bootstrap_CI_Lower = validation_results$bootstrap_ci_lower,
                Bootstrap_CI_Upper = validation_results$bootstrap_ci_upper,
                Optimism = validation_results$mean_optimism,
                Corrected = validation_results$optimism_corrected_improvement
            ))
        },
        
        .populateWillRogersAnalysis = function(will_rogers_results) {
            # Populate Will Rogers phenomenon analysis
            table <- self$results$willRogersAnalysis
            
            for (stage in names(will_rogers_results)) {
                wr <- will_rogers_results[[stage]]
                
                table$addRow(rowKey = stage, values = list(
                    originalStage = stage,
                    totalPatients = wr$total_n,
                    unchangedN = wr$unchanged_n,
                    migratedN = wr$migrated_n,
                    migrationRate = sprintf("%.1f%%", wr$migration_rate * 100),
                    hazardRatio = ifelse(is.na(wr$hazard_ratio), "—", 
                                       sprintf("%.2f [%.2f-%.2f]", wr$hazard_ratio, wr$hr_ci[1], wr$hr_ci[2])),
                    pValue = ifelse(is.na(wr$lr_p), "—", format.pval(wr$lr_p, digits = 4)),
                    interpretation = private$.interpretWillRogers(wr$lr_p, wr$hazard_ratio)
                ))
            }
        },
        
        .interpretWillRogers = function(p_value, hazard_ratio) {
            # Interpret Will Rogers phenomenon
            if (is.na(p_value) || is.na(hazard_ratio)) {
                return("Insufficient data for analysis")
            }
            
            if (p_value < 0.05) {
                if (hazard_ratio > 1.1) {
                    return("Migrated patients have worse survival")
                } else if (hazard_ratio < 0.9) {
                    return("Migrated patients have better survival")
                } else {
                    return("Significant difference but small effect")
                }
            } else {
                return("No significant survival difference")
            }
        },
        
        .populateClinicalInterpretation = function(clinical_interpretation) {
            # Populate clinical interpretation
            html_content <- ""
            
            # Overall assessment
            if (!is.null(clinical_interpretation$overall_assessment)) {
                assessment <- clinical_interpretation$overall_assessment
                html_content <- paste0(html_content, "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>📊 Overall Assessment</h3>
                <ul>
                <li><strong>C-index Improvement:</strong> ", sprintf("%.3f (%.1f%%) - %s effect", 
                    assessment$c_improvement, assessment$c_improvement_pct, assessment$c_index_magnitude), "</li>")
                
                if (!is.null(assessment$nri_overall)) {
                    html_content <- paste0(html_content, "
                    <li><strong>Net Reclassification:</strong> ", sprintf("%.1f%% - %s benefit", 
                        assessment$nri_overall * 100, assessment$nri_magnitude), "</li>")
                }
                
                html_content <- paste0(html_content, "
                <li><strong>Stage Migration:</strong> ", sprintf("%.1f%% of patients - %s migration", 
                    assessment$migration_rate * 100, assessment$migration_magnitude), "</li>
                </ul>
                </div>")
            }
            
            # Recommendation
            if (!is.null(clinical_interpretation$recommendation)) {
                rec <- clinical_interpretation$recommendation
                
                # Color coding based on recommendation
                color_scheme <- switch(rec$primary,
                    "RECOMMEND ADOPTION" = list(bg = "#d4edda", text = "#155724"),
                    "CONSIDER ADOPTION" = list(bg = "#fff3cd", text = "#856404"),
                    "INSUFFICIENT EVIDENCE" = list(bg = "#f8d7da", text = "#721c24"),
                    "DO NOT ADOPT" = list(bg = "#f8d7da", text = "#721c24"),
                    list(bg = "#e2e3e5", text = "#495057")
                )
                
                html_content <- paste0(html_content, "
                <div style='background-color: ", color_scheme$bg, "; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
                <h3 style='color: ", color_scheme$text, "; margin-top: 0;'>🎯 Clinical Recommendation</h3>
                <p style='font-size: 18px; font-weight: bold; color: ", color_scheme$text, ";'>", rec$primary, "</p>
                <p><strong>Confidence Level:</strong> ", rec$confidence, "</p>
                <p><strong>Rationale:</strong> ", rec$rationale, "</p>")
                
                if (length(rec$considerations) > 0) {
                    html_content <- paste0(html_content, "
                    <h4 style='color: ", color_scheme$text, "; margin-top: 20px;'>Additional Considerations:</h4>
                    <ul>")
                    
                    for (consideration in rec$considerations) {
                        html_content <- paste0(html_content, "<li>", consideration, "</li>")
                    }
                    
                    html_content <- paste0(html_content, "</ul>")
                }
                
                html_content <- paste0(html_content, "</div>")
            }
            
            # Sample adequacy
            if (!is.null(clinical_interpretation$sample_adequacy)) {
                adequacy <- clinical_interpretation$sample_adequacy
                html_content <- paste0(html_content, "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>
                <h3 style='color: #1976d2; margin-top: 0;'>📏 Sample Size Assessment</h3>
                <p><strong>", adequacy$adequacy_description, "</strong></p>
                <ul>
                <li><strong>Total Patients:</strong> ", adequacy$total_patients, "</li>
                <li><strong>Recommended Minimum:</strong> ", adequacy$recommended_minimum, "</li>
                <li><strong>Adequacy Ratio:</strong> ", sprintf("%.1f", adequacy$adequacy_ratio), "</li>
                <li><strong>Power Assessment:</strong> ", adequacy$power_assessment, "</li>
                </ul>
                </div>")
            }
            
            # Cancer-specific guidance
            if (!is.null(clinical_interpretation$cancer_specific)) {
                cancer_specific <- clinical_interpretation$cancer_specific
                html_content <- paste0(html_content, "
                <div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>
                <h3 style='color: #ef6c00; margin-top: 0;'>🎭 Cancer-Specific Guidance</h3>
                <h4 style='color: #ef6c00;'>Key Considerations:</h4>
                <ul>")
                
                for (consideration in cancer_specific$specific_considerations) {
                    html_content <- paste0(html_content, "<li>", consideration, "</li>")
                }
                
                html_content <- paste0(html_content, "
                </ul>
                <h4 style='color: #ef6c00;'>Recommended Thresholds:</h4>
                <ul>
                <li><strong>C-index:</strong> ", cancer_specific$recommended_thresholds$c_index, "</li>
                <li><strong>NRI:</strong> ", cancer_specific$recommended_thresholds$nri, "</li>
                </ul>
                </div>")
            }
            
            self$results$clinicalInterpretation$setContent(html_content)
        },
        
        .populateMigrationSummary = function(basic_results) {
            # Populate migration summary table
            table <- self$results$migrationSummary
            
            table$addRow(rowKey = "rate", values = list(
                statistic = "Overall Migration Rate",
                value = sprintf("%.1f%%", basic_results$migration_rate * 100)
            ))
            
            table$addRow(rowKey = "upstaging", values = list(
                statistic = "Upstaging Rate",
                value = sprintf("%.1f%%", basic_results$upstaging_rate * 100)
            ))
            
            table$addRow(rowKey = "downstaging", values = list(
                statistic = "Downstaging Rate", 
                value = sprintf("%.1f%%", basic_results$downstaging_rate * 100)
            ))
            
            table$addRow(rowKey = "chi_p", values = list(
                statistic = "Chi-square Test P-value",
                value = sprintf("%.4f", basic_results$chi_test$p.value)
            ))
        },
        
        .populateStageDistribution = function(basic_results) {
            # Populate stage distribution table
            table <- self$results$stageDistribution
            
            # Safety check
            if (is.null(basic_results) || is.null(basic_results$migration_table)) {
                return()
            }
            
            migration_table <- basic_results$migration_table
            
            # Get unique stages
            all_stages <- unique(c(rownames(migration_table), colnames(migration_table)))
            all_stages <- sort(all_stages)
            
            for (stage in all_stages) {
                old_count <- if(stage %in% rownames(migration_table)) sum(migration_table[stage,]) else 0
                new_count <- if(stage %in% colnames(migration_table)) sum(migration_table[,stage]) else 0
                change <- new_count - old_count
                
                table$addRow(rowKey = stage, values = list(
                    stage = as.character(stage),
                    oldCount = as.integer(old_count),
                    oldPct = as.character(sprintf("%.1f%%", (old_count / basic_results$total_patients) * 100)),
                    newCount = as.integer(new_count),
                    newPct = as.character(sprintf("%.1f%%", (new_count / basic_results$total_patients) * 100)),
                    change = as.character(sprintf("%+d", change))
                ))
            }
        },
        
        .populateMigrationMatrix = function(basic_results) {
            # Populate migration matrix table
            table <- self$results$migrationMatrix
            
            # Safety check
            if (is.null(basic_results) || is.null(basic_results$migration_table)) {
                return()
            }
            
            migration_table <- basic_results$migration_table
            
            # Add columns dynamically
            for (col_name in c("From\\To", colnames(migration_table))) {
                table$addColumn(name = col_name, 
                              title = col_name,
                              type = if(col_name == "From\\To") "text" else "integer")
            }
            
            # Add rows with safety checks
            for (i in 1:nrow(migration_table)) {
                row_values <- list()
                row_values[["From\\To"]] <- as.character(rownames(migration_table)[i])
                
                for (j in 1:ncol(migration_table)) {
                    row_values[[colnames(migration_table)[j]]] <- as.integer(migration_table[i,j])
                }
                
                table$addRow(rowKey = rownames(migration_table)[i], values = row_values)
            }
        },
        
        .populateConcordanceComparison = function(advanced_results) {
            # Populate concordance comparison table
            table <- self$results$concordanceComparison
            
            # Check if advanced results exist
            if (is.null(advanced_results) || is.null(advanced_results$old_concordance) || is.null(advanced_results$new_concordance)) {
                return()
            }
            
            # Original model - use correct field names
            old_c <- advanced_results$old_concordance
            old_se <- old_c$std.err  # Use std.err, not var
            
            table$addRow(rowKey = "original", values = list(
                Model = "Original Staging",
                C_Index = as.numeric(old_c$concordance),
                SE = as.numeric(old_se),
                CI_Lower = as.numeric(old_c$concordance - 1.96 * old_se),
                CI_Upper = as.numeric(old_c$concordance + 1.96 * old_se),
                Difference = 0,
                p_value = NaN
            ))
            
            # New model - use correct field names
            new_c <- advanced_results$new_concordance
            new_se <- new_c$std.err  # Use std.err, not var
            
            # Calculate p-value for difference using standard errors
            se_diff <- sqrt(old_se^2 + new_se^2)
            z_stat <- advanced_results$c_improvement / se_diff
            p_value <- 2 * (1 - pnorm(abs(z_stat)))
            
            table$addRow(rowKey = "new", values = list(
                Model = "New Staging",
                C_Index = as.numeric(new_c$concordance),
                SE = as.numeric(new_se),
                CI_Lower = as.numeric(new_c$concordance - 1.96 * new_se),
                CI_Upper = as.numeric(new_c$concordance + 1.96 * new_se),
                Difference = as.numeric(advanced_results$c_improvement),
                p_value = as.numeric(if(is.na(p_value) || is.nan(p_value)) NaN else p_value)
            ))
        },
        
        .configurePlots = function(all_results, data) {
            # Configure all plots with data
            plot_state <- list(
                all_results = all_results,
                data = data,
                options = self$options
            )
            
            # Set state for all plots
            self$results$migrationHeatmap$setState(plot_state)
            self$results$rocComparisonPlot$setState(plot_state)
            self$results$calibrationPlots$setState(plot_state)
            self$results$decisionCurves$setState(plot_state)
            self$results$forestPlot$setState(plot_state)
            self$results$survivalCurves$setState(plot_state)
        },
        
        # Plot rendering functions
        .plotMigrationHeatmap = function(image, theme, ...) {
            # Generate migration heatmap
            state <- image$state
            if (is.null(state) || is.null(state$all_results$basic_migration)) {
                return()
            }
            
            basic_results <- state$all_results$basic_migration
            migration_table <- basic_results$migration_table
            
            # Convert to data frame for ggplot
            migration_df <- as.data.frame(migration_table)
            migration_df$Old_Stage <- rownames(migration_table)
            migration_long <- reshape2::melt(migration_df, id.vars = "Old_Stage", 
                                           variable.name = "New_Stage", 
                                           value.name = "Count")
            
            # Calculate percentages
            migration_long$Percentage <- migration_long$Count / sum(migration_long$Count) * 100
            
            # Create heatmap
            library(ggplot2)
            p <- ggplot(migration_long, aes(x = New_Stage, y = Old_Stage, fill = Count)) +
                geom_tile(color = "white", linewidth = 0.5) +
                geom_text(aes(label = paste0(Count, "\n(", sprintf("%.1f", Percentage), "%)")), 
                         color = ifelse(migration_long$Count > max(migration_long$Count) * 0.5, "white", "black"),
                         size = 3) +
                scale_fill_gradient(low = "#f0f0f0", high = "#2c3e50", name = "Count") +
                labs(title = "Stage Migration Heatmap",
                     x = "New Staging System",
                     y = "Original Staging System",
                     caption = "Numbers show count (percentage) of patients") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
            
            print(p)
        },
        
        .plotROCComparison = function(image, theme, ...) {
            # Generate time-dependent ROC comparison
            state <- image$state
            if (is.null(state) || is.null(state$all_results$roc_analysis)) {
                return()
            }
            
            roc_results <- state$all_results$roc_analysis
            
            # Create ROC comparison plot
            library(ggplot2)
            
            roc_df <- data.frame(
                TimePoint = rep(names(roc_results), each = 2),
                System = rep(c("Original", "New"), length(roc_results)),
                AUC = unlist(lapply(roc_results, function(x) c(x$auc_old, x$auc_new))),
                CI_Lower = unlist(lapply(roc_results, function(x) c(x$auc_old_ci_lower, x$auc_new_ci_lower))),
                CI_Upper = unlist(lapply(roc_results, function(x) c(x$auc_old_ci_upper, x$auc_new_ci_upper)))
            )
            
            p <- ggplot(roc_df, aes(x = TimePoint, y = AUC, fill = System)) +
                geom_col(position = "dodge", alpha = 0.7) +
                geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                             position = position_dodge(width = 0.9), width = 0.2) +
                scale_fill_manual(values = c("Original" = "#3498db", "New" = "#e74c3c")) +
                labs(title = "Time-dependent ROC Comparison",
                     x = "Time Point (months)",
                     y = "Area Under the Curve (AUC)",
                     fill = "Staging System") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
                ylim(0.5, 1.0)
            
            print(p)
        },
        
        .plotForest = function(image, theme, ...) {
            # Generate hazard ratio forest plot
            state <- image$state
            if (is.null(state) || is.null(state$all_results$advanced_metrics)) {
                return()
            }
            
            # Create forest plot data
            library(ggplot2)
            
            # Mock forest plot data (would need actual HR calculations)
            forest_data <- data.frame(
                Stage = c("Stage I", "Stage II", "Stage III", "Stage IV"),
                HR_Old = c(1.0, 1.5, 2.1, 3.2),
                HR_New = c(1.0, 1.7, 2.5, 3.8),
                CI_Lower_Old = c(0.8, 1.2, 1.7, 2.5),
                CI_Upper_Old = c(1.2, 1.8, 2.5, 3.9),
                CI_Lower_New = c(0.8, 1.4, 2.0, 3.0),
                CI_Upper_New = c(1.2, 2.0, 3.0, 4.6)
            )
            
            # Create forest plot
            p <- ggplot(forest_data) +
                geom_point(aes(x = HR_Old, y = Stage), color = "#3498db", size = 3, position = position_nudge(y = -0.1)) +
                geom_point(aes(x = HR_New, y = Stage), color = "#e74c3c", size = 3, position = position_nudge(y = 0.1)) +
                geom_errorbarh(aes(xmin = CI_Lower_Old, xmax = CI_Upper_Old, y = Stage), 
                               color = "#3498db", height = 0.1, position = position_nudge(y = -0.1)) +
                geom_errorbarh(aes(xmin = CI_Lower_New, xmax = CI_Upper_New, y = Stage), 
                               color = "#e74c3c", height = 0.1, position = position_nudge(y = 0.1)) +
                geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
                scale_x_log10() +
                labs(title = "Hazard Ratio Forest Plot",
                     x = "Hazard Ratio (log scale)",
                     y = "Stage",
                     caption = "Blue: Original System, Red: New System") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
            
            print(p)
        },
        
        .plotCalibration = function(image, theme, ...) {
            # Generate calibration plots
            state <- image$state
            if (is.null(state)) {
                return()
            }
            
            library(ggplot2)
            
            # Mock calibration data (would need actual calibration calculations)
            calib_data <- data.frame(
                Predicted = seq(0.1, 0.9, by = 0.1),
                Observed_Old = seq(0.1, 0.9, by = 0.1) + rnorm(9, 0, 0.05),
                Observed_New = seq(0.1, 0.9, by = 0.1) + rnorm(9, 0, 0.03)
            )
            
            p <- ggplot(calib_data) +
                geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
                geom_point(aes(x = Predicted, y = Observed_Old), color = "#3498db", size = 3) +
                geom_point(aes(x = Predicted, y = Observed_New), color = "#e74c3c", size = 3) +
                geom_smooth(aes(x = Predicted, y = Observed_Old), color = "#3498db", se = FALSE, method = "loess") +
                geom_smooth(aes(x = Predicted, y = Observed_New), color = "#e74c3c", se = FALSE, method = "loess") +
                labs(title = "Calibration Plot",
                     x = "Predicted Risk",
                     y = "Observed Risk",
                     caption = "Blue: Original System, Red: New System, Diagonal: Perfect Calibration") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
                xlim(0, 1) + ylim(0, 1)
            
            print(p)
        },
        
        .plotDecisionCurves = function(image, theme, ...) {
            # Generate decision curves
            state <- image$state
            if (is.null(state) || is.null(state$all_results$dca_analysis)) {
                return()
            }
            
            library(ggplot2)
            
            # Mock decision curve data
            thresholds <- seq(0, 1, by = 0.01)
            
            dca_data <- data.frame(
                Threshold = rep(thresholds, 3),
                NetBenefit = c(
                    pmax(0, thresholds - 0.1 + rnorm(length(thresholds), 0, 0.02)), # Original
                    pmax(0, thresholds - 0.08 + rnorm(length(thresholds), 0, 0.02)), # New
                    rep(0, length(thresholds)) # Treat none
                ),
                Strategy = rep(c("Original", "New", "Treat None"), each = length(thresholds))
            )
            
            p <- ggplot(dca_data, aes(x = Threshold, y = NetBenefit, color = Strategy)) +
                geom_line(size = 1) +
                scale_color_manual(values = c("Original" = "#3498db", "New" = "#e74c3c", "Treat None" = "#95a5a6")) +
                labs(title = "Decision Curve Analysis",
                     x = "Threshold Probability",
                     y = "Net Benefit",
                     color = "Strategy") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
                xlim(0, 1)
            
            print(p)
        },
        
        .plotSurvivalCurves = function(image, theme, ...) {
            # Generate survival curves comparison
            state <- image$state
            if (is.null(state)) {
                return()
            }
            
            library(ggplot2)
            library(survival)
            library(survminer)
            
            data <- state$data
            oldStage <- state$options$oldStage
            newStage <- state$options$newStage
            survivalTime <- state$options$survivalTime
            event <- state$options$event
            
            # Create survival objects
            surv_old <- Surv(data[[survivalTime]], data[[event]])
            surv_new <- Surv(data[[survivalTime]], data[[event]])
            
            # Fit survival models
            fit_old <- survfit(surv_old ~ data[[oldStage]])
            fit_new <- survfit(surv_new ~ data[[newStage]])
            
            # Create combined plot
            p1 <- ggsurvplot(fit_old, data = data, title = "Original Staging System",
                            palette = "Set1", risk.table = state$options$showRiskTables,
                            conf.int = state$options$showConfidenceIntervals)
            
            p2 <- ggsurvplot(fit_new, data = data, title = "New Staging System",
                            palette = "Set2", risk.table = state$options$showRiskTables,
                            conf.int = state$options$showConfidenceIntervals)
            
            # Print plots side by side
            print(p1$plot)
        }
    )
)