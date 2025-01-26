#' @title Treatment Pathway Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom tidyr separate_rows

alluvialSurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "alluvialSurvivalClass",
    inherit = alluvialSurvivalBase,
    private = list(
        # .init = function() {
        #     # Initialize results visibility
        #     private$.initResults()
        # },

        # .initResults = function() {
        #     if (private$.areRequiredOptionsSet()) {
        #         self$results$todo$setVisible(FALSE)
        #         self$results$plot$setVisible(TRUE)
        #         self$results$summaryTable$setVisible(TRUE)
        #     } else {
        #         todo <- "
        #             <br>Welcome to Treatment Pathway Visualization
        #             <br><br>
        #             This tool requires the following variables:
        #             <br>- Time: Time points for measurements
        #             <br>- Disease Stage: Stage at each time point
        #             <br>- Treatment: Treatment received
        #             <br>- Patient ID: Unique identifier
        #             <br>- Survival Status (optional): For survival curves
        #             "
        #         self$results$todo$setContent(todo)
        #         self$results$plot$setVisible(FALSE)
        #         self$results$summaryTable$setVisible(FALSE)
        #     }
        # },

        # .areRequiredOptionsSet = function() {
        #     return (!is.null(self$options$timeVar) &&
        #                 !is.null(self$options$stageVar) &&
        #                 !is.null(self$options$treatmentVar) &&
        #                 !is.null(self$options$patientId))
        # },

        .run = function() {
            # if (!private$.areRequiredOptionsSet())
            #     return()

            # if (is.null(self$data) || nrow(self$data) == 0)
            #     stop('Data contains no complete rows')

            # Create a copy of the data for safety
            plotData <- self$data

            # Verify all required columns exist
            # requiredCols <- c(self$options$timeVar,
            #                   self$options$stageVar,
            #                   self$options$treatmentVar,
            #                   self$options$patientId)


            mydataview <- self$results$mydataview
            mydataview$setContent(
              list(
                  plotData
              )
            )


            # missingCols <- requiredCols[!requiredCols %in% names(plotData)]
            # if (length(missingCols) > 0)
            #     stop(paste("Missing required columns:", paste(missingCols, collapse=", ")))

            # Calculate summary statistics
            # stats <- private$.calculateStats(plotData)

            # Format statistics for display
            # formattedStats <- private$.formatStats(stats)
            # self$results$summaryTable$setContent(formattedStats)

            # Store data for plotting
            # image <- self$results$plot
            # image$setState(list(
            #     data = plotData,
            #     stats = stats,
            #     options = self$options
            # ))
        }

        # ,
        # .calculateStats = function(data) {
        #     # Calculate statistics using base R functions for reliability
        #     timePoints <- sort(unique(data[[self$options$timeVar]]))
        #
        #     stats <- lapply(timePoints, function(t) {
        #         # Subset data for this time point
        #         currentData <- data[data[[self$options$timeVar]] == t,]
        #
        #         # Calculate basic counts
        #         result <- list(
        #             timePoint = t,
        #             totalPatients = length(unique(currentData[[self$options$patientId]])),
        #             stageDistribution = table(currentData[[self$options$stageVar]]),
        #             treatmentDistribution = table(currentData[[self$options$treatmentVar]])
        #         )
        #
        #         # Add survival statistics if available
        #         if (!is.null(self$options$survivalVar) &&
        #             self$options$survivalVar %in% names(currentData)) {
        #             result$survivalRate <- mean(currentData[[self$options$survivalVar]])
        #         }
        #
        #         return(result)
        #     })
        #
        #     return(stats)
        # },

        # .formatStats = function(stats) {
        #     # Convert stats to a presentable format
        #     formatted <- do.call(rbind, lapply(stats, function(s) {
        #         data.frame(
        #             Time = s$timePoint,
        #             Patients = s$totalPatients,
        #             Stages = paste(names(s$stageDistribution), s$stageDistribution,
        #                            collapse=", "),
        #             Treatments = paste(names(s$treatmentDistribution),
        #                                s$treatmentDistribution, collapse=", "),
        #             stringsAsFactors = FALSE
        #         )
        #     }))
        #
        #     return(formatted)
        # },

        # .plot = function(image, ggtheme, theme, ...) {
        #     if (is.null(image$state))
        #         return(FALSE)
        #
        #     plotData <- image$state$data
        #     options <- image$state$options
        #
        #     # Create base plot
        #     p <- ggplot2::ggplot(plotData) +
        #         ggplot2::aes_string(
        #             x = options$timeVar,
        #             stratum = options$treatmentVar,
        #             alluvium = options$patientId,
        #             fill = options$stageVar
        #         ) +
        #         ggalluvial::geom_flow(
        #             stat = "alluvium",
        #             lode.guidance = "frontback",
        #             color = "darkgray",
        #             alpha = 0.7
        #         ) +
        #         ggalluvial::geom_stratum() +
        #         ggplot2::theme_minimal() +
        #         ggplot2::theme(
        #             legend.position = "right",
        #             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        #         ) +
        #         ggplot2::labs(
        #             x = "Time",
        #             y = "Number of Patients",
        #             fill = "Disease Stage"
        #         )
        #
        #     # Add survival curve if available
        #     if (!is.null(options$survivalVar) &&
        #         options$survivalVar %in% names(plotData) &&
        #         options$showSurvival) {
        #
        #         survData <- aggregate(
        #             as.formula(paste(options$survivalVar, "~", options$timeVar)),
        #             data = plotData,
        #             FUN = mean
        #         )
        #
        #         maxCount <- max(table(plotData[[options$timeVar]]))
        #
        #         p <- p + ggplot2::geom_line(
        #             data = survData,
        #             ggplot2::aes_string(
        #                 x = options$timeVar,
        #                 y = sprintf("%s * %d", options$survivalVar, maxCount)
        #             ),
        #             color = "red",
        #             linetype = "dashed"
        #         )
        #     }
        #
        #     # Add percentage axis if requested
        #     if (options$showRightAxis) {
        #         p <- p + ggplot2::scale_y_continuous(
        #             sec.axis = ggplot2::sec_axis(
        #                 ~./max(.) * 100,
        #                 name = "Percentage",
        #                 labels = scales::percent
        #             )
        #         )
        #     }
        #
        #     print(p)
        #     return(TRUE)
        # }
    )
)
