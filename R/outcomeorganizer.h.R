
# This file is automatically generated, you probably don't want to edit this

outcomeorganizerOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "outcomeorganizerOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            outcome = NULL,
            outcomeLevel = NULL,
            recurrence = NULL,
            recurrenceLevel = NULL,
            patientID = NULL,
            analysistype = "os",
            multievent = FALSE,
            dod = NULL,
            dooc = NULL,
            awd = NULL,
            awod = NULL,
            useHierarchy = FALSE,
            eventPriority = 1,
            intervalCensoring = FALSE,
            intervalStart = NULL,
            intervalEnd = NULL,
            adminCensoring = FALSE,
            adminDate = NULL,
            outputTable = TRUE,
            diagnostics = FALSE,
            visualization = FALSE, ...) {

            super$initialize(
                package="jsurvival",
                name="outcomeorganizer",
                requiresData=TRUE,
                ...)

            private$..outcome <- jmvcore::OptionVariable$new(
                "outcome",
                outcome,
                suggested=list(
                    "ordinal",
                    "nominal",
                    "continuous"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..outcomeLevel <- jmvcore::OptionLevel$new(
                "outcomeLevel",
                outcomeLevel,
                variable="(outcome)")
            private$..recurrence <- jmvcore::OptionVariable$new(
                "recurrence",
                recurrence,
                suggested=list(
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..recurrenceLevel <- jmvcore::OptionLevel$new(
                "recurrenceLevel",
                recurrenceLevel,
                variable="(recurrence)")
            private$..patientID <- jmvcore::OptionVariable$new(
                "patientID",
                patientID)
            private$..analysistype <- jmvcore::OptionList$new(
                "analysistype",
                analysistype,
                options=list(
                    "os",
                    "cause",
                    "compete",
                    "rfs",
                    "pfs",
                    "dfs",
                    "ttp",
                    "multistate"),
                default="os")
            private$..multievent <- jmvcore::OptionBool$new(
                "multievent",
                multievent,
                default=FALSE)
            private$..dod <- jmvcore::OptionLevel$new(
                "dod",
                dod,
                variable="(outcome)",
                allowNone=TRUE)
            private$..dooc <- jmvcore::OptionLevel$new(
                "dooc",
                dooc,
                variable="(outcome)",
                allowNone=TRUE)
            private$..awd <- jmvcore::OptionLevel$new(
                "awd",
                awd,
                variable="(outcome)",
                allowNone=TRUE)
            private$..awod <- jmvcore::OptionLevel$new(
                "awod",
                awod,
                variable="(outcome)",
                allowNone=TRUE)
            private$..useHierarchy <- jmvcore::OptionBool$new(
                "useHierarchy",
                useHierarchy,
                default=FALSE)
            private$..eventPriority <- jmvcore::OptionInteger$new(
                "eventPriority",
                eventPriority,
                default=1)
            private$..intervalCensoring <- jmvcore::OptionBool$new(
                "intervalCensoring",
                intervalCensoring,
                default=FALSE)
            private$..intervalStart <- jmvcore::OptionVariable$new(
                "intervalStart",
                intervalStart)
            private$..intervalEnd <- jmvcore::OptionVariable$new(
                "intervalEnd",
                intervalEnd)
            private$..adminCensoring <- jmvcore::OptionBool$new(
                "adminCensoring",
                adminCensoring,
                default=FALSE)
            private$..adminDate <- jmvcore::OptionVariable$new(
                "adminDate",
                adminDate)
            private$..outputTable <- jmvcore::OptionBool$new(
                "outputTable",
                outputTable,
                default=TRUE)
            private$..diagnostics <- jmvcore::OptionBool$new(
                "diagnostics",
                diagnostics,
                default=FALSE)
            private$..visualization <- jmvcore::OptionBool$new(
                "visualization",
                visualization,
                default=FALSE)
            private$..addOutcome <- jmvcore::OptionOutput$new(
                "addOutcome")

            self$.addOption(private$..outcome)
            self$.addOption(private$..outcomeLevel)
            self$.addOption(private$..recurrence)
            self$.addOption(private$..recurrenceLevel)
            self$.addOption(private$..patientID)
            self$.addOption(private$..analysistype)
            self$.addOption(private$..multievent)
            self$.addOption(private$..dod)
            self$.addOption(private$..dooc)
            self$.addOption(private$..awd)
            self$.addOption(private$..awod)
            self$.addOption(private$..useHierarchy)
            self$.addOption(private$..eventPriority)
            self$.addOption(private$..intervalCensoring)
            self$.addOption(private$..intervalStart)
            self$.addOption(private$..intervalEnd)
            self$.addOption(private$..adminCensoring)
            self$.addOption(private$..adminDate)
            self$.addOption(private$..outputTable)
            self$.addOption(private$..diagnostics)
            self$.addOption(private$..visualization)
            self$.addOption(private$..addOutcome)
        }),
    active = list(
        outcome = function() private$..outcome$value,
        outcomeLevel = function() private$..outcomeLevel$value,
        recurrence = function() private$..recurrence$value,
        recurrenceLevel = function() private$..recurrenceLevel$value,
        patientID = function() private$..patientID$value,
        analysistype = function() private$..analysistype$value,
        multievent = function() private$..multievent$value,
        dod = function() private$..dod$value,
        dooc = function() private$..dooc$value,
        awd = function() private$..awd$value,
        awod = function() private$..awod$value,
        useHierarchy = function() private$..useHierarchy$value,
        eventPriority = function() private$..eventPriority$value,
        intervalCensoring = function() private$..intervalCensoring$value,
        intervalStart = function() private$..intervalStart$value,
        intervalEnd = function() private$..intervalEnd$value,
        adminCensoring = function() private$..adminCensoring$value,
        adminDate = function() private$..adminDate$value,
        outputTable = function() private$..outputTable$value,
        diagnostics = function() private$..diagnostics$value,
        visualization = function() private$..visualization$value,
        addOutcome = function() private$..addOutcome$value),
    private = list(
        ..outcome = NA,
        ..outcomeLevel = NA,
        ..recurrence = NA,
        ..recurrenceLevel = NA,
        ..patientID = NA,
        ..analysistype = NA,
        ..multievent = NA,
        ..dod = NA,
        ..dooc = NA,
        ..awd = NA,
        ..awod = NA,
        ..useHierarchy = NA,
        ..eventPriority = NA,
        ..intervalCensoring = NA,
        ..intervalStart = NA,
        ..intervalEnd = NA,
        ..adminCensoring = NA,
        ..adminDate = NA,
        ..outputTable = NA,
        ..diagnostics = NA,
        ..visualization = NA,
        ..addOutcome = NA)
)

outcomeorganizerResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "outcomeorganizerResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        summary = function() private$.items[["summary"]],
        outputTable = function() private$.items[["outputTable"]],
        diagnosticsTable = function() private$.items[["diagnosticsTable"]],
        outcomeViz = function() private$.items[["outcomeViz"]],
        addOutcome = function() private$.items[["addOutcome"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Enhanced Outcome Organizer for Survival Analysis",
                refs=list(
                    "survival",
                    "survminer",
                    "finalfit",
                    "cmprsk",
                    "mstate",
                    "survivaltutorial",
                    "ClinicoPathJamoviModule"))
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="To Do",
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod",
                    "recurrence",
                    "recurrenceLevel")))
            self$add(jmvcore::Html$new(
                options=options,
                name="summary",
                title="Summary of Outcome Recoding",
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod",
                    "recurrence",
                    "recurrenceLevel",
                    "useHierarchy",
                    "eventPriority",
                    "intervalCensoring",
                    "adminCensoring")))
            self$add(jmvcore::Table$new(
                options=options,
                name="outputTable",
                title="Recoded Outcome Summary",
                visible="(outputTable)",
                rows=0,
                columns=list(
                    list(
                        `name`="outcome", 
                        `title`="Value", 
                        `type`="text"),
                    list(
                        `name`="label", 
                        `title`="Meaning", 
                        `type`="text"),
                    list(
                        `name`="count", 
                        `title`="Count", 
                        `type`="integer"),
                    list(
                        `name`="percentage", 
                        `title`="Percentage", 
                        `type`="number")),
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod",
                    "recurrence",
                    "recurrenceLevel",
                    "useHierarchy",
                    "eventPriority")))
            self$add(jmvcore::Table$new(
                options=options,
                name="diagnosticsTable",
                title="Diagnostic Information",
                visible="(diagnostics)",
                rows=0,
                columns=list(
                    list(
                        `name`="check", 
                        `title`="Check", 
                        `type`="text"),
                    list(
                        `name`="result", 
                        `title`="Result", 
                        `type`="text")),
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod",
                    "recurrence",
                    "recurrenceLevel",
                    "useHierarchy",
                    "eventPriority",
                    "intervalCensoring",
                    "adminCensoring")))
            self$add(jmvcore::Image$new(
                options=options,
                name="outcomeViz",
                title="Outcome Distribution",
                width=500,
                height=400,
                renderFun=".plotOutcome",
                visible="(visualization)",
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod")))
            self$add(jmvcore::Output$new(
                options=options,
                name="addOutcome",
                title="Add Recoded Outcome to Data",
                varTitle="`Recoded Outcome for {analysistype} Survival Analysis`",
                varDescription="Outcome variable recoded for survival analysis based on selected analysis type",
                measureType="nominal",
                clearWith=list(
                    "outcome",
                    "outcomeLevel",
                    "multievent",
                    "analysistype",
                    "dod",
                    "dooc",
                    "awd",
                    "awod",
                    "recurrence",
                    "recurrenceLevel",
                    "useHierarchy",
                    "eventPriority",
                    "intervalCensoring",
                    "adminCensoring")))}))

outcomeorganizerBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "outcomeorganizerBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jsurvival",
                name = "outcomeorganizer",
                version = c(0,0,3),
                options = options,
                results = outcomeorganizerResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'none')
        }))

#' Outcome Organizer for Survival Analysis
#'
#' Advanced tool for preparing outcome variables for various types of survival 
#' analysis including overall survival, cause-specific, competing risks, 
#' progression-free survival, and multistate models.
#'
#' @examples
#' \donttest{
#' # Example usage will be added
#'}
#' @param data The data as a data frame.
#' @param outcome The primary outcome variable to be recoded for survival
#'   analysis (e.g., vital status).
#' @param outcomeLevel The level of the outcome variable that represents the
#'   event of interest (e.g., "Death", "Relapse").
#' @param recurrence Variable indicating disease recurrence or progression
#'   (for RFS/PFS/DFS analyses).
#' @param recurrenceLevel The level indicating recurrence or progression has
#'   occurred.
#' @param patientID Patient identifier for handling multiple records or
#'   applying event hierarchies.
#' @param analysistype The type of survival analysis to prepare the outcome
#'   for.
#' @param multievent If true, allows for multiple event types (e.g., death
#'   from disease vs death from other causes).
#' @param dod The level representing death from the disease of interest.
#' @param dooc The level representing death from causes other than the disease
#'   of interest.
#' @param awd The level representing patients who are alive but have the
#'   disease.
#' @param awod The level representing patients who are alive and disease-free.
#' @param useHierarchy If true, applies a hierarchy when multiple events occur
#'   for the same patient.
#' @param eventPriority The event code (e.g., 1, 2) that takes precedence when
#'   multiple events occur.
#' @param intervalCensoring If true, prepares data for interval-censored
#'   analysis where exact event times are unknown.
#' @param intervalStart Variable containing the start of the interval when the
#'   event might have occurred.
#' @param intervalEnd Variable containing the end of the interval when the
#'   event might have occurred.
#' @param adminCensoring If true, applies administrative censoring at a
#'   specified date.
#' @param adminDate Variable containing the administrative censoring date.
#' @param outputTable If true, displays a table showing the frequency of each
#'   recoded outcome value.
#' @param diagnostics If true, displays diagnostic information about the
#'   recoding process.
#' @param visualization If true, displays a visualization of the distribution
#'   of recoded outcomes.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$summary} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$outputTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$diagnosticsTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$outcomeViz} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$addOutcome} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$outputTable$asDF}
#'
#' \code{as.data.frame(results$outputTable)}
#'
#' @export
outcomeorganizer <- function(
    data,
    outcome,
    outcomeLevel,
    recurrence,
    recurrenceLevel,
    patientID,
    analysistype = "os",
    multievent = FALSE,
    dod,
    dooc,
    awd,
    awod,
    useHierarchy = FALSE,
    eventPriority = 1,
    intervalCensoring = FALSE,
    intervalStart,
    intervalEnd,
    adminCensoring = FALSE,
    adminDate,
    outputTable = TRUE,
    diagnostics = FALSE,
    visualization = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("outcomeorganizer requires jmvcore to be installed (restart may be required)")

    if ( ! missing(outcome)) outcome <- jmvcore::resolveQuo(jmvcore::enquo(outcome))
    if ( ! missing(recurrence)) recurrence <- jmvcore::resolveQuo(jmvcore::enquo(recurrence))
    if ( ! missing(patientID)) patientID <- jmvcore::resolveQuo(jmvcore::enquo(patientID))
    if ( ! missing(intervalStart)) intervalStart <- jmvcore::resolveQuo(jmvcore::enquo(intervalStart))
    if ( ! missing(intervalEnd)) intervalEnd <- jmvcore::resolveQuo(jmvcore::enquo(intervalEnd))
    if ( ! missing(adminDate)) adminDate <- jmvcore::resolveQuo(jmvcore::enquo(adminDate))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(outcome), outcome, NULL),
            `if`( ! missing(recurrence), recurrence, NULL),
            `if`( ! missing(patientID), patientID, NULL),
            `if`( ! missing(intervalStart), intervalStart, NULL),
            `if`( ! missing(intervalEnd), intervalEnd, NULL),
            `if`( ! missing(adminDate), adminDate, NULL))

    for (v in recurrence) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- outcomeorganizerOptions$new(
        outcome = outcome,
        outcomeLevel = outcomeLevel,
        recurrence = recurrence,
        recurrenceLevel = recurrenceLevel,
        patientID = patientID,
        analysistype = analysistype,
        multievent = multievent,
        dod = dod,
        dooc = dooc,
        awd = awd,
        awod = awod,
        useHierarchy = useHierarchy,
        eventPriority = eventPriority,
        intervalCensoring = intervalCensoring,
        intervalStart = intervalStart,
        intervalEnd = intervalEnd,
        adminCensoring = adminCensoring,
        adminDate = adminDate,
        outputTable = outputTable,
        diagnostics = diagnostics,
        visualization = visualization)

    analysis <- outcomeorganizerClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

