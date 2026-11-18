# Read-only qualification of GUI-saved files. Run from the jsurvival root.
suppressPackageStartupMessages(library(RProtoBuf))
readProtoFiles(system.file("jamovi.proto", package = "jmvcore"))
metadata <- yaml::read_yaml("data-raw/gui-examples.yaml")$datasets
catalog <- yaml::read_yaml("jamovi/0000.yaml")$datasets
tutorials <- jsonlite::fromJSON("data-raw/gui-example-tutorials.json",
                                simplifyVector = FALSE)
checks <- 0L
check <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
  checks <<- checks + 1L
}
decode <- function(x) {
  if (x$has("c")) {
    values <- lapply(x$c$options, decode)
    if (x$c$hasNames) names(values) <- unlist(x$c$names)
    return(values)
  }
  for (kind in c("s", "d", "i")) if (x$has(kind)) return(x[[kind]])
  if (x$has("o")) return(switch(as.character(x$o), "0" = FALSE, "1" = TRUE))
  NULL
}
flatten <- function(x) {
  children <- if (x$has("group")) x$group$elements else if (x$has("array")) {
    x$array$elements
  } else list()
  c(setNames(list(x), x$name), unlist(lapply(children, flatten), recursive = FALSE))
}
column <- function(x, name) {
  col <- Filter(function(z) z$name == name, x$table$columns)[[1]]
  vapply(col$cells, function(z) {
    if (z$has("s")) return(z$s)
    if (z$has("d")) return(as.character(z$d))
    if (z$has("i")) return(as.character(z$i))
    NA_character_
  }, character(1))
}
analysis_options <- function(analysis) {
  options <- lapply(analysis$options$options, decode)
  names(options) <- unlist(analysis$options$names)
  options
}
required_images <- function(name, options) {
  enabled <- function(key) isTRUE(options[[key]])
  choose <- function(key, image = key) if (enabled(key)) image else character()
  switch(name,
    singlearm = c(
      if (enabled("sc")) {
        if (enabled("multievent") && identical(options$analysistype, "compete")) {
          "plot_cif"
        } else "plot"
      },
      choose("kmunicate", "plot6"), choose("ce", "plot2"), choose("ch", "plot3"),
      choose("baseline_hazard", "baselineHazardPlot"),
      choose("hazard_smoothing", "smoothedHazardPlot")
    ),
    survival = c(
      choose("sc", "plot"), choose("ph_cox", "plot8"),
      choose("ce", "plot2"), choose("ch", "plot3"),
      choose("kmunicate", "plot6"), choose("loglog", "plot7"),
      choose("residual_diagnostics", "residualsPlot"),
      if (enabled("age_adjustment") && enabled("adjusted_curves")) "adjustedCurvesPlot",
      if (enabled("age_adjustment") && enabled("age_stratified_km")) "ageStratifiedKMPlot",
      choose("calibration_curves", "calibrationPlot"), choose("rcs_analysis", "rcsPlot"),
      if (enabled("use_parametric") && enabled("parametric_survival_plots")) {
        "parametricSurvivalPlot"
      }
    ),
    survivalcont = c(
      choose("findcut", "plot4"),
      if (enabled("findcut")) c(choose("sc", "plot5"), choose("ce", "plot2"),
        choose("ch", "plot3"), choose("kmunicate", "plot6"), choose("loglog", "plot7")),
      choose("multiple_cutoffs", "plotMultipleCutoffs"),
      if (enabled("multiple_cutoffs") && enabled("sc")) "plotMultipleSurvival",
      choose("residual_diagnostics", "residualsPlot")
    ),
    multisurvival = c(
      if (enabled("hr")) if (identical(options$sty, "t3")) "plot3" else "plot",
      choose("ph_cox", "plot8"), choose("km", "plotKM"), choose("ac", "plot_adj"),
      choose("showNomogram", "plot_nomogram"),
      if (enabled("calculateRiskScore") && enabled("plotRiskGroups")) "riskGroupPlot",
      if (enabled("show_survmetrics") && enabled("survmetrics_show_plots")) "survMetricsPlot"
    ),
    lassocox = c(choose("cv_plot"), choose("coef_plot"), choose("survival_plot"),
                 choose("path_plot")),
    oddsratio = c("plot", if (enabled("showNomogram") && !enabled("usePenalized")) {
      "plot_nomogram"
    }),
    outcomeorganizer = choose("visualization", "outcomeViz"),
    character()
  )
}
check_images <- function(elements, ids, label) {
  check(all(vapply(ids, function(id) {
    element <- elements[[id]]
    !is.null(element) && element$has("image") && nzchar(element$image$path)
  }, logical(1))), paste(label, "requested images cached"))
}
read_integer_output_example <- function(file) {
  # jmvReadWrite currently rejects nominal Integer/Output columns. Read this
  # fixture's column-major integer/double payload directly without changing it.
  meta <- jsonlite::fromJSON(unz(file, "metadata.json"), simplifyVector = FALSE)$dataSet
  labels <- jsonlite::fromJSON(unz(file, "xdata.json"), simplifyVector = FALSE)
  connection <- unz(file, "data.bin", "rb")
  on.exit(close(connection))
  columns <- lapply(meta$fields, function(field) {
    if (!(field$type %in% c("integer", "number"))) stop("Unexpected fixture storage type")
    value <- readBin(connection, if (field$type == "integer") integer() else double(),
                     n = meta$rowCount, endian = "little")
    if (identical(field$dataType, "Text")) {
      levels <- labels[[field$name]]$labels
      codes <- vapply(levels, function(x) x[[1]], numeric(1))
      text <- vapply(levels, function(x) x[[2]], character(1))
      value <- text[match(value, codes)]
    }
    value
  })
  names(columns) <- vapply(meta$fields, `[[`, character(1), "name")
  as.data.frame(columns, check.names = FALSE)
}
saved <- list()
represented <- character()
event_code <- function(x) if (is.logical(x)) as.numeric(x) else as.numeric(as.character(x))
for (entry in metadata) {
  file <- file.path("data", entry$path)
  name <- sub("_example[.]omv$", "", entry$path)
  function_name <- if (name == "oddsratio_binary") "oddsratio" else name
  listing <- unzip(file, list = TRUE)
  streams <- grep("/analysis$", listing$Name, value = TRUE)
  analyses <- lapply(streams, function(stream) {
    connection <- unz(file, stream, "rb")
    on.exit(close(connection))
    read(P("jamovi.coms.AnalysisResponse"), readBin(connection, "raw", 1e7))
  })
  check(length(analyses) > 0L, paste(name, "native analyses present"))
  for (analysis in analyses) {
    label <- paste(name, "analysis", analysis$analysisId)
    check(analysis$status == 3L && !nzchar(analysis$error$message),
          paste(label, "completed status"))
    check(all(vapply(flatten(analysis$results), function(x) {
      !nzchar(x$error$message)
    }, logical(1))), paste(label, "result errors"))
  }
  tutorial <- Filter(function(x) identical(x$file, entry$path), tutorials)
  check(length(tutorial) == 1L, paste(name, "tutorial metadata"))
  introductions <- Filter(function(x) x$name == "empty" && x$ns == "jmv", analyses)
  check(length(introductions) > 0L, paste(name, "native introduction"))
  note <- analysis_options(introductions[[1]])
  check(identical(note[["results//heading"]], tutorial[[1]]$heading),
        paste(name, "native tutorial heading"))
  expected_note <- paste0(paste(unlist(tutorial[[1]]$paragraphs), collapse = "\n\n"), "\n")
  note_text <- paste0(vapply(note[["results//topText"]]$ops, function(x) x$insert,
                            character(1)), collapse = "")
  check(identical(note_text, expected_note), paste(name, "native tutorial text"))
  analyses <- Filter(function(x) x$name != "empty", analyses)
  check(length(analyses) > 0L, paste(name, "saved module analyses present"))
  analyses <- analyses[order(vapply(analyses, function(x) x$index, integer(1)))]
  records <- lapply(analyses, function(analysis) {
    label <- paste(name, "analysis", analysis$analysisId)
    check(analysis$name == function_name && analysis$ns == "jsurvival",
          paste(label, "identity"))
    elements <- flatten(analysis$results)
    options <- analysis_options(analysis)
    images <- Filter(function(x) x$has("image") && nzchar(x$image$path), elements)
    check(all(vapply(images, function(x) x$image$path %in% listing$Name, logical(1))),
          paste(label, "image resources"))
    check_images(elements, required_images(function_name, options), label)
    list(elements = elements, options = options, image_count = length(images))
  })
  represented <- union(represented, function_name)
  check(identical(Filter(function(x) identical(x$path, entry$path), catalog), list(entry)),
        paste(name, "catalog metadata"))
  data <- if (name == "outcomeorganizer") read_integer_output_example(file) else {
    jmvReadWrite::read_omv(file)
  }
  expected_rows <- switch(name, datetimeconverter = 8L, oddsratio_binary = 240L, 150L)
  check(nrow(data) == expected_rows,
        paste(name, "row count"))
  saved[[name]] <- c(list(data = data, analyses = records), records[[1]])
  cat(entry$path, ":", length(records), "completed module analyses;",
      sum(vapply(records, `[[`, integer(1), "image_count")), "cached images\n")
}
required_functions <- c("singlearm", "survival", "survivalcont", "multisurvival",
                        "lassocox", "oddsratio", "datetimeconverter",
                        "outcomeorganizer", "timeinterval")
check(setequal(represented, required_functions), "All nine shipped functions represented")
check(!is.null(saved$oddsratio_binary), "Fixed-endpoint binary workbook registered")

for (name in c("singlearm", "survival", "survivalcont", "multisurvival", "lassocox")) {
  x <- saved[[name]]
  check(all(is.finite(x$data$time) & x$data$time > 0), paste(name, "positive time"))
  check(sum(event_code(x$data$event) == 1) == 106L, paste(name, "event count"))
  check(x$options$outcomeLevel == "1", paste(name, "event coding"))
}
x <- saved$survival
check(isTRUE(x$options$rmst_analysis) && x$options$rmst_tau == 24,
      "Survival prespecified 24-month RMST horizon")
x <- saved$datetimeconverter
check(identical(as.numeric(x$data[["Year - from iso_date"]]),
                as.numeric(format(as.Date(as.character(x$data$iso_date)), "%Y"))),
      "DateTime exported year")
check(identical(as.numeric(column(x$elements$qualityCounts, "column2")), c(8, 1, 7, 0)),
      "DateTime parsing counts")
check(identical(as.numeric(column(x$elements$qualityCounts, "column3")), c(100, 12.5, 87.5, 0)),
      "DateTime percentages")
x <- saved$timeinterval
expected <- as.numeric(as.Date(as.character(x$data$end_date)) -
                       as.Date(as.character(x$data$start_date))) / 30.4375
check(max(abs(expected - x$data[["Calculated Time (months)"]])) < 1e-10,
      "Independent standardized-month arithmetic")
check(x$options$time_basis == "standardized", "Explicit standardized time basis")
check(length(x$analyses) == 2L &&
        identical(vapply(x$analyses, function(model) model$options$time_basis,
                         character(1)), c("standardized", "calendar")),
      "Saved standardized and calendar interval analyses")
check(all(vapply(x$analyses, function(model) {
  identical(model$options$output_unit, "months") &&
    isTRUE(model$options$calculated_time$value) &&
    isTRUE(model$options$include_quality_metrics) && isTRUE(model$options$show_summary) &&
    isTRUE(model$options$show_glossary) && !isTRUE(model$options$use_landmark) &&
    !isTRUE(model$options$remove_negative) && !isTRUE(model$options$remove_extreme)
}, logical(1))), "Both interval bases exported with teaching output and no filtering")
calendar_months <- function(start, end) {
  # Independent base-Date arithmetic: monthly anniversaries clamp the original
  # day to the target month's final day. Use the actual span to the next
  # anniversary for the remaining fraction; do not call the backend/lubridate.
  start <- as.Date(start)
  end <- as.Date(end)
  month_index <- function(date) {
    as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m")) - 1L
  }
  origin_month <- month_index(start)
  origin_day <- as.integer(format(start, "%d"))
  first_day <- function(index) {
    as.Date(sprintf("%04d-%02d-01", index %/% 12L, index %% 12L + 1L))
  }
  anniversary <- function(months) {
    first <- first_day(origin_month + months)
    last_day <- as.integer(first_day(origin_month + months + 1L) - first)
    first + min(origin_day, last_day) - 1L
  }
  whole <- month_index(end) - origin_month
  while (anniversary(whole) > end) whole <- whole - 1L
  while (anniversary(whole + 1L) <= end) whole <- whole + 1L
  whole + as.numeric(end - anniversary(whole)) /
    as.numeric(anniversary(whole + 1L) - anniversary(whole))
}
expected_calendar <- vapply(seq_len(nrow(x$data)), function(row) {
  calendar_months(as.character(x$data$start_date[row]), as.character(x$data$end_date[row]))
}, numeric(1))
check(max(abs(expected_calendar - x$data[["Calculated Time (months) (2)"]])) < 1e-10,
      "Independent calendar-month anniversary arithmetic")
check(any(abs(expected_calendar - expected) > 1e-6),
      "Calendar and standardized examples demonstrate different month definitions")
x <- saved$outcomeorganizer
check(identical(as.numeric(as.character(x$data[["Recoded Outcome (overall survival)"]])),
                as.numeric(as.character(x$data$status) %in% c("DOD", "DOOC"))),
      "Independent overall-survival mapping")
check(length(x$analyses) == 2L &&
        identical(vapply(x$analyses, function(model) model$options$analysistype,
                         character(1)), c("os", "compete")),
      "Saved overall-survival and competing-risk outcome analyses")
check(all(vapply(x$analyses, function(model) {
  isTRUE(model$options$addOutcome$value) && isTRUE(model$options$showNaturalSummary) &&
    isTRUE(model$options$showGlossary)
}, logical(1))), "Both outcome recodings exported with summaries and glossary")
expected_codes <- ifelse(as.character(x$data$status) == "DOD", 1L,
                         ifelse(as.character(x$data$status) == "DOOC", 2L, 0L))
check(identical(tabulate(expected_codes + 1L, nbins = 3L), c(44L, 82L, 24L)),
      "Independent competing-risk counts: censored, disease death, other death")
# The exported factor deliberately uses Censored/Event/Competing labels for the
# downstream survival hand-off. Its physical 1/2/3 factor storage is not the
# endpoint's 0/1/2 coding; verify the native labels before decoding those codes.
output_name <- "Recoded Outcome (competing risks)"
labels <- jsonlite::fromJSON(unz("data/outcomeorganizer_example.omv", "xdata.json"),
                             simplifyVector = FALSE)[[output_name]]$labels
storage <- vapply(labels, function(level) level[[1]], numeric(1))
text <- vapply(labels, function(level) level[[2]], character(1))
check(identical(text, c("Censored", "Event", "Competing")),
      "Competing-risk export retains all three semantic levels")
exported_labels <- text[match(x$data[[output_name]], storage)]
exported_codes <- match(exported_labels, c("Censored", "Event", "Competing")) - 1L
check(identical(exported_codes, expected_codes), "Independent competing-risk exported mapping")
model <- x$analyses[[2]]
displayed_codes <- as.integer(column(model$elements$outputTable, "outcome"))
displayed_counts <- as.integer(column(model$elements$outputTable, "count"))
check(identical(displayed_counts[match(0:2, displayed_codes)], c(44L, 82L, 24L)),
      "Displayed competing-risk summary counts")
x <- saved$multisurvival
data <- x$data
data$event <- event_code(data$event)
fit <- survival::coxph(survival::Surv(time, event) ~ group + stage + age + marker, data)
displayed <- as.numeric(sub(" .*", "", column(x$elements$fittedTable, "column5")[c(2, 4:7)]))
check(max(abs(displayed - exp(stats::coef(fit)))) < 0.0051,
      "Independent multivariable Cox hazard ratios")
x <- saved$lassocox
check(x$options$nfolds == 5 && x$options$lambda == "lambda.min" &&
        x$options$random_seed == 123456, "LASSO saved tuning settings")
check(all(vapply(x$elements[c("cv_plot", "coef_plot", "survival_plot")],
                 function(z) nzchar(z$image$path), logical(1))), "LASSO plots cached")
beta <- as.numeric(column(x$elements$coefficients, "coefficient"))
hr <- as.numeric(column(x$elements$coefficients, "hazardRatio"))
check(max(abs(exp(beta) - hr)) < 1e-10, "LASSO coefficient-HR arithmetic")
check(length(x$analyses) == 2L, "Two saved LASSO penalty-rule analyses")
for (index in seq_along(x$analyses)) {
  model <- x$analyses[[index]]
  check(model$options$nfolds == 5 && model$options$random_seed == 123456 &&
          isTRUE(model$options$standardize), paste("LASSO", index, "matched CV settings"))
  check(identical(model$options$lambda, c("lambda.min", "lambda.1se")[[index]]),
        paste("LASSO", index, "penalty rule"))
  teaching_options <- c("showSummary", "showExplanations", "showEncoding",
                        "showReproducibility", "path_plot")
  check(all(vapply(model$options[teaching_options], isTRUE, logical(1))),
        paste("LASSO", index, "teaching options"))
  check_images(model$elements, c("cv_plot", "coef_plot", "survival_plot", "path_plot"),
               paste("LASSO", index, "four plots"))
  beta <- as.numeric(column(model$elements$coefficients, "coefficient"))
  hr <- as.numeric(column(model$elements$coefficients, "hazardRatio"))
  check(max(abs(exp(beta) - hr)) < 1e-10,
        paste("LASSO", index, "coefficient-HR arithmetic"))
}
x <- saved$oddsratio
check(x$options$usePenalized && x$options$showNomogram, "Firth safeguard requested")
check(grepl("not generated", x$elements$nomogram$html$content, fixed = TRUE),
      "Firth nomogram safeguard displayed")

x <- saved$oddsratio_binary
check(!isTRUE(x$options$usePenalized) && isTRUE(x$options$showNomogram),
      "Binary pathology ordinary logistic and diagnostic outputs requested")
check(identical(x$options$outcome, "nodal_status") &&
        identical(x$options$outcomeLevel, "Positive") &&
        identical(x$options$outcomeNegativeLevel, "Negative") &&
        identical(x$options$diagnosticPredictor, "diagnostic_test") &&
        identical(x$options$predictorLevel, "Positive") &&
        identical(x$options$predictorNegativeLevel, "Negative"),
      "Binary pathology explicit outcome and test coding")
data <- x$data
data$.reference_positive <- as.integer(as.character(data$nodal_status) == "Positive")
predictors <- unlist(x$options$explanatory)
check(setequal(predictors, c("grade", "age", "tumor_size")),
      "Binary pathology model terms")
fit <- stats::glm(stats::reformulate(predictors, ".reference_positive"),
                  data = data, family = stats::binomial())
check(isTRUE(fit$converged), "Independent binary logistic model converged")
odds_columns <- Filter(function(col) grepl("OR (", col$title, fixed = TRUE),
                       x$elements$fittedTable$table$columns)
displayed_or <- function(kind) {
  cols <- Filter(function(col) grepl(kind, col$title, fixed = TRUE), odds_columns)
  check(length(cols) == 1L, paste("Binary pathology", kind, "OR column"))
  values <- suppressWarnings(as.numeric(sub(" .*", "",
    column(x$elements$fittedTable, cols[[1]]$name))))
  sort(values[is.finite(values)])
}
expected_multivariable <- sort(exp(stats::coef(fit)[-1L]))
actual_multivariable <- displayed_or("multivariable")
check(length(actual_multivariable) == length(expected_multivariable) &&
        max(abs(actual_multivariable - expected_multivariable)) < 0.0051,
      "Independent multivariable logistic odds ratios")
expected_univariable <- sort(unlist(lapply(predictors, function(predictor) {
  fit <- stats::glm(stats::reformulate(predictor, ".reference_positive"),
                    data = data, family = stats::binomial())
  exp(stats::coef(fit)[-1L])
})))
actual_univariable <- displayed_or("univariable")
check(length(actual_univariable) == length(expected_univariable) &&
        max(abs(actual_univariable - expected_univariable)) < 0.0051,
      "Independent univariable logistic odds ratios")
positive_test <- as.character(data$diagnostic_test) == "Positive"
positive_outcome <- data$.reference_positive == 1L
counts <- c(TN = sum(!positive_test & !positive_outcome),
            FP = sum(positive_test & !positive_outcome),
            FN = sum(!positive_test & positive_outcome),
            TP = sum(positive_test & positive_outcome))
check(identical(counts, c(TN = 129L, FP = 20L, FN = 12L, TP = 79L)),
      "Independent binary diagnostic counts")
check(identical(column(x$elements$diagnosticContingency, "column1"),
                c("Positive", "Negative")) &&
        identical(as.numeric(column(x$elements$diagnosticContingency, "column2")),
                  as.numeric(counts[c("TP", "FN")])) &&
        identical(as.numeric(column(x$elements$diagnosticContingency, "column3")),
                  as.numeric(counts[c("FP", "TN")])),
      "Displayed binary diagnostic contingency counts")
cat("PASS:", checks, "saved-example checks.\n")
