# Read-only qualification of GUI-saved files. Run from the jsurvival root.
suppressPackageStartupMessages(library(RProtoBuf))
readProtoFiles(system.file("jamovi.proto", package = "jmvcore"))
metadata <- yaml::read_yaml("data-raw/gui-examples.yaml")$datasets
catalog <- yaml::read_yaml("jamovi/0000.yaml")$datasets
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
event_code <- function(x) if (is.logical(x)) as.numeric(x) else as.numeric(as.character(x))
for (entry in metadata) {
  file <- file.path("data", entry$path)
  name <- sub("_example[.]omv$", "", entry$path)
  listing <- unzip(file, list = TRUE)
  streams <- grep("/analysis$", listing$Name, value = TRUE)
  analyses <- lapply(streams, function(stream) {
    connection <- unz(file, stream, "rb")
    on.exit(close(connection))
    read(P("jamovi.coms.AnalysisResponse"), readBin(connection, "raw", 1e7))
  })
  analyses <- Filter(function(x) x$name != "empty", analyses)
  check(length(analyses) == 1L, paste(name, "analysis count"))
  analysis <- analyses[[1]]
  check(analysis$name == name && analysis$ns == "jsurvival", paste(name, "identity"))
  check(analysis$status == 3L, paste(name, "completed status"))
  elements <- flatten(analysis$results)
  check(all(vapply(elements, function(x) !nzchar(x$error$message), logical(1))),
        paste(name, "result errors"))
  images <- Filter(function(x) x$has("image") && nzchar(x$image$path), elements)
  check(all(vapply(images, function(x) x$image$path %in% listing$Name, logical(1))),
        paste(name, "image resources"))
  check(identical(Filter(function(x) identical(x$path, entry$path), catalog), list(entry)),
        paste(name, "catalog metadata"))
  data <- if (name == "outcomeorganizer") read_integer_output_example(file) else {
    jmvReadWrite::read_omv(file)
  }
  check(nrow(data) == if (name == "datetimeconverter") 8L else 150L,
        paste(name, "row count"))
  options <- lapply(analysis$options$options, decode)
  names(options) <- unlist(analysis$options$names)
  saved[[name]] <- list(data = data, elements = elements, options = options)
  cat(entry$path, ": complete;", length(images), "cached images\n")
}

for (name in c("singlearm", "survival", "survivalcont", "multisurvival", "lassocox")) {
  x <- saved[[name]]
  check(all(is.finite(x$data$time) & x$data$time > 0), paste(name, "positive time"))
  check(sum(event_code(x$data$event) == 1) == 106L, paste(name, "event count"))
  check(x$options$outcomeLevel == "1", paste(name, "event coding"))
}
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
x <- saved$outcomeorganizer
check(identical(as.numeric(as.character(x$data[["Recoded Outcome (overall survival)"]])),
                as.numeric(as.character(x$data$status) %in% c("DOD", "DOOC"))),
      "Independent overall-survival mapping")
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
x <- saved$oddsratio
check(x$options$usePenalized && x$options$showNomogram, "Firth safeguard requested")
check(grepl("not generated", x$elements$nomogram$html$content, fixed = TRUE),
      "Firth nomogram safeguard displayed")
cat("PASS:", checks, "saved-example checks.\n")
