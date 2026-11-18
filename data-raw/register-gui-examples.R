# Run from the jsurvival repository root, after saving the examples in jamovi.
# Use the maintained distribution helper; do not rewrite generated analysis metadata.
metadata <- yaml::read_yaml("data-raw/gui-examples.yaml")$datasets
paths <- vapply(metadata, `[[`, character(1), "path")
stopifnot(!anyDuplicated(paths), all(file.exists(file.path("data", paths))))

helper <- "../ClinicoPathJamoviModule/_updateModules_utils.R"
if (!file.exists(helper)) stop("The sibling ClinicoPath distribution tooling is required.")
registration <- new.env(parent = globalenv())
sys.source(helper, envir = registration)
registration$.omv_entry <- function(omv) {
  entry <- metadata[match(omv, paths)]
  if (is.na(match(omv, paths))) stop("Missing example metadata: ", omv)
  lines <- strsplit(yaml::as.yaml(entry), "\n", fixed = TRUE)[[1]]
  paste0("  ", lines)
}
zero <- "jamovi/0000.yaml"
before <- yaml::read_yaml(zero)
registration$document_module_omv(normalizePath("."), "jsurvival")

# The distribution helper appends newly discovered workbooks. Refresh wording
# only for these maintained entries when their saved examples change. Preserve
# the generated analysis registry, other metadata and unowned dataset entries.
lines <- readLines(zero, warn = FALSE)
dataset_start <- grep("^datasets:", lines)
stopifnot(length(dataset_start) == 1L)
later_keys <- which(seq_along(lines) > dataset_start & grepl("^[A-Za-z]", lines))
dataset_end <- if (length(later_keys)) later_keys[[1]] else length(lines) + 1L
starts <- which(seq_along(lines) > dataset_start & seq_along(lines) < dataset_end &
                  grepl("^  - ", lines))
ends <- c(starts[-1] - 1L, dataset_end - 1L)
refreshed <- 0L
for (i in rev(seq_along(starts))) {
  block <- lines[starts[[i]]:ends[[i]]]
  entry <- yaml::yaml.load(paste(c("datasets:", block), collapse = "\n"))$datasets[[1]]
  index <- match(entry$path, paths)
  if (is.na(index) || identical(entry, metadata[[index]])) next
  replacement <- registration$.omv_entry(paths[[index]])
  lines <- c(lines[seq_len(starts[[i]] - 1L)], replacement,
             if (ends[[i]] < length(lines)) lines[(ends[[i]] + 1L):length(lines)])
  refreshed <- refreshed + 1L
}
if (refreshed) writeLines(lines, zero)

after <- yaml::read_yaml(zero)
stopifnot(identical(before$analyses, after$analyses))
unowned <- function(x) Filter(function(entry) !(entry$path %in% paths), x$datasets)
stopifnot(identical(unowned(before), unowned(after)))
before$datasets <- after$datasets <- NULL
stopifnot(identical(before, after))
catalog <- yaml::read_yaml(zero)$datasets
for (entry in metadata) {
  matches <- Filter(function(x) identical(x$path, entry$path), catalog)
  stopifnot(length(matches) == 1L, identical(matches[[1]], entry))
}
message("Registered and verified ", length(metadata), " GUI examples; refreshed ",
        refreshed, " maintained descriptions.")
