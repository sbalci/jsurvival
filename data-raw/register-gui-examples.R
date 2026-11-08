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
registration$document_module_omv(normalizePath("."), "jsurvival")

catalog <- yaml::read_yaml("jamovi/0000.yaml")$datasets
for (entry in metadata) {
  matches <- Filter(function(x) identical(x$path, entry$path), catalog)
  stopifnot(length(matches) == 1L, identical(matches[[1]], entry))
}
message("Registered and verified ", length(metadata), " GUI examples.")
