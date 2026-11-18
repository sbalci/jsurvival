# Reproduce the data-only starting workbook for a fixed binary pathology endpoint.
# Run from the jsurvival repository root, then configure and save in jamovi.
# Refuse to replace an existing workbook containing a saved teaching analysis.
path <- file.path("data", "oddsratio_binary_example.omv")
if (file.exists(path)) stop("Refusing to overwrite: ", path)

set.seed(20261118)
n <- 240L
patient_id <- sprintf("SYN%03d", seq_len(n))
age <- round(pmin(85, pmax(35, rnorm(n, mean = 61, sd = 11))), 1)
tumor_size <- round(pmin(80, pmax(5, rgamma(n, shape = 3, scale = 9) + 5)), 1)
grade <- factor(sample(c("I", "II", "III"), n, replace = TRUE,
                       prob = c(.25, .45, .30)), levels = c("I", "II", "III"))

# These invented associations produce varied, plausible teaching records.
# They are simulation parameters, not published clinical effect estimates.
log_odds <- -1.4 + .04 * (tumor_size - 25) + .012 * (age - 60) +
  .55 * (grade == "II") + 1.1 * (grade == "III")
nodal_positive <- rbinom(n, size = 1, prob = plogis(log_odds)) == 1
nodal_status <- factor(ifelse(nodal_positive, "Positive", "Negative"),
                       levels = c("Negative", "Positive"))

# Generating targets only: sensitivity .82 and specificity .85.
# Observed sample estimates are calculated from the generated records below.
test_positive <- rbinom(n, size = 1,
                       prob = ifelse(nodal_positive, .82, 1 - .85)) == 1
diagnostic_test <- factor(ifelse(test_positive, "Positive", "Negative"),
                         levels = c("Negative", "Positive"))

cohort <- data.frame(patient_id, nodal_status, diagnostic_test, age, tumor_size, grade)
stopifnot(nrow(cohort) == n, !anyNA(cohort), !anyDuplicated(cohort$patient_id),
          all(cohort$age >= 35 & cohort$age <= 85),
          all(cohort$tumor_size >= 5 & cohort$tumor_size <= 80))
jmvReadWrite::write_omv(dtaFrm = cohort, fleOut = path, frcWrt = FALSE)

cat("Synthetic pathology cohort:", n, "independent patients; age in years; size in mm.\n")
print(table(nodal_status, diagnostic_test))
cat("These counts describe invented data and do not validate a diagnostic test.\n")
