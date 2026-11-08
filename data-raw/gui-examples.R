# Seeded synthetic data ONLY. Configure and save analyses using the jamovi GUI.
# Run from the jsurvival root. Refuses to overwrite an existing saved example.
set.seed(11082026)
n <- 150L
group <- factor(sample(c("Control", "Treatment"), n, TRUE))
age <- round(runif(n, 40, 80), 1)
marker <- round(rnorm(n), 2)
stage <- factor(sample(c("I", "II", "III"), n, TRUE), levels = c("I", "II", "III"))
event_time <- rexp(n, .03 * exp(
  .65 * (group == "Treatment") + .025 * (age - 60) +
    .8 * marker + .3 * (as.integer(stage) - 1)
))
censor_time <- runif(n, 6, 60)
time <- round(pmin(event_time, censor_time), 3) + .001
event <- factor(as.integer(event_time <= censor_time), levels = 0:1)
cohort <- data.frame(time, event, group, age, marker, stage)
start_date <- as.Date("2017-01-01") + sample(0:365, n, TRUE)
end_date <- start_date + round(time * 30.4375)
status <- factor(
  ifelse(event == "1", ifelse(runif(n) < .75, "DOD", "DOOC"),
         ifelse(runif(n) < .5, "AWD", "AWOD")),
  levels = c("AWOD", "AWD", "DOD", "DOOC")
)
datasets <- list(
  datetimeconverter = data.frame(
    iso_date = c("2020-01-02", "2020-02-29", "2021-12-31", NA,
                 "2022-01-01", "2023-06-15", "2020-04-10", "2020-05-20"),
    mixed_date = c("2020-01-02", "not a date", "2021-12-31", NA,
                   "2022-01-01", "2023-06-15", "2020-04-10", "2020-05-20"),
    excel_date = c(43832, 43890, 44561, NA, 44562, 45092, 43931, 43971)
  ),
  timeinterval = data.frame(start_date = as.character(start_date),
                            end_date = as.character(end_date)),
  outcomeorganizer = data.frame(status, followup = time,
                                patient_id = sprintf("SIM%03d", seq_len(n))),
  singlearm = cohort[c("time", "event")],
  survival = cohort,
  survivalcont = cohort[c("time", "event", "marker", "age")],
  multisurvival = cohort,
  lassocox = cohort,
  oddsratio = cohort[c("event", "group", "age", "marker", "stage")]
)
for (fn in names(datasets)) {
  path <- file.path("data", paste0(fn, "_example.omv"))
  if (file.exists(path)) stop("Refusing to overwrite: ", path)
  jmvReadWrite::write_omv(dtaFrm = datasets[[fn]], fleOut = path, frcWrt = FALSE)
}
