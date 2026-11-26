# Attained age as the Cox time scale

[Request from Tonje in ClinicoPathJamoviModule #122](https://github.com/sbalci/ClinicoPathJamoviModule/issues/122#issuecomment-7194661782).

## What works in jSurvival 1.1.5

For one predictor, use **Survival Analysis → Statistical Analysis → Age Adjustment**.
Enable **Age-adjusted Cox regression** and **Age as time scale (epidemiologic)**.
Choose the **Age Variable (Continuous)** in years at entry and set **Time Type in
Output** to the follow-up unit. Read the separate attained-age Cox table, not
the ordinary follow-up-time table. Using age as the time scale is not the
same as including age as an ordinary covariate.

Multivariable Survival Analysis does not currently accept delayed entry. For
multiple predictors, the current workaround in R is:

```r
survival::coxph(
    survival::Surv(age_at_entry, age_at_exit, event) ~ exposure + covariate,
    data = dat,
    ties = "efron"
)
```

Both ages are in years, with exit at the event or censoring. Use one row per
participant and explicitly code the outcome as 1 for event and 0 for censoring.
A participant last observed at age 60 who entered at age 57 cannot contribute
to a risk set at age 55. See the [survival package manual](https://cran.r-project.org/web/packages/survival/refman/survival.html#Surv).

## Proposed jamovi integration

This draft adds a private fitting helper and regression cases, not a jamovi
control. For the first interface change, add an opt-in coefficient table
with explicit numeric entry and exit ages in years, the binary outcome, and
baseline predictors. Show sample and event counts, any exclusions and fit
warnings. Compare the actual jamovi table against the reference R result and
confirm a saved analysis with the option off produces unchanged results.
Do not reuse follow-up-time plots, absolute-risk estimates, landmarking, or
competing-risk results for this fit. Stratification, time-varying covariates,
repeated records and adjusted curves are outside this first slice.
