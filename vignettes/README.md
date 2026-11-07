Clinician Quickstart for jSurvival Vignettes
===========================================

Audience: Statistics‑naive clinicians (pathology/oncology) using jamovi.

Recommended learning path
-------------------------
- Getting started: 01-introduction-user-friendly.Rmd
- One complete workflow: 05-jamovi-workflow-user-friendly.Rmd
- Comparing groups: 14-comparing-survival-user-friendly.Rmd
- Competing risks: 15-competing-survival-user-friendly.Rmd
- Cox regression, briefly: 18-cox-brief-for-clinicians.Rmd

What you’ll learn
-----------------
- Basic concepts: time-to-event, censoring, Kaplan–Meier (KM) plots
- Group comparisons: log-rank p-values, median survival, pairwise tests
- Competing risks: when and how to use CIF plots
- Cox models: hazard ratios, assumptions, and simple checks

Pick the right analysis
-----------------------
- Single-arm KM: Describe overall survival over time
- Comparing survival: Compare groups (e.g., treatment, biomarker)
- Competing risks: Multiple causes/events can preclude each other
- Cox regression: Adjust for multiple factors (age, grade, stage)

Key terms (plain language)
--------------------------
- Time-to-event: Follow-up time until an outcome (or last contact)
- Censoring: Patient didn’t have the event during observed follow-up
- Kaplan–Meier curve: Probability of remaining event-free over time
- Log-rank test: Are KM curves different beyond chance?
- Median survival: Time at which 50% have had the event
- Hazard ratio (HR): Ratio of hazards at any moment (HR < 1 is a lower hazard); an association, not a cause
- Proportional hazards: HR stays roughly constant over time
- Cumulative incidence (CIF): Probability an event has happened by a time accounting for competing risks

Common pitfalls to avoid
------------------------
- Mismatched coding: Confirm that the Event Level you selected is the event
- Time unit confusion: Keep time in consistent units (months/days)
- Missing dates: Check for impossible/negative times when deriving time
- Multiple groups: Use pairwise comparisons after overall log-rank
- Non-PH in Cox: Check assumptions; consider stratification or alternatives

jamovi navigation (at a glance)
-------------------------------
- One group: Survival > ClinicoPath Survival > Single Arm Survival
- Comparing groups: Survival > ClinicoPath Survival > Survival Analysis
- Competing risks: Single Arm Survival or Survival Analysis, under Analysis with Multiple Outcomes
- Cox model and proportional hazards check: Survival > ClinicoPath Survival > Multivariable Survival Analysis
