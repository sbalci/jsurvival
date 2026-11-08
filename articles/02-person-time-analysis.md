# Person-Time Analysis in jSurvival Module

## Introduction

Person-time analysis is a fundamental concept in epidemiology and
survival analysis that accounts for the varying observation periods of
study participants. The jSurvival module provides person-time
calculations and incidence rates across its survival analysis functions.

### What is Person-Time?

**Person-time** represents the total observation time contributed by all
participants in a study while being at risk for the event of interest.
Unlike simple participant counts, person-time captures both the number
of subjects and their observation duration, providing accurate
denominators for rate calculations.

#### Key Concepts

- **Total Person-Time**: Sum of all individual follow-up periods
- **Incidence Rate**: Number of events ÷ Total person-time  
- **Time Units**: Typically expressed as person-days, person-months, or
  person-years
- **Censoring**: Counts observed exposure up to the endpoint or
  censoring; informative loss to follow-up can still bias comparisons
- **Varying Follow-up**: Handles different observation periods for each
  participant

#### Why Person-Time Matters

1.  **Accurate Rate Calculations**: Provides proper denominators for
    incidence rates
2.  **Comparison Across Studies**: Expresses counts on a common exposure
    scale; differences in case mix, follow-up period and ascertainment
    still require assessment
3.  **Adjustment for Time**: Accounts for varying exposure periods in
    survival analysis

## Person-Time in jSurvival Modules

The jSurvival module includes person-time analysis in five functions:

### 1. Comprehensive Time Interval Calculator

**Purpose**: Calculate time intervals that form the basis of person-time
analysis

**Features**: - Date-based time calculations with multiple formats -
Person-time concept explanation - Total person-time calculation -
Landmark analysis support

**Usage**:

    Analyses → Survival → Data Preparation → Comprehensive Time Interval Calculator

**Key Outputs**: - Time interval summary with total person-time -
Educational information about person-time concepts - Calculated time
variables for further analysis

### 2. Single Arm Survival

**Purpose**: Analyze survival for a single cohort with person-time
metrics

**Features**: - Overall incidence rate calculation - Time-stratified
person-time analysis - Confidence intervals using Poisson exact
methods - Rate multiplier options (per 100, 1000 person-time units)

**Usage**:

    Analyses → Survival → ClinicoPath Survival → Single Arm Survival
    Enable: "Person-time metrics"

**Key Outputs**: - Person-time analysis table with rates and confidence
intervals - Time-interval stratified analysis - Summary with
interpretation

### 3. Survival Analysis (Univariate)

**Purpose**: Compare survival between groups with group-specific
person-time analysis

**Features**: - Group-specific person-time calculations - Incidence
rates per group, shown side by side - Time-stratified analysis

**Usage**:

    Analyses → Survival → ClinicoPath Survival → Survival Analysis
    Enable: "Person-time rates"

**Key Outputs**: - Person-time table by group - Group-specific incidence
rates - Confidence intervals for each group - Overall and stratified
summaries

### 4. Multivariable Survival

**Purpose**: Report person-time rates alongside a model with multiple
covariates. The rates are crude, not adjusted by the model

**Features**: - Overall person-time, and crude rates per covariate
level - Person-time summary with multiple predictors

**Usage**:

    Analyses → Survival → ClinicoPath Survival → Multivariable Survival Analysis
    Enable: "Person-time metrics"

**Key Outputs**: - Overall person-time summary - Person-time and crude
rates per covariate level - Time-interval stratified rates

### 5. Survival Analysis for Continuous Explanatory Variable

**Purpose**: Report person-time rates alongside the analysis of a
continuous predictor

**Usage**:

    Analyses → Survival → ClinicoPath Survival → Survival Analysis for Continuous Explanatory Variable
    Enable: "Person-time rates (events per time unit)"

## Getting Started with Person-Time Analysis

### Step 1: Prepare Your Data

Your dataset should include: - **Time variable**: Follow-up duration
(numeric) - **Event variable**: Event indicator, in any coding; you
select the Event Level - **Group variables**: For comparisons
(optional) - **Date variables**: For time calculation (optional)

Example data structure:

    patient_id | followup_months | event | treatment_group | diagnosis_date | last_contact
    PT0001     | 24.5           | 1     | Standard       | 2020-01-15     | 2022-01-30
    PT0002     | 36.2           | 0     | Experimental   | 2020-02-10     | 2023-02-15

### Step 2: Choose the Appropriate Analysis

**Comprehensive Time Interval Calculator**: If you need to calculate
follow-up times from dates - Input: Start and end dates - Output: Time
intervals and person-time concepts

**Single Arm Survival**: For overall cohort analysis - Input: Time,
event variables - Output: Overall incidence rates and time-stratified
analysis

**Survival Analysis**: For group comparisons - Input: Time, event,
grouping variables - Output: Group-specific rates

**Multivariable Survival**: For a Cox model with multiple covariates -
Input: Time, event, multiple covariates - Output: Crude rates per
covariate level, alongside the Cox model

### Step 3: Configure Person-Time Options

1.  **Enable person-time analysis**: Tick the person-time option named
    above for your analysis
2.  **Set time intervals**: Specify intervals for stratified analysis
    (e.g., “12, 36, 60”)
3.  **Choose rate multiplier**: Enter 100 or 1000 for rate presentation
4.  **Configure time units**: Ensure consistent units throughout

### Step 4: Interpret Results

#### Person-Time Table Columns

- **Time Interval**: Period analyzed (e.g., 0-12 months, 12-36 months)
- **Events**: Number of events in the interval
- **Person-Time**: Total observation time in the interval
- **Incidence Rate**: Events per person-time unit
- **Lower** and **Upper**: 95% confidence interval for the rate (Poisson
  exact)

#### Key Metrics to Report

1.  **Total Person-Time**: Overall observation time
2.  **Overall Incidence Rate**: Events per 100 person-time units
3.  **Time-Specific Rates**: Rates in different follow-up periods
4.  **Group Rates**: The rate in each group
5.  **Confidence Intervals**: Precision of rate estimates

## Clinical Applications

The counts in these examples are made up to show the arithmetic. The
module reports rates and their confidence intervals; it does not report
rate ratios or differences, so compute those by hand.

### Example 1: Cancer Survival Study

**Scenario**: Comparing survival between two treatment groups

**Analysis Setup**: - Module: Survival Analysis - Variables:
treatment_group, survival_months, death_status - Enable: Person-time
metrics

**Expected Results**: - Person-time by treatment group - Group-specific
incidence rates - Time-stratified analysis

**Interpretation**:

    Standard Treatment: 45 deaths in 2,400 person-months (1.88 per 100 person-months)
    New Treatment: 32 deaths in 2,200 person-months (1.45 per 100 person-months)
    Rate Ratio, by hand: 0.78 (22% lower death rate)

### Example 2: Epidemiological Cohort Study

**Scenario**: Disease incidence in exposed vs. unexposed populations

**Analysis Setup**: - Module: Survival Analysis  
- Variables: exposure_status, followup_years, disease_onset - Enable:
Person-time metrics

**Expected Results**: - Exposure-specific person-time - Incidence rates
by exposure - Time trends in incidence

**Interpretation**:

    Exposed Group: 15 cases in 850 person-years (1.76 per 100 person-years)
    Unexposed Group: 8 cases in 920 person-years (0.87 per 100 person-years)
    Rate Difference, by hand: 0.90 per 100 person-years
    Rate Ratio, by hand: 2.03

### Example 3: Drug Safety Monitoring

**Scenario**: Adverse event rates during drug therapy

**Analysis Setup**: - Module: Single Arm Survival - Variables:
treatment_duration, adverse_event - Enable: Person-time metrics with
time stratification

**Expected Results**: - Overall adverse event rate - Time-stratified
rates (early vs. late) - Confidence intervals for safety assessment

**Interpretation**:

    Overall Rate: 12 events in 1,500 person-months (0.80 per 100 person-months)
    0-6 months: 8 events in 600 person-months (1.33 per 100 person-months)
    6+ months: 4 events in 900 person-months (0.44 per 100 person-months)
    Conclusion: Higher event rate in early treatment period

## Advanced Features

### Time-Stratified Analysis

Person-time analysis can be stratified by time intervals to detect: -
Changing hazards over time - Early vs. late event patterns

**Setup**: Specify intervals in “Time Interval Stratification” option

    Example: "6, 12, 24" creates intervals: 0-6, 6-12, 12-24 months and the remaining follow-up

### Rate Multipliers

Choose appropriate multipliers based on event frequency: - **Rate per
100 person-time**: Common for moderate event rates - **Rate per 1000
person-time**: Useful for rare events - **Rate per 10,000 person-time**:
For very rare events

### Confidence Intervals

Person-time rates use Poisson exact confidence intervals: - **Exact
method**: Based on Poisson distribution of event counts - **Coverage**:
Exact count limits under a Poisson model; they do not correct
informative censoring, confounding or dependence between patients -
**Asymmetric intervals**: Reflects the nature of count data

### Integration with Survival Analysis

Person-time analysis complements traditional survival methods: -
**Kaplan-Meier curves**: Show survival probability over time - **Cox
regression**: Models hazard rates (instantaneous person-time rates) -
**Log-rank tests**: Compare survival distributions - **Person-time
rates**: Provide absolute event rates

## Quality Assurance

### Data Validation

Before person-time analysis:

1.  **Check time variables**: Ensure positive, numeric values
2.  **Validate event coding**: Confirm the Event Level
3.  **Review missingness**: Address missing time or event data
4.  **Assess outliers**: Investigate extremely long or short follow-up
    times

### Results Interpretation

When reviewing person-time results:

1.  **Biological plausibility**: Do rates make clinical sense?
2.  **Confidence intervals**: Are they appropriately wide for the data?
3.  **Time trends**: Do time-stratified rates show expected patterns?
4.  **Group differences**: Are rate differences clinically meaningful?

### Common Issues

#### Issue 1: Zero Events in Intervals

**Problem**: Some time intervals have no events **Solution**: Retain
prespecified intervals and report the zero rate with its upper
confidence limit. A zero count does not establish zero hazard; no rate
is defined if exposure is also zero.

#### Issue 2: Very Unequal Follow-up

**Problem**: Large variation in follow-up times **Solution**: Use
time-stratified analysis and report median follow-up

#### Issue 3: Competing Risks

**Problem**: Multiple event types affecting person-time **Note**:
jSurvival has no event-specific person-time. Use cumulative incidence in
competing-risk mode for these data

## Reporting Person-Time Results

### Methods Section

    Person-time analysis was performed to calculate incidence rates accounting 
    for varying follow-up periods. Total person-time was calculated as the sum 
    of individual follow-up periods from [entry criteria] to [endpoint definition]. 
    Incidence rates were calculated as the number of events divided by person-time 
    at risk, expressed per [time unit]. Confidence intervals were calculated using 
    Poisson exact methods. Time-stratified analysis was performed using intervals 
    of [specify intervals] to assess for time-varying hazards.

### Results Section

    A total of [N] participants contributed [total person-time] [time units] of 
    follow-up (median [median], range [range]). [Number] events occurred during 
    the observation period, yielding an overall incidence rate of [rate] per 
    [multiplier] [time units] (95% CI: [CI]). Incidence rates were [rate 1] in 
    [group 1] and [rate 2] in [group 2] per [multiplier] [time units].

### Tables and Figures

#### Table: Person-Time Analysis Results

A layout example with made-up counts:

    Group           N    Events   Person-Time   Rate/100 PT   95% CI
    Control        150    25       4,200        0.60         0.39-0.88
    Treatment      145    18       4,150        0.43         0.26-0.68
    Total          295    43       8,350        0.51         0.37-0.69

#### Figure: Incidence Rates by Time Period

- Forest plot showing rates and confidence intervals
- Time-stratified analysis results
- Group comparisons over time

## Conclusion

Person-time analysis gives event rates that account for varying
follow-up. The jSurvival module:

1.  **Explains** person-time concepts
2.  **Calculates rates** with exact confidence intervals
3.  **Reports rates per group**, for crude comparison by hand
4.  **Supports time-stratified analysis** for detecting temporal
    patterns

By incorporating person-time analysis into your survival studies, you
can: - Provide more accurate and interpretable results - Put study rates
on a common exposure scale while assessing comparability of
populations - Better understand temporal patterns in your data - Meet
epidemiological standards for rate-based analysis

For questions about person-time analysis, see the [jsurvival
website](https://www.serdarbalci.com/jsurvival/).
