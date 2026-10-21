# Outcome Organizer for Survival Analysis

Advanced tool for preparing outcome variables for various types of
survival analysis including overall survival, cause-specific, competing
risks, progression-free survival, and multistate models.

## Usage

``` r
outcomeorganizer(
  data,
  outcome = NULL,
  outcomeLevel,
  recurrence = NULL,
  recurrenceLevel,
  patientID = NULL,
  followupTime = NULL,
  analysistype = "os",
  multievent = FALSE,
  dod,
  dooc,
  awd,
  awod,
  useHierarchy = FALSE,
  eventPriority = 1,
  intervalCensoring = FALSE,
  intervalStart = NULL,
  intervalEnd = NULL,
  adminCensoring = FALSE,
  adminDate = NULL,
  outputTable = FALSE,
  diagnostics = FALSE,
  visualization = FALSE,
  showNaturalSummary = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  The primary outcome variable to be recoded for survival analysis
  (e.g., vital status).

- outcomeLevel:

  The level of the outcome variable that represents the event of
  interest (e.g., "Death", "Relapse"). Required for a factor outcome,
  and never taken from the order of the levels: without it the analysis
  stops and lists the levels. A numeric 0/1 outcome is read as 1 =
  event.

- recurrence:

  Variable indicating disease recurrence or progression (for RFS/PFS/DFS
  analyses).

- recurrenceLevel:

  The level indicating recurrence or progression has occurred. Required
  when a recurrence/progression variable is used, and never taken from
  the order of the levels: without it the analysis stops and asks for
  it.

- patientID:

  Patient identifier for handling multiple records or applying event
  hierarchies.

- followupTime:

  Optional follow-up time. Supplying it lets administrative censoring
  actually truncate follow-up and reset event status, and lets the event
  hierarchy keep the earliest priority event per patient instead of
  writing a later one back onto earlier records. Leave empty to keep the
  previous behaviour.

- analysistype:

  The type of survival analysis to prepare the outcome for.

- multievent:

  If true, allows for multiple event types (e.g., death from disease vs
  death from other causes).

- dod:

  The level representing death from the disease of interest.

- dooc:

  The level representing death from causes other than the disease of
  interest.

- awd:

  The level representing patients who are alive but have the disease.

- awod:

  The level representing patients who are alive and disease-free.

- useHierarchy:

  If true, applies a hierarchy when multiple events occur for the same
  patient.

- eventPriority:

  The event code (1, 2, ...) that takes precedence when a patient has
  several records. Minimum 1: code 0 is the censored/baseline code, and
  giving it priority would make "censored" outrank every event and
  silently erase them all. If the code entered here never occurs in the
  recoded outcome the hierarchy cannot act, and the analysis says so
  rather than reporting that it was applied.

- intervalCensoring:

  If true, prepares data for interval-censored analysis where exact
  event times are unknown.

- intervalStart:

  Variable containing the start of the interval when the event might
  have occurred.

- intervalEnd:

  Variable containing the end of the interval when the event might have
  occurred.

- adminCensoring:

  If true, applies administrative censoring at a specified date.

- adminDate:

  Variable containing the administrative censoring date.

- outputTable:

  If true, displays a table showing the frequency of each recoded
  outcome value.

- diagnostics:

  If true, displays diagnostic information about the recoding process.

- visualization:

  If true, displays a visualization of the distribution of recoded
  outcomes.

- showNaturalSummary:

  Display a plain-language summary of the recoding suitable for copying
  to reports.

- showGlossary:

  Display definitions of survival analysis terms.

## Value

A results object containing:

|                            |     |     |     |     |           |
|----------------------------|-----|-----|-----|-----|-----------|
| `results$eventRecodeInfo`  |     |     |     |     | a html    |
| `results$todo`             |     |     |     |     | a html    |
| `results$errors`           |     |     |     |     | a html    |
| `results$strongWarnings`   |     |     |     |     | a html    |
| `results$warnings`         |     |     |     |     | a html    |
| `results$infoMessages`     |     |     |     |     | a html    |
| `results$summary`          |     |     |     |     | a html    |
| `results$outputTable`      |     |     |     |     | a table   |
| `results$diagnosticsTable` |     |     |     |     | a table   |
| `results$outcomeViz`       |     |     |     |     | an image  |
| `results$naturalSummary`   |     |     |     |     | a html    |
| `results$glossary`         |     |     |     |     | a html    |
| `results$addOutcome`       |     |     |     |     | an output |
| `results$addAdminTime`     |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$outputTable$asDF`

`as.data.frame(results$outputTable)`
