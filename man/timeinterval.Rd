% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeinterval.h.R
\name{timeinterval}
\alias{timeinterval}
\title{Time Interval Calculator}
\usage{
timeinterval(
  data,
  dx_date,
  fu_date,
  time_format = "ymd",
  output_unit = "months",
  use_landmark = FALSE,
  landmark_time = 3,
  add_times = TRUE
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{dx_date}{Column containing start dates}

\item{fu_date}{Column containing end dates}

\item{time_format}{.}

\item{output_unit}{.}

\item{use_landmark}{.}

\item{landmark_time}{.}

\item{add_times}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$personTimeInfo} \tab \tab \tab \tab \tab a html \cr
\code{results$summary} \tab \tab \tab \tab \tab a html \cr
\code{results$calculated_time} \tab \tab \tab \tab \tab an output \cr
}
}
\description{
Calculates time intervals between dates or from pre-calculated values. Time
intervals form the basis of person-time follow-up calculations in survival
analysis. Person-time follows the accumulated time that participants
contribute to a study while being at risk for the event of interest,
accounting for varying entry times and censoring. Person-time is typically
expressed in person-days, person-months,  or person-years depending on the
study duration and outcome frequency.
}
\examples{
# Calculate time intervals for person-time analysis:
# timeinterval(
#   data = study_data,
#   dx_date = "diagnosis_date",
#   fu_date = "followup_date",
#   output_unit = "months"
# )

}
