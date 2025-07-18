% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{calculate_auc}
\alias{calculate_auc}
\title{Approximate AUC from sensitivity and specificity}
\usage{
calculate_auc(sens, spec)
}
\arguments{
\item{sens}{Sensitivity of the test}

\item{spec}{Specificity of the test}
}
\value{
Numeric AUC value or NA when inputs are not valid
}
\description{
Uses a simplified formula to approximate AUC from sensitivity and specificity
}
