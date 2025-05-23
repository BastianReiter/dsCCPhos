% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassifyDiagnosisAssociation.R
\name{ClassifyDiagnosisAssociation}
\alias{ClassifyDiagnosisAssociation}
\title{ClassifyDiagnosisAssociation}
\usage{
ClassifyDiagnosisAssociation(
  DiagnosisEntries,
  RuleCalls,
  ProgressBarObject = NULL
)
}
\arguments{
\item{DiagnosisEntries}{\code{data.frame} containing multiple diagnosis entries}

\item{RuleCalls}{\code{list} - Unevaluated \code{dplyr::case_when}-Statements compiled from rules defined in \code{RuleSet_DiagnosisAssociation}}

\item{ProgressBarObject}{\code{progress::progress_bar} object - Optionally pass progress bar object to display progress}
}
\value{
\code{data.frame} enhanced with classified diagnosis associations
}
\description{
Auxiliary function within \code{\link{CurateDataDS}}
}
\details{
In patients with multiple diagnosis entries: Identify and classify associations between diagnosis entries.
}
\author{
Bastian Reiter
}
