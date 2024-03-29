% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AugmentDataDS.R
\name{AugmentDataDS}
\alias{AugmentDataDS}
\title{AugmentDataDS}
\usage{
AugmentDataDS(CuratedDataSetName.S = "CuratedDataSet")
}
\arguments{
\item{CuratedDataSetName.S}{String | Name of the Curated Data Set object on server | Default: 'CuratedDataSet'}
}
\value{
A list containing the following objects:
\itemize{\item AugmentedDataSet (list)
\item AugmentationReport (list)
\item AugmentationMessages (list)}
}
\description{
Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
}
\details{
Server-side ASSIGN method
}
\author{
Bastian Reiter
}
