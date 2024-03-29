% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XMLToDataModel.R
\name{XMLToDataModel}
\alias{XMLToDataModel}
\title{XMLToDataModel}
\usage{
XMLToDataModel(
  XMLSchemaFilePath,
  TableNodeElement = "container",
  TableNameAttribute = "opal-table",
  FeatureNodeElement = "attribute",
  FeatureNameAttribute = "csv-column",
  FeatureTypeAttribute = "opal-value-type"
)
}
\arguments{
\item{FeatureTypeAttribute}{}
}
\value{
A list of data frames
}
\description{
Make List of data frames describing a data model
}
\author{
Bastian Reiter
}
