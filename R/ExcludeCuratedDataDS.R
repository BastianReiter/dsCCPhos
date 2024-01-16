
#' ExcludeCuratedDataDS
#'
#' Exclude data from Curated Data Set
#'
#' Server-side ASSIGN method
#'
#' @param Name_CurationOutput String | Name of the list object created by CurateDataDS() | Default: 'CurationOutput'
#'
#' @return The Curation Output with modified Curated Data Set
#' @export
#'
#' @examples
ExcludeCuratedDataDS <- function(Name_RawDataSet = "RawDataSet")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(Name_RawDataSet))
{
    RawDataSet <- eval(parse(text = Name_RawDataSet), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'Name_RawDataSet' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(lubridate)
require(purrr)
require(stringr)

}
