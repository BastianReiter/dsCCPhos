
#' GetOutcomeMeasuresDS
#'
#' Calculate clinical outcome measures (COM) from Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param Name_AugmentationOutput String | Name of the list object created by AugmentDataDS() | Default: 'AugmentationOutput'
#'
#' @return ABC
#' @export
#'
#' @author Bastian Reiter
GetOutcomeMeasuresDS <- function(Name_AugmentationOutput = "AugmentationOutput")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(Name_RawDataSet))
{
    AugmentationOutput <- eval(parse(text = Name_AugmentationOutput), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'Name_AugmentationOutput' must be specified as a character string"
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
