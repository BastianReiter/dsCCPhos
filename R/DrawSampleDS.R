
#' DrawSampleDS
#'
#' Draws a sample (subset) from Raw Data Set
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param SampleSize \code{string} - Number of patients in sample
#'
#' @return A \code{list} containing a subset of Raw Data Set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DrawSampleDS <- function(RawDataSetName.S = "RawDataSet",
                         SampleSize.S = "100")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(RawDataSetName.S) & is.character(SampleSize.S))
{
    RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
    SampleSize <- as.integer(SampleSize.S)
}
else
{
    ClientMessage <- "ERROR: 'RawDataSetName.S' and 'SampleSize.S' must be specified as character strings"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(purrr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample PatientIDs and subset RDS tables accordingly
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AllPatientIDs <- RawDataSet$RDS_Patient$"_id"
#AllPatientIDs <- RawDataSet$RDS_Patient$PatientID
AvailableNumberPatients <- length(unique(AllPatientIDs))

if (SampleSize > AvailableNumberPatients) { SampleSize <- AvailableNumberPatients }


# Get a random sample of PatientIDs
SampleIDs <- sample(AllPatientIDs,
                    size = SampleSize)

# Subset RDS tables with sampled PatientIDs
RawDataSetSample <- RawDataSet %>%
                        imap(function(Table, tablename)
                             {
                                if (length(Table) > 0 && !is.null(Table) && !is_empty(Table) && nrow(Table) > 0)
                                {
                                    if (tablename == "RDS_Patient") { return(filter(Table, Table$'_id' %in% SampleIDs)) }
                                    else if (tablename %in% c("RDS_GeneralCondition", "RDS_OtherClassification", "RDS_TherapyRecommendation")) { return(filter(Table, Table$PatientID %in% SampleIDs)) }
                                    else { return(filter(Table, Table$'patient-id' %in% SampleIDs)) }
                                }
                                else { return(NULL) }
                             })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return sample of RDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(RawDataSetSample)

}
