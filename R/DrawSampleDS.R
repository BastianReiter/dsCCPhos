
#' DrawSampleDS
#'
#' Draws a sample (subset) from Raw Data Set
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param SampleSize.S \code{integer} - Number of patients in sample
#'
#' @return A \code{list} containing a subset of Raw Data Set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DrawSampleDS <- function(RawDataSetName.S = "RawDataSet",
                         SampleSize.S = 100)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(purrr)

  # --- Argument Assertions ---
  assert_that(is.string(RawDataSetName.S),
              is.count(SampleSize.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
  SampleSize <- SampleSize.S

#-------------------------------------------------------------------------------
# Sample PatientIDs and subset RDS tables accordingly
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
  return(RawDataSetSample)
}
