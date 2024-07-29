
#' GetValidationReportDS_RawData
#'
#' Performs validation operations on Raw Data Set (RDS) and returns a report.
#'
#' Server-side AGGREGATE method
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#'
#' @return A list containing the validation report
#' @export
#'
#' @examples
#' @author Bastian Reiter
GetValidationReportDS_RawData <- function(Name_RawDataSet = "RawDataSet")
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
require(validate)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check existence and completeness of tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Go through available dataframes in RawDataSet and check for completeness
TableCheckPresent <- RawDataSet %>%
                          imap(function(dataframe, name)
                               {
                                  TableExists <- (!is_empty(dataframe))

                                  RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == name)$FeatureName_Raw
                                  PresentFeatureNames <- names(dataframe)

                                  MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

                                  TableComplete <- (length(MissingFeatures) == 0)

                                  return(list(TableExists = TableExists,
                                              TableComplete = TableComplete,
                                              MissingFeatures = MissingFeatures))
                               })

# Get all required table names from meta data
RequiredTableNames <- paste0("RDS_", dsCCPhosClient::Meta_TableNames$TableName_Curated)

# Compile summarizing list of table check
TableCheck <- RequiredTableNames %>%
                  map(function(tablename)
                      {
                          if (tablename %in% names(TableCheckPresent)) { return(TableCheckPresent[[tablename]]) }
                          else { return(list(TableExists = FALSE)) }
                      }) %>%
                  set_names(RequiredTableNames)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define validation rules using package "validate"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rules_BioSampling <- validator(#--- Data Types ---
                               is.character(SampleID),
                               is.character(PatientID),
                               is.Date(SampleTakingDate),
                               is.character(SampleType),
                               is.character(SampleStatus),
                               is.character(SampleProjectName),
                               is.numeric(SampleQuantity),
                               is.character(SampleUnit),
                               is.character(SampleAliquot),
                               #--- Missings ---
                               is.na(SampleID),
                               is.na(PatientID),
                               is.na(SampleTakingDate),
                               is.na(SampleType),
                               is.na(SampleStatus),
                               is.na(SampleProjectName),
                               is.na(SampleQuantity),
                               is.na(SampleUnit),
                               is.na(SampleAliquot)
                               #--- Formats ---
                                )


Output_BioSampling <- confront(df_RDS_BioSampling,
                               Rules_BioSampling)

Report_BioSampling <- summary(Output_BioSampling)

#Plot_BioSampling <- plot(Output_BioSampling)

return(Report_BioSampling)


}
