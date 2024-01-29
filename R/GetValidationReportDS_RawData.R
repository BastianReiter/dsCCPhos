
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
require(validate)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract data frames from list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Temporarily save table names for later use
vc_TableNames <- names(RawDataSet)

# Transform feature names for easier handling in later functions
RawDataSet <- purrr::map(.x = names(RawDataSet),
                         .f = function(TableName)
                              {
                                  # Create named vector to look up matching feature names in meta data
                                  vc_Lookup <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Raw
                                  names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Curated

                                  # Rename feature names according to look-up vector
                                  dplyr::rename(RawDataSet[[TableName]], any_of(vc_Lookup))      # Returns a tibble
                               })

# Re-assign table names in Raw Data Set
names(RawDataSet) <- vc_TableNames


df_RDS_BioSampling <- RawDataSet$BioSampling
df_RDS_Diagnosis <- RawDataSet$Diagnosis
df_RDS_Histology <- RawDataSet$Histology
df_RDS_Metastasis <- RawDataSet$Metastasis
df_RDS_MolecularDiagnostics <- RawDataSet$MolecularDiagnostics
df_RDS_Patient <- RawDataSet$Patient
df_RDS_Progress <- RawDataSet$Progress
df_RDS_RadiationTherapy <- RawDataSet$RadiationTherapy
df_RDS_Staging <- RawDataSet$Staging
df_RDS_Surgery <- RawDataSet$Surgery
df_RDS_SystemicTherapy <- RawDataSet$SystemicTherapy


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
