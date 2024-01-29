
#' ExcludeRawDataDS
#'
#' Exclude data from Raw Data Set (RDS)
#'
#' Server-side ASSIGN method
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#'
#' @return Modified Raw Data Set object (list)
#' @export
#'
#' @examples
#' @author Bastian Reiter
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


# Unpack list into data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


df_RDS_BioSampling <- df_RDS_BioSampling



}
