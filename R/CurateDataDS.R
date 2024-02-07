
#' CurateDataDS
#'
#' Takes Raw Data Set (RDS) and transforms it into Curated Data Set (CDS) while tracing transformation operations.
#'
#' Server-side ASSIGN method
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param RuleProfile_RawDataTransformation String | Profile name defining rule set to be used for data transformation. Profile name must be stated in \code{\link{RuleSet_RawDataTransformation}. | Default: 'Default'
#' @param RuleProfile_DiagnosisRedundancy String | Profile name defining rule set to be used for classification of diagnosis redundancies. Profile name must be stated in \code{\link{RuleSet_DiagnosisRedundancy}. | Default: 'Default'
#' @param RuleProfile_DiagnosisAssociation String | Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{\link{RuleSet_DiagnosisAssociation}. | Default: 'Default'
#'
#' @return A list containing two lists: The Curated Data Set (CDS) and a curation report.
#' @export
#'
#' @examples
#' @author Bastian Reiter
CurateDataDS <- function(Name_RawDataSet = "RawDataSet",
                         RuleProfile_RawDataTransformation = "Default",
                         RuleProfile_DiagnosisRedundancy = "Default",
                         RuleProfile_DiagnosisAssociation = "Default")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   SETUP
#     - Evaluation and parsing of input
#     - Loading of required package namespaces
#
#   MODUL 1)  Data Harmonization / Transformation
#     - Transform feature names
#     - Definition of features to monitor during value transformation
#     - Value transforming operations
#     - Compilation of curation report
#
#   MODUL 2)  Process diagnosis data
#     - Joining of df_CDS_Diagnosis and df_CDS_Histology
#     - Classification and removal of redundant diagnosis entries
#     - Classification and bundling of associated diagnosis entries
#     - Replace DiagnosisIDs in other tables
#
#
#   Return list containing Curated Data Set (CDS) and Curation Report


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
require(dsCCPhos)
require(lubridate)
require(progress)
require(purrr)
require(stringr)
require(tidyr)


# Suppress summarize info messages
options(dplyr.summarise.inform = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODUL 1)  Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set up progress bar
CountProgressItems <- 29
ProgressBar <- progress_bar$new(format = "Harmonizing data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_CuratedDataSet <- RawDataSet

ls_CuratedDataSet <- purrr::map(.x = names(RawDataSet),
                                .f = function(TableName)
                                     {
                                         # Create named vector to look up matching feature names in meta data ("OldName = 'NewName'")
                                         vc_Lookup <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Raw
                                         names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Curated

                                         # Rename feature names according to look-up vector
                                         dplyr::rename(RawDataSet[[TableName]], any_of(vc_Lookup))      # Returns a tibble
                                     })

# Re-set table names
names(ls_CuratedDataSet) <- names(RawDataSet)

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definition of features to monitor during Curation (Transformation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Object syntax: List of vectors
#     - Vector names = Name of feature to be monitored during Curation (Transformation)
#     - Vector values = Set of eligible values defined in Meta Data
# If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

ls_MonitorFeatures_BioSampling <- list(SampleType = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType")$Value_Curated,
                                       SampleAliquot = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleAliquot")$Value_Curated)


ls_MonitorFeatures_Diagnosis <- list(ICD10Version = NULL,
                                     LocalizationSide = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Diagnosis" & Feature == "LocalizationSide")$Value_Curated)


ls_MonitorFeatures_Histology <- list(Grading = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading")$Value_Curated)


ls_MonitorFeatures_Metastasis <- list(MetastasisLocalization = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Metastasis" & Feature == "MetastasisLocalization")$Value_Curated)


ls_MonitorFeatures_MolecularDiagnostics <- list()


ls_MonitorFeatures_Patient <- list(Gender = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "Gender")$Value_Curated,
                                   LastVitalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "LastVitalStatus")$Value_Curated)


ls_MonitorFeatures_Progress <- list(GlobalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "GlobalStatus")$Value_Curated,
                                    LocalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LocalStatus")$Value_Curated,
                                    LymphnodalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalStatus")$Value_Curated,
                                    MetastasisStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "MetastasisStatus")$Value_Curated)


ls_MonitorFeatures_RadiationTherapy <- list(RadiationTherapyRelationToSurgery = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyRelationToSurgery")$Value_Curated,
                                            RadiationTherapyIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyIntention")$Value_Curated)


ls_MonitorFeatures_Staging <- list(UICCStage = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "UICCStage")$Value_Curated,
                                   TNM_T = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T")$Value_Curated,
                                   TNM_N = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N")$Value_Curated,
                                   TNM_M = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M")$Value_Curated,
                                   TNM_T_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T_Prefix")$Value_Curated,
                                   TNM_N_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N_Prefix")$Value_Curated,
                                   TNM_M_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M_Prefix")$Value_Curated,
                                   TNM_ySymbol = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_ySymbol")$Value_Curated,
                                   TNM_rSymbol = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_rSymbol")$Value_Curated,
                                   TNM_mSymbol = NULL)


ls_MonitorFeatures_Surgery <- list(SurgeryIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "SurgeryIntention")$Value_Curated,
                                   ResidualAssessmentLocal = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal")$Value_Curated,
                                   ResidualAssessmentTotal = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal")$Value_Curated)


ls_MonitorFeatures_SystemicTherapy <- list(IsChemotherapy = NULL,
                                           IsImmunotherapy = NULL,
                                           IsHormoneTherapy = NULL,
                                           IsBoneMarrowTransplant = NULL,
                                           SystemicTherapyIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "SystemicTherapyIntention")$Value_Curated,
                                           SystemicTherapyRelationToSurgery = dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "SystemicTherapyRelationToSurgery")$Value_Curated)


# Put all objects in one list object to make them passable to functions (alphabetic order)
ls_MonitorFeatures_All <- list(ls_MonitorFeatures_BioSampling,
                               ls_MonitorFeatures_Diagnosis,
                               ls_MonitorFeatures_Histology,
                               ls_MonitorFeatures_Metastasis,
                               ls_MonitorFeatures_MolecularDiagnostics,
                               ls_MonitorFeatures_Patient,
                               ls_MonitorFeatures_Progress,
                               ls_MonitorFeatures_RadiationTherapy,
                               ls_MonitorFeatures_Staging,
                               ls_MonitorFeatures_Surgery,
                               ls_MonitorFeatures_SystemicTherapy)

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track feature values of raw data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_Monitors_Raw <- purrr::map2(.x = ls_CuratedDataSet,
                               .y = ls_MonitorFeatures_All,
                               .f = function(DataFrame, MonitorFeatures)
                                    {
                                       DataFrame %>%
                                           dsCCPhos::TrackFeatureValues(Features = MonitorFeatures,
                                                                        CurationStage = "Raw")
                                    })

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CURATION: Value transforming Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Order of operations in each raw data frame:
#   1) Harmonizing and correctional transformation of data values using dsCCPhos::TransformData()
#   2) Recoding data using dsCCPhos::RecodeData()
#         - dsCCPhos::RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#   3) Data formatting instructions


# Unpack list into data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_BioSampling <- ls_CuratedDataSet$BioSampling
df_CDS_Diagnosis <- ls_CuratedDataSet$Diagnosis
df_CDS_Histology <- ls_CuratedDataSet$Histology
df_CDS_Metastasis <- ls_CuratedDataSet$Metastasis
df_CDS_MolecularDiagnostics <- ls_CuratedDataSet$MolecularDiagnostics
df_CDS_Patient <- ls_CuratedDataSet$Patient
df_CDS_Progress <- ls_CuratedDataSet$Progress
df_CDS_RadiationTherapy <- ls_CuratedDataSet$RadiationTherapy
df_CDS_Staging <- ls_CuratedDataSet$Staging
df_CDS_Surgery <- ls_CuratedDataSet$Surgery
df_CDS_SystemicTherapy <- ls_CuratedDataSet$SystemicTherapy



# Transform df_CDS_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_BioSampling <- df_CDS_BioSampling %>%
                          #--- Transformation ----------------------------------
                          dsCCPhos::TransformData(TableName = "BioSampling", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                          #--- Recoding ----------------------------------------
                          mutate(SampleType = dsCCPhos::RecodeData(SampleType, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                                                    set_names(Value_Curated, Value_Raw))),      # This returns a vector of the form c("Value_Raw1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                                 SampleAliquot = dsCCPhos::RecodeData(SampleAliquot, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleAliquot"),
                                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                          #--- Formatting --------------------------------------
                          mutate(SampleTakingDate = format(as_datetime(SampleTakingDate), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        #--- Transformation ------------------------------------
                        dsCCPhos::TransformData(TableName = "Diagnosis", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                        #--- Recoding ------------------------------------------
                        mutate(LocalizationSide = dsCCPhos::RecodeData(LocalizationSide, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Diagnosis" & Feature == "LocalizationSide"),
                                                                                              set_names(Value_Curated, Value_Raw)))) %>%
                        #--- Formatting ----------------------------------------
                        mutate(InitialDiagnosisDate = format(as_datetime(InitialDiagnosisDate), format = "%Y-%m-%d"),
                               ICD10Version = as.integer(str_extract(ICD10Version, "\\d+")))      # Extract ICD-10 catalogue version year from string

ProgressBar$tick()


# Transform df_CDS_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Histology <- df_CDS_Histology %>%
                        #--- Transformation ------------------------------------
                        dsCCPhos::TransformData(TableName = "Histology", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                        #--- Recoding ------------------------------------------
                        mutate(Grading = dsCCPhos::RecodeData(Grading, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading"),
                                                                            set_names(Value_Curated, Value_Raw)))) %>%
                        #--- Formatting ----------------------------------------
                        mutate(HistologyDate = format(as_datetime(HistologyDate), format = "%Y-%m-%d")) %>%
                        #--- Special -------------------------------------------
                        { if ("HistologyID" %in% names(df_CDS_Histology))      # If column exists, extract integer number from string in HistologyID. Serves as surrogate for chronological order of events.
                          { mutate(., HistologyID = as.integer(str_extract(HistologyID, "\\d+"))) }
                          else {.}
                        }

ProgressBar$tick()


# Transform df_CDS_Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Metastasis <- df_CDS_Metastasis %>%
                          #--- Transformation ----------------------------------
                          dsCCPhos::TransformData(TableName = "Metastasis", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                          #--- Formatting --------------------------------------
                          mutate(MetastasisDiagnosisDate = format(as_datetime(MetastasisDiagnosisDate), format = "%Y-%m-%d"),
                                 HasMetastasis = as.logical(HasMetastasis))

ProgressBar$tick()


# Transform df_CDS_MolecularDiagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>%
                                    #--- Transformation ------------------------
                                    dsCCPhos::TransformData(TableName = "MolecularDiagnostics", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                                    #--- Formatting ----------------------------
                                    mutate(MolecularDiagnosticsDate = format(as_datetime(MolecularDiagnosticsDate), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Patient <- df_CDS_Patient %>%
                        #--- Transformation ------------------------------------
                        dsCCPhos::TransformData(TableName = "Patient", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                        #--- Recoding ------------------------------------------
                        mutate(Gender = dsCCPhos::RecodeData(Gender, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "Gender"),
                                                                          set_names(Value_Curated, Value_Raw))),
                               LastVitalStatus = dsCCPhos::RecodeData(LastVitalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "LastVitalStatus"),
                                                                                            set_names(Value_Curated, Value_Raw)))) %>%
                        #--- Formatting ----------------------------------------
                        mutate(LastVitalStatusDate = format(as_datetime(LastVitalStatusDate), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Progress <- df_CDS_Progress %>%
                        #--- Transformation ------------------------------------
                        dsCCPhos::TransformData(TableName = "Progress", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                        #--- Recoding ------------------------------------------
                        mutate(GlobalStatus = dsCCPhos::RecodeData(GlobalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "GlobalStatus"),
                                                                                      set_names(Value_Curated, Value_Raw))),
                               LocalStatus = dsCCPhos::RecodeData(LocalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LocalStatus"),
                                                                                    set_names(Value_Curated, Value_Raw))),
                               LymphnodalStatus = dsCCPhos::RecodeData(LymphnodalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalStatus"),
                                                                                              set_names(Value_Curated, Value_Raw))),
                               MetastasisStatus = dsCCPhos::RecodeData(MetastasisStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "MetastasisStatus"),
                                                                                              set_names(Value_Curated, Value_Raw)))) %>%
                        #--- Formatting ----------------------------------------
                        mutate(ProgressReportDate = format(as_datetime(ProgressReportDate), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>%
                                #--- Transformation ----------------------------
                                dsCCPhos::TransformData(TableName = "RadiationTherapy", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                                #--- Recoding ----------------------------------
                                mutate(RadiationTherapyRelationToSurgery = dsCCPhos::RecodeData(RadiationTherapyRelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyRelationToSurgery"),
                                                                                                                                        set_names(Value_Curated, Value_Raw))),
                                       RadiationTherapyIntention = dsCCPhos::RecodeData(RadiationTherapyIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyIntention"),
                                                                                                                        set_names(Value_Curated, Value_Raw)))) %>%
                                #--- Formatting --------------------------------
                                mutate(RadiationTherapyStart = format(as_datetime(RadiationTherapyStart), format = "%Y-%m-%d"),
                                       RadiationTherapyEnd = format(as_datetime(RadiationTherapyEnd), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Staging <- df_CDS_Staging %>%
                      #--- Transformation --------------------------------------
                      dsCCPhos::TransformData(TableName = "Staging", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                      #--- Recoding --------------------------------------------
                      mutate(UICCStage = dsCCPhos::RecodeData(UICCStage, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "UICCStage"),
                                                                              set_names(Value_Curated, Value_Raw))),
                             TNM_T = dsCCPhos::RecodeData(TNM_T, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T"),
                                                                      set_names(Value_Curated, Value_Raw))),
                             TNM_N = dsCCPhos::RecodeData(TNM_N, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N"),
                                                                      set_names(Value_Curated, Value_Raw))),
                             TNM_M = dsCCPhos::RecodeData(TNM_M, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M"),
                                                                      set_names(Value_Curated, Value_Raw))),
                             TNM_T_Prefix = dsCCPhos::RecodeData(TNM_T_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T_Prefix"),
                                                                                    set_names(Value_Curated, Value_Raw))),
                             TNM_N_Prefix = dsCCPhos::RecodeData(TNM_N_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N_Prefix"),
                                                                                    set_names(Value_Curated, Value_Raw))),
                             TNM_M_Prefix = dsCCPhos::RecodeData(TNM_M_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M_Prefix"),
                                                                                    set_names(Value_Curated, Value_Raw)))) %>%
                      #--- Formatting ------------------------------------------
                      mutate(StagingReportDate = format(as_datetime(StagingReportDate), format = "%Y-%m-%d"))

ProgressBar$tick()


# Transform df_CDS_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Surgery <- df_CDS_Surgery %>%
                      #--- Transformation --------------------------------------
                      dsCCPhos::TransformData(TableName = "Surgery", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                      #--- Recoding --------------------------------------------
                      mutate(SurgeryIntention = dsCCPhos::RecodeData(SurgeryIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "SurgeryIntention"),
                                                                                            set_names(Value_Curated, Value_Raw))),
                             ResidualAssessmentLocal = dsCCPhos::RecodeData(ResidualAssessmentLocal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal"),
                                                                                                          set_names(Value_Curated, Value_Raw))),
                             ResidualAssessmentTotal = dsCCPhos::RecodeData(ResidualAssessmentTotal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal"),
                                                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                      #--- Formatting ------------------------------------------
                      mutate(SurgeryDate = format(as_datetime(SurgeryDate), format = "%Y-%m-%d")) %>%
                      #--- Special ---------------------------------------------
                      { if ("SurgeryID" %in% names(df_CDS_Surgery))      # If column exists, extract integer number from string in SurgeryID. Serves as surrogate for chronological order of events.
                        { mutate(., SurgeryID = as.integer(str_extract(SurgeryID, "\\d+"))) }
                        else {.}
                      }

ProgressBar$tick()


# Transform df_CDS_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>%
                              #--- Transformation ------------------------------
                              dsCCPhos::TransformData(TableName = "SystemicTherapy", RuleSet = dsCCPhos::RuleSet_RawDataTransformation, RuleProfile = RuleProfile_RawDataTransformation) %>%
                              #--- Recoding ------------------------------------
                              mutate(SystemicTherapyIntention = dsCCPhos::RecodeData(SystemicTherapyIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "SystemicTherapyIntention"),
                                                                                                                    set_names(Value_Curated, Value_Raw))),
                                     SystemicTherapyRelationToSurgery = dsCCPhos::RecodeData(SystemicTherapyRelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "SystemicTherapyRelationToSurgery"),
                                                                                                                                    set_names(Value_Curated, Value_Raw)))) %>%
                              #--- Formatting ----------------------------------
                              mutate(IsChemotherapy = as.logical(IsChemotherapy),
                                     IsImmunotherapy = as.logical(IsImmunotherapy),
                                     IsHormoneTherapy = as.logical(IsHormoneTherapy),
                                     IsBoneMarrowTransplant = as.logical(IsBoneMarrowTransplant),
                                     SystemicTherapyStart = format(as_datetime(SystemicTherapyStart), format = "%Y-%m-%d"),
                                     SystemicTherapyEnd = format(as_datetime(SystemicTherapyEnd), format = "%Y-%m-%d"))

ProgressBar$tick()


# Re-pack data frames into list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_CuratedDataSet <- list(BioSampling = df_CDS_BioSampling,
                          Diagnosis = df_CDS_Diagnosis,
                          Histology = df_CDS_Histology,
                          Metastasis = df_CDS_Metastasis,
                          MolecularDiagnostics = df_CDS_MolecularDiagnostics,
                          Patient = df_CDS_Patient,
                          Progress = df_CDS_Progress,
                          RadiationTherapy = df_CDS_RadiationTherapy,
                          Staging = df_CDS_Staging,
                          Surgery = df_CDS_Surgery,
                          SystemicTherapy = df_CDS_SystemicTherapy)

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track feature values after Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_Monitors_Transformed <- map2(.x = ls_CuratedDataSet,
                                .y = ls_MonitorFeatures_All,
                                .f = function(DataFrame, MonitorFeatures)
                                     {
                                          DataFrame %>%
                                              dsCCPhos::TrackFeatureValues(Features = MonitorFeatures,
                                                                           CurationStage = "Transformed")
                                     })

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finalize transformation of data values using dsCCPhos::FinalizeDataTransformation()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - (Optional) Exclusion of ineligible data (including data that could not be transformed)
#   - (Optional) Conversion to ordered factor
#   - (Optional) Assignment of factor labels
#   - All predefined information stored in dsCCPhos::Meta_ValueSets
#-------------------------------------------------------------------------------

# Finalize df_CDS_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_BioSampling <- df_CDS_BioSampling %>%
                          mutate(SampleType = dsCCPhos::FinalizeDataTransformation(SampleType, TableName = "BioSampling", FeatureName = "SampleType"),
                                 SampleAliquot = dsCCPhos::FinalizeDataTransformation(SampleAliquot, TableName = "BioSampling", FeatureName = "SampleAliquot"))

ProgressBar$tick()


# Finalize df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        mutate(LocalizationSide = dsCCPhos::FinalizeDataTransformation(LocalizationSide, TableName = "Diagnosis", FeatureName = "LocalizationSide", AssignFactorLabels = TRUE))

ProgressBar$tick()


# Finalize df_CDS_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Histology <- df_CDS_Histology %>%
                        mutate(Grading = dsCCPhos::FinalizeDataTransformation(Grading, TableName = "Histology", FeatureName = "Grading", AssignFactorLabels = TRUE))

ProgressBar$tick()


# Finalize df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Patient <- df_CDS_Patient %>%
                        mutate(Gender = dsCCPhos::FinalizeDataTransformation(Gender, TableName = "Patient", FeatureName = "Gender", AssignFactorLabels = TRUE),
                               LastVitalStatus = dsCCPhos::FinalizeDataTransformation(LastVitalStatus, TableName = "Patient", FeatureName = "LastVitalStatus"))

ProgressBar$tick()


# Finalize df_CDS_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Progress <- df_CDS_Progress %>%
                        mutate(GlobalStatus = dsCCPhos::FinalizeDataTransformation(GlobalStatus, TableName = "Progress", FeatureName = "GlobalStatus", AssignFactorLabels = TRUE),
                               LocalStatus = dsCCPhos::FinalizeDataTransformation(LocalStatus, TableName = "Progress", FeatureName = "LocalStatus", AssignFactorLabels = TRUE),
                               LymphnodalStatus = dsCCPhos::FinalizeDataTransformation(LymphnodalStatus, TableName = "Progress", FeatureName = "LymphnodalStatus", AssignFactorLabels = TRUE),
                               MetastasisStatus = dsCCPhos::FinalizeDataTransformation(MetastasisStatus, TableName = "Progress", FeatureName = "MetastasisStatus", AssignFactorLabels = TRUE))

ProgressBar$tick()


# Finalize df_CDS_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>%
                                mutate(RadiationTherapyRelationToSurgery = dsCCPhos::FinalizeDataTransformation(RadiationTherapyRelationToSurgery, TableName = "RadiationTherapy", FeatureName = "RadiationTherapyRelationToSurgery", AssignFactorLabels = TRUE),
                                       RadiationTherapyIntention = dsCCPhos::FinalizeDataTransformation(RadiationTherapyIntention, TableName = "RadiationTherapy", FeatureName = "RadiationTherapyIntention", AssignFactorLabels = TRUE))

ProgressBar$tick()


# Finalize df_CDS_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Staging <- df_CDS_Staging %>%
                      mutate(UICCStage = dsCCPhos::FinalizeDataTransformation(UICCStage, TableName = "Staging", FeatureName = "UICCStage"),
                             TNM_T = dsCCPhos::FinalizeDataTransformation(TNM_T, TableName = "Staging", FeatureName = "TNM_T"),
                             TNM_N = dsCCPhos::FinalizeDataTransformation(TNM_N, TableName = "Staging", FeatureName = "TNM_N"),
                             TNM_M = dsCCPhos::FinalizeDataTransformation(TNM_M, TableName = "Staging", FeatureName = "TNM_M"),
                             TNM_T_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_T_Prefix, TableName = "Staging", FeatureName = "TNM_T_Prefix"),
                             TNM_N_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_N_Prefix, TableName = "Staging", FeatureName = "TNM_N_Prefix"),
                             TNM_M_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_M_Prefix, TableName = "Staging", FeatureName = "TNM_M_Prefix"),
                             TNM_ySymbol = dsCCPhos::FinalizeDataTransformation(TNM_ySymbol, TableName = "Staging", FeatureName = "TNM_ySymbol"),
                             TNM_rSymbol = dsCCPhos::FinalizeDataTransformation(TNM_rSymbol, TableName = "Staging", FeatureName = "TNM_rSymbol"))

ProgressBar$tick()


# Finalize df_CDS_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Surgery <- df_CDS_Surgery %>%
                      mutate(SurgeryIntention = dsCCPhos::FinalizeDataTransformation(SurgeryIntention, TableName = "Surgery", FeatureName = "SurgeryIntention", AssignFactorLabels = TRUE),
                             ResidualAssessmentLocal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentLocal, TableName = "Surgery", FeatureName = "ResidualAssessmentLocal"),
                             ResidualAssessmentTotal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentTotal, TableName = "Surgery", FeatureName = "ResidualAssessmentTotal"))

ProgressBar$tick()


# Finalize df_CDS_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>%
                              mutate(SystemicTherapyRelationToSurgery = dsCCPhos::FinalizeDataTransformation(SystemicTherapyRelationToSurgery, TableName = "SystemicTherapy", FeatureName = "SystemicTherapyRelationToSurgery", AssignFactorLabels = TRUE),
                                     SystemicTherapyIntention = dsCCPhos::FinalizeDataTransformation(SystemicTherapyIntention, TableName = "SystemicTherapy", FeatureName = "SystemicTherapyIntention", AssignFactorLabels = TRUE))

ProgressBar$tick()


# Re-pack data frames into list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_CuratedDataSet <- list(BioSampling = df_CDS_BioSampling,
                          Diagnosis = df_CDS_Diagnosis,
                          Histology = df_CDS_Histology,
                          Metastasis = df_CDS_Metastasis,
                          MolecularDiagnostics = df_CDS_MolecularDiagnostics,
                          Patient = df_CDS_Patient,
                          Progress = df_CDS_Progress,
                          RadiationTherapy = df_CDS_RadiationTherapy,
                          Staging = df_CDS_Staging,
                          Surgery = df_CDS_Surgery,
                          SystemicTherapy = df_CDS_SystemicTherapy)

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values after Finalized Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_Monitors_Final <- map2(.x = ls_CuratedDataSet,
                          .y = ls_MonitorFeatures_All,
                          .f = function(DataFrame, MonitorFeatures)
                               {
                                    DataFrame %>%
                                        dsCCPhos::TrackFeatureValues(Features = MonitorFeatures,
                                                                     CurationStage = "Final")
                               })

ProgressBar$tick()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Merge Monitor Objects into Coherent Summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitorSummaries <- list()

for (i in 1:length(ls_Monitors_Raw))
{
    if (nrow(ls_Monitors_Raw[[i]]) > 0)
    {
        df_CurrentSummary <- ls_Monitors_Raw[[i]] %>%
                                  dplyr::full_join(ls_Monitors_Transformed[[i]], by = join_by(Feature, Value, IsValueEligible, CurationStage, Frequency)) %>%
                                  dplyr::full_join(ls_Monitors_Final[[i]], by = join_by(Feature, Value, IsValueEligible, CurationStage, Frequency)) %>%
                                  tidyr::pivot_wider(names_from = CurationStage,
                                                     values_from = Frequency) %>%
                                  dplyr::group_by(Feature, Value, IsValueEligible) %>%
                                      dplyr::summarize(Raw = sum(Raw, na.rm = TRUE),
                                                       Transformed = sum(Transformed, na.rm = TRUE),
                                                       Final = sum(Final, na.rm = TRUE)) %>%
                                      dplyr::arrange(Feature, desc(IsValueEligible))
    }
    else
    {
        df_CurrentSummary <- NULL
    }

    ls_MonitorSummaries <- c(ls_MonitorSummaries, list(df_CurrentSummary))
}

# Name the monitor summary objects
names(ls_MonitorSummaries) <- c("Monitor_BioSampling",
                                "Monitor_Diagnosis",
                                "Monitor_Histology",
                                "Monitor_Metastasis",
                                "Monitor_MolecularDiagnostics",
                                "Monitor_Patient",
                                "Monitor_Progress",
                                "Monitor_RadiationTherapy",
                                "Monitor_Staging",
                                "Monitor_Surgery",
                                "Monitor_SystemicTherapy")

ProgressBar$tick()
ProgressBar$terminate()




# df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Clear redundant patient entries
#-------------------------------------------------------------------------------
# df_Aux_Patient <- df_CDS_Patient %>%
#                       group_by(PatientID) %>%
#                           mutate(CountDifferentCombinations = n()) %>%
#                       filter(CountDifferentCombinations > 1)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODUL 2)  Process diagnosis data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A) Joining of df_CDS_Diagnosis and df_CDS_Histology to get coherent diagnosis data
#   B) Classification and removal of redundant diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   C) Classification and bundling of associated diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   D) Reconstruct df_CDS_Histology from df_CDS_Diagnosis
#-------------------------------------------------------------------------------


# A) Join df_CDS_Diagnosis and df_CDS_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Create composed ID ('DiagnosisID / HistologyID')
#   - Add auxiliary features for filtering purposes
#-------------------------------------------------------------------------------

# Create custom histology ID in case it is not contained in raw data
df_CDS_Histology <- df_CDS_Histology %>%
                        mutate(HistologyID = row_number())


df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        left_join(df_CDS_Histology, by = join_by(PatientID, DiagnosisID)) %>%
                        mutate(OriginalDiagnosisID = DiagnosisID,
                               DiagnosisID = paste0(DiagnosisID, "/", HistologyID)) %>%
                        relocate(DiagnosisID, .after = PatientID) %>%
                        group_by(PatientID) %>%
                            mutate(PatientCountInitialEntries = n()) %>%
                        ungroup()

# df_Aux_Diagnosis_IDMappingInitial <- df_CDS_Diagnosis %>%
#                                           select(OriginalDiagnosisID,
#                                                  DiagnosisID)


# B) Classification and removal of redundant entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisRedundancy() to identify and consolidate redundant diagnosis entries
#   - Rules for classification of redundancy are defined in customizable data object delivered with dsCCPhos
#   - Replace redundant DiagnosisIDs in all related tables
#-------------------------------------------------------------------------------


# Compile rule calls from data using dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Names of features that are required to compile rule calls from dsCCPhos::RuleSet_DiagnosisRedundancy
PredictorFeatures_DiagnosisRedundancy = c("CountDeviatingValues",
                                          "InitialDiagnosisDate",
                                          "ICD10Code",
                                          "ICDOTopographyCode",
                                          "LocalizationSide",
                                          "HistologyDate",
                                          "ICDOMorphologyCode",
                                          "Grading")

# Pass required information to dsCCPhos::CompileClassificationCall to compile rull calls (dplyr::case_when-Statements)
Call_IsLikelyRedundant <- CompileClassificationCall(TargetFeature = "IsLikelyRedundant",
                                                    PredictorFeatures = PredictorFeatures_DiagnosisRedundancy,
                                                    RuleSet = dsCCPhos::RuleSet_DiagnosisRedundancy,
                                                    RuleProfile = RuleProfile_DiagnosisRedundancy,
                                                    ValueIfNoRuleMet = FALSE)
# Make list of rule calls to pass them to function
RuleCalls_DiagnosisRedundancy <- list(IsLikelyRedundant = Call_IsLikelyRedundant)


# Set up progress bar
CountProgressItems <- df_CDS_Diagnosis %>% filter(PatientCountInitialEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying redundant diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)


# Filter patients with multiple diagnosis entries and apply dsCCPhos::ClassifyDiagnosisRedundancy()
df_Aux_Diagnosis_ClassifiedRedundancies <- df_CDS_Diagnosis %>%
                                                filter(PatientCountInitialEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisRedundancy(DiagnosisEntries = .x,
                                                                                               RuleCalls = RuleCalls_DiagnosisRedundancy,
                                                                                               ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble df_CDS_Diagnosis after processing of redundant diagnosis entries
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        filter(PatientCountInitialEntries == 1) %>%
                        bind_rows(df_Aux_Diagnosis_ClassifiedRedundancies) %>%
                        arrange(PatientID) %>%
                        group_by(PatientID) %>%
                            mutate(PatientCountDistinctEntries = n()) %>%
                        ungroup()

# For monitoring purposes, obtain:
# a) number of redundant diagnosis entries and
# b) number of patients that had redundant diagnosis entries
CountDiagnosisRedundancies <- sum(df_CDS_Diagnosis$CountRedundancies, na.rm = TRUE)
CountPatientsWithRedundancies <- df_CDS_Diagnosis %>%
                                      filter(CountRedundancies > 0) %>%
                                      pull(PatientID) %>%
                                      n_distinct()

# Output monitoring message
cat("Found ", CountDiagnosisRedundancies, " redundancies related to ", CountPatientsWithRedundancies, " patient IDs. \n")


# Replace IDs (Original DiagnosisID and not newly composed one) of redundant diagnosis entries in related tables
# Get table of affected DiagnosisIDs
df_Aux_Diagnosis_IDMappingRedundancies <- df_CDS_Diagnosis %>%
                                              ungroup() %>%
                                              filter(CountRedundancies > 0) %>%
                                              select(PatientID, RedundantOriginalIDs, OriginalDiagnosisID) %>%
                                              unnest(cols = c(RedundantOriginalIDs)) %>%
                                              rename(all_of(c(OldDiagnosisID = "RedundantOriginalIDs",
                                                              NewDiagnosisID = "OriginalDiagnosisID"))) %>%
                                              filter(OldDiagnosisID != NewDiagnosisID) %>%
                                              distinct()

# Replace IDs of redundant diagnosis entries in related tables
df_CDS_Metastasis <- df_CDS_Metastasis %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_Progress <- df_CDS_Progress %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_Staging <- df_CDS_Staging %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_Surgery <- df_CDS_Surgery %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)

# Remove columns of redundant IDs (not needed anymore)
df_CDS_Diagnosis <-  df_CDS_Diagnosis %>%
                          select(-c(RedundantIDs,
                                    RedundantOriginalIDs,
                                    CountRedundancies))


# C) Classify associations between diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple distinct diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisAssociations() to plausibly distinguish pseudo-different from actually different diagnoses
#   - Rules for classification of associated diagnosis entries are defined in customizable data object delivered with dsCCPhos
#   - Replace DiagnosisIDs in all related tables with ReferenceDiagnosisID
#-------------------------------------------------------------------------------

# Names of features that are required to compile rule calls from dsCCPhos::RuleSet_DiagnosisAssociation
PredictorFeatures_DiagnosisAssociation <- c("ICD10Code",
                                            "ICD10CodeShort",
                                            "ICD10Group",
                                            "ICDOTopographyCode",
                                            "ICDOTopographyCodeShort",
                                            "LocalizationSide",
                                            "ICDOMorphologyCode",
                                            "ICDOMorphologyCodeShort",
                                            "Grading")

# Compile rule calls (unevaluated dplyr::case_when-Statements) with dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Call_IsLikelyAssociated <- CompileClassificationCall(TargetFeature = "IsLikelyAssociated",
                                                     PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                     RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                     RuleProfile = RuleProfile_DiagnosisAssociation,
                                                     ValueIfNoRuleMet = FALSE)

Call_InconsistencyCheck <- CompileClassificationCall(TargetFeature = "InconsistencyCheck",
                                                     PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                     RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                     RuleProfile = RuleProfile_DiagnosisAssociation,
                                                     ValueIfNoRuleMet = "No apparent inconsistency")

Call_ImplausibilityCheck <- CompileClassificationCall(TargetFeature = "ImplausibilityCheck",
                                                      PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                      RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                      RuleProfile = RuleProfile_DiagnosisAssociation,
                                                      ValueIfNoRuleMet = "No apparent implausibility")

Call_Relation_ICD10 <- CompileClassificationCall(TargetFeature = "Relation_ICD10",
                                                 PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                 RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                 RuleProfile = RuleProfile_DiagnosisAssociation,
                                                 ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOTopography <- CompileClassificationCall(TargetFeature = "Relation_ICDOTopography",
                                                          PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                          RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                          RuleProfile = RuleProfile_DiagnosisAssociation,
                                                          ValueIfNoRuleMet = NA_character_)

Call_Relation_LocalizationSide <- CompileClassificationCall(TargetFeature = "Relation_LocalizationSide",
                                                            PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                            RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                            RuleProfile = RuleProfile_DiagnosisAssociation,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOMorphology <- CompileClassificationCall(TargetFeature = "Relation_ICDOMorphology",
                                                            PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                            RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                            RuleProfile = RuleProfile_DiagnosisAssociation,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_Grading <- CompileClassificationCall(TargetFeature = "Relation_Grading",
                                                   PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                   RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                   RuleProfile = RuleProfile_DiagnosisAssociation,
                                                   ValueIfNoRuleMet = NA_character_)

Call_IsLikelyProgression <- CompileClassificationCall(TargetFeature = "IsLikelyProgression",
                                                      PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                      RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                      RuleProfile = RuleProfile_DiagnosisAssociation,
                                                      ValueIfNoRuleMet = NA)

Call_IsLikelyRecoding <- CompileClassificationCall(TargetFeature = "IsLikelyRecoding",
                                                   PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                   RuleSet = dsCCPhos::RuleSet_DiagnosisAssociation,
                                                   RuleProfile = RuleProfile_DiagnosisAssociation,
                                                   ValueIfNoRuleMet = NA)


# Make list of rule calls to pass them to function
RuleCalls_DiagnosisAssociation <- list(IsLikelyAssociated = Call_IsLikelyAssociated,
                                       InconsistencyCheck = Call_InconsistencyCheck,
                                       ImplausibilityCheck = Call_ImplausibilityCheck,
                                       Relation_ICD10 = Call_Relation_ICD10,
                                       Relation_ICDOTopography = Call_Relation_ICDOTopography,
                                       Relation_LocalizationSide = Call_Relation_LocalizationSide,
                                       Relation_ICDOMorphology = Call_Relation_ICDOMorphology,
                                       Relation_Grading = Call_Relation_Grading,
                                       IsLikelyProgression = Call_IsLikelyProgression,
                                       IsLikelyRecoding = Call_IsLikelyRecoding)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set up progress bar
CountProgressItems <- df_CDS_Diagnosis %>% filter(PatientCountDistinctEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying associated diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)

# Filter patients with multiple distinct diagnosis entries and apply dsCCPhos::ClassifyDiagnosisAssociations()
df_Aux_Diagnosis_ClassifiedAssociations <- df_CDS_Diagnosis %>%
                                                filter(PatientCountDistinctEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisAssociation(DiagnosisEntries = .x,
                                                                                                RuleCalls = RuleCalls_DiagnosisAssociation,
                                                                                                ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble df_CDS_Diagnosis after processing of associated diagnosis entries
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        filter(PatientCountDistinctEntries == 1) %>%
                        mutate(ReferenceDiagnosisID = DiagnosisID) %>%
                        bind_rows(df_Aux_Diagnosis_ClassifiedAssociations) %>%
                        arrange(PatientID) %>%
                        relocate(c(PatientID, ReferenceDiagnosisID), .before = DiagnosisID)

# For monitoring purposes, obtain:
# a) number of associated diagnosis entries and
# b) number of patients that have associated diagnosis entries
CountAssociatedDiagnoses <- sum(df_CDS_Diagnosis$IsLikelyAssociated, na.rm = TRUE)
CountPatientsWithAssociatedDiagnoses <- df_Aux_Diagnosis_ClassifiedAssociations %>%
                                            filter(IsLikelyAssociated == TRUE) %>%
                                            pull(PatientID) %>%
                                            n_distinct()

# Output monitoring message
cat("Classified ", CountAssociatedDiagnoses, " associated diagnosis entries related to ", CountPatientsWithAssociatedDiagnoses, " patient IDs. \n")


# Create table for DiagnosisID replacement in related tables
df_Aux_Diagnosis_IDMappingAssociations <- df_CDS_Diagnosis %>%
                                              ungroup() %>%
                                              select(PatientID, OriginalDiagnosisID, ReferenceDiagnosisID) %>%
                                              rename(all_of(c(OldDiagnosisID = "OriginalDiagnosisID",
                                                              NewDiagnosisID = "ReferenceDiagnosisID"))) %>%
                                              distinct()

# Replace DiagnosisIDs in all related tables with ReferenceDiagnosisID to associate entries
df_CDS_Metastasis <- df_CDS_Metastasis %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_Progress <- df_CDS_Progress %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_Staging <- df_CDS_Staging %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_Surgery <- df_CDS_Surgery %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations)



# D) Rename columns in df_CDS_Diagnosis and reconstruct df_CDS_Histology from df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove OriginalDiagnosisID column (not needed anymore)
df_CDS_Diagnosis <-  df_CDS_Diagnosis %>%
                          select(-OriginalDiagnosisID) %>%
                          rename(all_of(c(SubDiagnosisID = "DiagnosisID",
                                          DiagnosisID = "ReferenceDiagnosisID")))

# Reconstruct df_CDS_Histology
df_CDS_Histology <- df_CDS_Diagnosis %>%
                        select(all_of(c("SubDiagnosisID", names(df_CDS_Histology)))) %>%
                        relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = HistologyID)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re-pack data frames into list to get compact Curated Data Set (CDS) object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_CuratedDataSet <- list(BioSampling = df_CDS_BioSampling,
                          Diagnosis = df_CDS_Diagnosis,
                          Histology = df_CDS_Histology,
                          Metastasis = df_CDS_Metastasis,
                          MolecularDiagnostics = df_CDS_MolecularDiagnostics,
                          Patient = df_CDS_Patient,
                          Progress = df_CDS_Progress,
                          RadiationTherapy = df_CDS_RadiationTherapy,
                          Staging = df_CDS_Staging,
                          Surgery = df_CDS_Surgery,
                          SystemicTherapy = df_CDS_SystemicTherapy)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RETURN STATEMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Return both the Curated Data Set (CDS) and the list of monitor objects as Curation Report
return(list(CuratedDataSet = ls_CuratedDataSet,
            CurationReport = ls_MonitorSummaries))

}

