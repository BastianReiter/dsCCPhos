
#' CurateDataDS
#'
#' Takes Raw Data Set (RDS) and transforms it into Curated Data Set (CDS) while tracing transformation operations.
#'
#' Server-side ASSIGN method
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#'
#' @return A list containing the Curated Data Set (CDS) and a curation report.
#' @export
#'
#' @examples
CurateDataDS <- function(Name_RawDataSet = "RawDataSet")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROUGH OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   1) Evaluation and parsing of input
#   2) Transform feature names
#   3) Definition of features to monitor during value transformation
#   4) Value transforming operations
#   5) Additional transforming operations / correction of inconsistencies
#   6) Return statement

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_CuratedDataSet <- purrr::map(.x = names(RawDataSet),
                                .f = function(TableName)
                                     {
                                         # Create named vector to look up matching feature names in meta data
                                         vc_Lookup <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Raw
                                         names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Curated

                                         # Rename feature names according to look-up vector
                                         dplyr::rename(RawDataSet[[TableName]], any_of(vc_Lookup))      # Returns a tibble
                                     })

# Re-set table names
names(ls_CuratedDataSet) <- names(RawDataSet)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definition of features to monitor during Curation (Transformation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Object syntax: List of vectors
#     - Vector names = Name of feature to be monitored during Curation (Transformation)
#     - Vector values = Set of eligible values defined in Meta Data
# If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

ls_MonitorFeatures_BioSampling <- list(SampleType = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleType")$Value_Curated,
                                       SampleAliquot = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleAliquot")$Value_Curated)


ls_MonitorFeatures_Diagnosis <- list(ICD10Version = NULL,
                                     LocalizationSide = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Diagnosis" & FeatureName == "LocalizationSide")$Value_Curated)


ls_MonitorFeatures_Histology <- list(Grading = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Histology" & FeatureName == "Grading")$Value_Curated)


ls_MonitorFeatures_Metastasis <- list(MetastasisLocalization = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Metastasis" & FeatureName == "MetastasisLocalization")$Value_Curated)


ls_MonitorFeatures_MolecularDiagnostics <- list()


ls_MonitorFeatures_Patient <- list(Gender = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "Gender")$Value_Curated,
                                   LastVitalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "LastVitalStatus")$Value_Curated)


ls_MonitorFeatures_Progress <- list(GlobalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "GlobalStatus")$Value_Curated,
                                    LocalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LocalStatus")$Value_Curated,
                                    LymphnodalStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LymphnodalStatus")$Value_Curated,
                                    MetastasisStatus = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "MetastasisStatus")$Value_Curated)


ls_MonitorFeatures_RadiationTherapy <- list(RadiationTherapyRelationToSurgery = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyRelationToSurgery")$Value_Curated,
                                            RadiationTherapyIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyIntention")$Value_Curated)


ls_MonitorFeatures_Staging <- list(UICCStage = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "UICCStage")$Value_Curated,
                                   TNM_T = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T")$Value_Curated,
                                   TNM_N = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N")$Value_Curated,
                                   TNM_M = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M")$Value_Curated,
                                   TNM_T_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T_Prefix")$Value_Curated,
                                   TNM_N_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N_Prefix")$Value_Curated,
                                   TNM_M_Prefix = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M_Prefix")$Value_Curated,
                                   TNM_ySymbol = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_ySymbol")$Value_Curated,
                                   TNM_rSymbol = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_rSymbol")$Value_Curated,
                                   TNM_mSymbol = NULL)


ls_MonitorFeatures_Surgery <- list(SurgeryIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "SurgeryIntention")$Value_Curated,
                                   ResidualAssessmentLocal = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentLocal")$Value_Curated,
                                   ResidualAssessmentTotal = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentTotal")$Value_Curated)


ls_MonitorFeatures_SystemicTherapy <- list(IsChemotherapy = NULL,
                                           IsImmunotherapy = NULL,
                                           IsHormoneTherapy = NULL,
                                           IsBoneMarrowTransplant = NULL,
                                           SystemicTherapyIntention = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyIntention")$Value_Curated,
                                           SystemicTherapyRelationToSurgery = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyRelationToSurgery")$Value_Curated)


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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CURATION: Value transforming Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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


# dsCCPhos::Recode() uses a dictionary in the form of a named vector to perform recoding on a target vector


# Transform df_CDS_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_BioSampling <- df_CDS_BioSampling %>%
                          mutate(SampleType = dsCCPhos::Recode(SampleType, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleType"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                                                set_names(Value_Curated, Value_Raw))),      # This returns a vector of the form c("Value_Raw1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                                 #--------------------------------------------------
                                 SampleAliquot = dsCCPhos::Recode(SampleAliquot, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleAliquot"),
                                                                                    set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        mutate(ICD10Version = as.integer(str_extract(ICD10Version, "\\d+")),      # Extract ICD-10 catalogue version year from string
                               #------------------------------------------------
                               LocalizationSide = str_to_upper(LocalizationSide),
                               LocalizationSide = str_remove_all(LocalizationSide, " "),
                               LocalizationSide = dsCCPhos::Recode(LocalizationSide, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Diagnosis" & FeatureName == "LocalizationSide"),
                                                                                        set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Histology <- df_CDS_Histology %>%
                        mutate(HistologyID = as.integer(str_extract(HistologyID, "\\d+")),      # Extract integer number from string in HistologyID. Serves as surrogate for chronological order of events.
                               #----------------------------------------------------
                               Grading = str_to_upper(Grading),
                               Grading = str_remove_all(Grading, " "),
                               Grading = str_replace_all(Grading, "\\|", "I"),      # Replace symbol for Roman "One"
                               Grading = dsCCPhos::Recode(Grading, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Histology" & FeatureName == "Grading"),
                                                                      set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Metastasis <- df_CDS_Metastasis %>%
                          mutate(MetastasisDiagnosisDate = lubridate::as_date(MetastasisDiagnosisDate, format = "%d.%m.%Y"),
                                 #--------------------------------------------------
                                 HasMetastasis = as.logical(HasMetastasis),
                                 #--------------------------------------------------
                                 MetastasisLocalization = str_to_upper(MetastasisLocalization),
                                 MetastasisLocalization = str_remove_all(MetastasisLocalization, " "))


# Transform df_CDS_MolecularDiagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>%
                                    mutate(MolecularDiagnosticsDate = lubridate::as_date(MolecularDiagnosticsDate, format = "%d.%m.%Y"))


# Transform df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Patient <- df_CDS_Patient %>%
                        mutate(Gender = str_to_upper(Gender),      # Convert all lower to upper letters
                               Gender = str_remove_all(Gender, " "),      # Eliminate spaces
                               Gender = dsCCPhos::Recode(Gender, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "Gender"),
                                                                    set_names(Value_Curated, Value_Raw))),
                               #----------------------------------------------------
                               LastVitalStatusDate = lubridate::as_date(LastVitalStatusDate, format = "%m.%Y") + days(14),
                               #----------------------------------------------------
                               LastVitalStatus = dsCCPhos::Recode(LastVitalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "LastVitalStatus"),
                                                                                      set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Progress <- df_CDS_Progress %>%
                        mutate(ProgressReportDate = lubridate::as_date(ProgressReportDate, format = "%d.%m.%Y"),
                               #----------------------------------------------------
                               GlobalStatus = dsCCPhos::Recode(GlobalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "GlobalStatus"),
                                                                                  set_names(Value_Curated, Value_Raw))),
                               #----------------------------------------------------
                               LocalStatus = dsCCPhos::Recode(LocalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LocalStatus"),
                                                                                set_names(Value_Curated, Value_Raw))),
                               #----------------------------------------------------
                               LymphnodalStatus = dsCCPhos::Recode(LymphnodalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LymphnodalStatus"),
                                                                                          set_names(Value_Curated, Value_Raw))),
                               #----------------------------------------------------
                               MetastasisStatus = dsCCPhos::Recode(MetastasisStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "MetastasisStatus"),
                                                                                          set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>%
                                mutate(RadiationTherapyRelationToSurgery = dsCCPhos::Recode(RadiationTherapyRelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyRelationToSurgery"),
                                                                                                                                  set_names(Value_Curated, Value_Raw))),
                                       #----------------------------------------
                                       RadiationTherapyIntention = dsCCPhos::Recode(RadiationTherapyIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyIntention"),
                                                                                                                  set_names(Value_Curated, Value_Raw))),
                                       #----------------------------------------
                                       RadiationTherapyStart = as_date(RadiationTherapyStart, format = "%d.%m.%Y"),
                                       RadiationTherapyEnd = as_date(RadiationTherapyEnd, format = "%d.%m.%Y"))


# Transform df_CDS_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Staging <- df_CDS_Staging %>%
                      mutate(StagingReportDate = lubridate::as_date(StagingReportDate, format = "%d.%m.%Y"),
                             #------------------------------------------------------
                             UICCStage = str_to_upper(UICCStage),
                             UICCStage = str_remove_all(UICCStage, " "),
                             UICCStage = str_replace_all(UICCStage, c("0A" = "0a",      # For specific values, (re)turn upper to lower letters
                                                                      "0IS" = "0is")),
                             UICCStage = str_replace_all(UICCStage, "\\|", "I"),      # Replace vertical bar symbol with Roman "One"
                             UICCStage = dsCCPhos::Recode(UICCStage, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "UICCStage"),
                                                                        set_names(Value_Curated, Value_Raw))),
                             #------------------------------------------------------
                             TNM_T = str_to_lower(TNM_T),      # Convert all upper to lower letters
                             TNM_T = str_remove_all(TNM_T, " "),
                             TNM_T = str_replace_all(TNM_T, c("is\\(dcis\\)" = "is(DCIS)",      # For specific values, (re)turn lower to upper letters
                                                              "is\\(lcis\\)" = "is(LCIS)",
                                                              "t1mi" = "T1mi",
                                                              "x" = "X")),
                             TNM_T = str_replace_all(TNM_T, c("1\\(sm\\)" = "1b",
                                                              "1\\(sm1\\)" = "1b",
                                                              "1\\(sm2\\)" = "1b",
                                                              "1\\(sm3\\)" = "1b",
                                                              "1\\(sn3\\)" = "1b")),      # In esophageal cancer, T1b tumors can be subdivided depending on area of affected submucosa. This operation eliminates this subdivision.
                             TNM_T = dsCCPhos::Recode(TNM_T, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T"),
                                                                set_names(Value_Curated, Value_Raw))),
                             #--------------------------------------------------
                             TNM_N = str_to_lower(TNM_N),
                             TNM_N = str_remove_all(TNM_N, " "),
                             TNM_N = str_replace_all(TNM_N, c("x" = "X",
                                                              "x\\(sn\\)" = "X(sn)",
                                                              "0sn" = "0(sn)")),
                             TNM_N = dsCCPhos::Recode(TNM_N, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N"),
                                                                set_names(Value_Curated, Value_Raw))),
                             #--------------------------------------------------
                             TNM_M = str_to_lower(TNM_M),      # Convert all upper to lower letters
                             TNM_M = str_remove_all(TNM_M, " "),
                             TNM_M = str_replace_all(TNM_M, c("x" = "X")),
                             TNM_M = dsCCPhos::Recode(TNM_M, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M"),
                                                                set_names(Value_Curated, Value_Raw))),
                             #--------------------------------------------------
                             TNM_ySymbol = str_to_lower(TNM_ySymbol),
                             TNM_ySymbol = str_remove_all(TNM_ySymbol, " "),
                             TNM_ySymbol = ifelse(TNM_ySymbol != "y" & (str_starts(TNM_T_Prefix, "y") | str_starts(TNM_N_Prefix, "y") | str_starts(TNM_M_Prefix, "y")), "y", TNM_ySymbol),
                             #--------------------------------------------------
                             TNM_rSymbol = str_to_lower(TNM_rSymbol),
                             TNM_rSymbol = str_remove_all(TNM_rSymbol, " "),
                             TNM_rSymbol = ifelse(TNM_rSymbol != "r" & (str_starts(TNM_T_Prefix, "r") | str_starts(TNM_N_Prefix, "r") | str_starts(TNM_M_Prefix, "r")), "r", TNM_ySymbol),
                             #--------------------------------------------------
                             TNM_T_Prefix = str_to_lower(TNM_T_Prefix),
                             TNM_T_Prefix = str_remove_all(TNM_T_Prefix, " "),
                             TNM_T_Prefix = str_replace_all(TNM_T_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_T_Prefix = dsCCPhos::Recode(TNM_T_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T_Prefix"),
                                                                              set_names(Value_Curated, Value_Raw))),
                             #--------------------------------------------------
                             TNM_N_Prefix = str_to_lower(TNM_N_Prefix),
                             TNM_N_Prefix = str_remove_all(TNM_N_Prefix, " "),
                             TNM_N_Prefix = str_replace_all(TNM_N_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_N_Prefix = dsCCPhos::Recode(TNM_N_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N_Prefix"),
                                                                              set_names(Value_Curated, Value_Raw))),
                             #------------------------------------------------------
                             TNM_M_Prefix = str_to_lower(TNM_M_Prefix),
                             TNM_M_Prefix = str_remove_all(TNM_M_Prefix, " "),
                             TNM_M_Prefix = str_replace_all(TNM_M_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_M_Prefix = dsCCPhos::Recode(TNM_M_Prefix, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M_Prefix"),
                                                                              set_names(Value_Curated, Value_Raw))))



# Transform df_CDS_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Surgery <- df_CDS_Surgery %>%
                      mutate(SurgeryID = as.integer(str_extract(SurgeryID, "\\d+")),      # Extract integer number from string in SurgeryID. Serves as surrogate for chronological order of events.
                             #----------------------------------------------------
                             SurgeryIntention = dsCCPhos::Recode(SurgeryIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "SurgeryIntention"),
                                                                                      set_names(Value_Curated, Value_Raw))),
                             #------------------------------------------------------
                             ResidualAssessmentLocal = dsCCPhos::Recode(ResidualAssessmentLocal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentLocal"),
                                                                                              set_names(Value_Curated, Value_Raw))),
                             #------------------------------------------------------
                             ResidualAssessmentTotal = dsCCPhos::Recode(ResidualAssessmentTotal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentTotal"),
                                                                                              set_names(Value_Curated, Value_Raw))))


# Transform df_CDS_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>%
                              mutate(IsChemotherapy = as.logical(IsChemotherapy),
                                     IsImmunotherapy = as.logical(IsImmunotherapy),
                                     IsHormoneTherapy = as.logical(IsHormoneTherapy),
                                     IsBoneMarrowTransplant = as.logical(IsBoneMarrowTransplant),
                                     #------------------------------------------
                                     SystemicTherapyIntention = dsCCPhos::Recode(SystemicTherapyIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyIntention"),
                                                                                                              set_names(Value_Curated, Value_Raw))),
                                     #------------------------------------------
                                     SystemicTherapyRelationToSurgery = dsCCPhos::Recode(SystemicTherapyRelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyRelationToSurgery"),
                                                                                                                              set_names(Value_Curated, Value_Raw))),
                                     #------------------------------------------
                                     SystemicTherapyStart = as_date(SystemicTherapyStart, format = "%d.%m.%Y"),
                                     SystemicTherapyEnd = as_date(SystemicTherapyEnd, format = "%d.%m.%Y"))


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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finalize curation of data: Ineligible data (includes data that could not be curated) is turned into NA using factor conversion or other methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Finalize df_CDS_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_BioSampling <- df_CDS_BioSampling %>%
                          mutate(SampleType = factor(SampleType,
                                                     levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleType")$Value_Curated,
                                                     labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleType")$Label_Curated),      # Convert to factor to mark ineligible values as NA and establish level order where appropriate
                                 SampleAliquot = factor(SampleAliquot,
                                                        levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "BioSampling" & FeatureName == "SampleAliquot")$Value_Curated))


# Finalize df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        mutate(LocalizationSide = factor(LocalizationSide,
                                                         levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Diagnosis" & FeatureName == "LocalizationSide")$Value_Curated,
                                                         labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Diagnosis" & FeatureName == "LocalizationSide")$Label_Curated))


# Finalize df_CDS_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Histology <- df_CDS_Histology %>%
                        mutate(Grading = factor(Grading,
                                                levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Histology" & FeatureName == "Grading")$Value_Curated,
                                                labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Histology" & FeatureName == "Grading")$Label_Curated))


# Finalize df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Patient <- df_CDS_Patient %>%
                        mutate(Gender = factor(Gender,
                                               levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "Gender")$Value_Curated,
                                               labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "Gender")$Label_Curated),
                               LastVitalStatus = factor(LastVitalStatus,
                                                        levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Patient" & FeatureName == "LastVitalStatus")$Value_Curated))


# Finalize df_CDS_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Progress <- df_CDS_Progress %>%
                        mutate(GlobalStatus = factor(GlobalStatus,
                                                     levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "GlobalStatus")$Value_Curated,
                                                     labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "GlobalStatus")$Label_Curated),
                               LocalStatus = factor(LocalStatus,
                                                    levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LocalStatus")$Value_Curated,
                                                    labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LocalStatus")$Label_Curated),
                               LymphnodalStatus = factor(LymphnodalStatus,
                                                         levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LymphnodalStatus")$Value_Curated,
                                                         labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "LymphnodalStatus")$Label_Curated),
                               MetastasisStatus = factor(MetastasisStatus,
                                                         levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "MetastasisStatus")$Value_Curated,
                                                         labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Progress" & FeatureName == "MetastasisStatus")$Label_Curated))


# Finalize df_CDS_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_RadiationTherapy <- df_CDS_RadiationTherapy %>%
                                mutate(RadiationTherapyRelationToSurgery = factor(RadiationTherapyRelationToSurgery,
                                                                                  levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyRelationToSurgery")$Value_Curated,
                                                                                  labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyRelationToSurgery")$Label_Curated),
                                       RadiationTherapyIntention = factor(RadiationTherapyIntention,
                                                                          levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyIntention")$Value_Curated,
                                                                          labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "RadiationTherapy" & FeatureName == "RadiationTherapyIntention")$Label_Curated))


# Finalize df_CDS_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Staging <- df_CDS_Staging %>%
                      mutate(UICCStage = factor(UICCStage,
                                                levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "UICCStage")$Value_Curated),
                             TNM_T = factor(TNM_T,
                                            levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T")$Value_Curated),
                             TNM_N = factor(TNM_N,
                                            levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N")$Value_Curated),
                             TNM_M = factor(TNM_M,
                                            levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M")$Value_Curated),
                             TNM_T_Prefix = factor(TNM_T_Prefix,
                                                   levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_T_Prefix")$Value_Curated),
                             TNM_N_Prefix = factor(TNM_N_Prefix,
                                                   levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_N_Prefix")$Value_Curated),
                             TNM_M_Prefix = factor(TNM_M_Prefix,
                                                   levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_M_Prefix")$Value_Curated),
                             TNM_ySymbol = factor(TNM_ySymbol,
                                                  levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_ySymbol")$Value_Curated),
                             TNM_rSymbol = factor(TNM_rSymbol,
                                                   levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Staging" & FeatureName == "TNM_rSymbol")$Value_Curated))


# Finalize df_CDS_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_Surgery <- df_CDS_Surgery %>%
                      mutate(SurgeryIntention = factor(SurgeryIntention,
                                                       levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "SurgeryIntention")$Value_Curated,
                                                       labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "SurgeryIntention")$Label_Curated),
                             ResidualAssessmentLocal = factor(ResidualAssessmentLocal,
                                                              levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentLocal")$Value_Curated),
                             ResidualAssessmentTotal = factor(ResidualAssessmentTotal,
                                                              levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "Surgery" & FeatureName == "ResidualAssessmentTotal")$Value_Curated))


# Finalize df_CDS_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>%
                              mutate(SystemicTherapyRelationToSurgery = factor(SystemicTherapyRelationToSurgery,
                                                                               levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyRelationToSurgery")$Value_Curated,
                                                                               labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyRelationToSurgery")$Label_Curated),
                                     SystemicTherapyIntention = factor(SystemicTherapyIntention,
                                                                       levels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyIntention")$Value_Curated,
                                                                       labels = dplyr::filter(dsCCPhos::Meta_ValueSets, TableName_Curated == "SystemicTherapy" & FeatureName == "SystemicTherapyIntention")$Label_Curated))



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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Merge Monitor Objects into Coherent Summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitorSummaries <- list()

for (i in 1:length(ls_Monitors_Raw))
{
    if (nrow(ls_Monitors_Raw[[i]]) > 0)
    {
        df_CurrentSummary <- ls_Monitors_Raw[[i]] %>%
                                  dplyr::full_join(ls_Monitors_Transformed[[i]]) %>%
                                  dplyr::full_join(ls_Monitors_Final[[i]]) %>%
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADDITIONAL TRANSFORMATIONS (Correction of inconsistencies)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# df_CDS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Clear duplicate patient entries
#-------------------------------------------------------------------------------
df_Aux_Patient <- df_CDS_Patient %>%
                      group_by(PatientID) %>%
                          mutate(CountDifferentCombinations = n()) %>%
                      filter(CountDifferentCombinations > 1)


# df_CDS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple diagnosis entries: Aim to plausibly distinguish pseudo-different from actually different diagnoses
#-------------------------------------------------------------------------------

# Definition of duplicate entries: Same value in following variables:
#   - ICD10Code
#   - ICDOTopography
#   - LocalizationSide

# Document initital counts of different combinations and total duplicates per patient
df_Aux_Diagnosis <- df_CDS_Diagnosis %>%
                        group_by(PatientID) %>%
                            mutate(CountEntries = n(),
                                   CountDifferentCombinations = n_distinct(ICD10Code,
                                                                           ICDOTopographyCode,
                                                                           LocalizationSide),
                                   CountTotalDuplicatesInPatient = CountEntries - CountDifferentCombinations)

# For efficiency reasons, first filter out all patients who don't have any duplicate diagnosis entries...
df_Aux_Diagnosis_NoDuplicates <- df_Aux_Diagnosis %>%
                                      filter(CountTotalDuplicatesInPatient == 0)

# ... then handle all patients with any number of duplicate diagnosis entries
df_Aux_Diagnosis_HandledDuplicates <- df_Aux_Diagnosis %>%
                                            filter(CountTotalDuplicatesInPatient > 0) %>%
                                            group_by(PatientID, ICD10Code, ICDOTopographyCode, LocalizationSide) %>%
                                                mutate(CountDuplicates = n() - 1,
                                                       JointIDs = list(DiagnosisID)) %>%
                                                arrange(InitialDiagnosisDate) %>%
                                                slice_head()

# Get table to re-map deleted DiagnosisIDs in other tables
df_Aux_Diagnosis_IDMapping <- df_Aux_Diagnosis_HandledDuplicates %>%
                                  ungroup() %>%
                                  filter(CountDuplicates > 0) %>%
                                  select(PatientID, JointIDs, DiagnosisID) %>%
                                  unnest(cols = c(JointIDs)) %>%
                                  rename(all_of(c(OldDiagnosisID = "JointIDs",
                                                  NewDiagnosisID = "DiagnosisID")))

# Re-assign df_CDS_Diagnosis after clearance and handling of duplicate entries
df_CDS_Diagnosis <- rbind(df_Aux_Diagnosis_NoDuplicates,
                          df_Aux_Diagnosis_HandledDuplicates)



# Ensure correct re-mapping of DiagnosisIDs in other tables

df_CDSt_Histology <- df_CDS_Histology %>%
                          left_join(df_Aux_Diagnosis_IDMapping, by = join_by(PatientID,
                                                                             DiagnosisID == OldDiagnosisID)) %>%
                          mutate(DiagnosisID = ifelse(!is.na(NewDiagnosisID),
                                                      NewDiagnosisID,
                                                      DiagnosisID)) %>%
                          select(-NewDiagnosisID)





# df_Work_Diagnosis <- df_CDS_Diagnosis %>%
#                           group_by(PatientID) %>%
#                               mutate(CountEntries = n()) %>%
#                               arrange(desc(InitialDiagnosisDate)) %>%
#                               distinct(DiagnosisID,
#                                        ICD10Code,
#                                        ICDOTopographyCode,
#                                        LocalizationSide,
#                                        .keep_all = TRUE) %>%
#                               mutate(CountDifferentEntries = n())
#
#
#
# df_PatientsSingleDiagnosis <- df_Aux_Diagnosis %>%
#                                   filter(CountDifferentCombinations == 1)








                        #            CountDifferentDiagnoses = n_distinct(DiagnosisID),
                        #            CountDifferentCancers = n_distinct(ICD10Code)) %>%
                        # group_by(PatientID, ICD10Code) %>%
                        #     mutate(CountSameDiagnosisDifferentTopography = n_distinct(ICDOTopographyCode),
                        #            CountSameDiagnosisDifferentSide = n_distinct(LocalizationSide)) %>%
                        # group_by(PatientID, ICD10Code, LocalizationSide) %>%
                        #     mutate(CountSameDiagnosisSameSideDifferentTopography = n_distinct(ICDOTopographyCode)) %>%
                        # group_by(PatientID, ICD10Code, ICDOTopographyCode) %>%
                        #     mutate(CountSameDiagnosisSameTopographyDifferentSide = n_distinct(LocalizationSide)) %>%
                        # group_by(PatientID, ICDOTopographyCode) %>%
                        #     mutate(CountSameTopographyDifferentDiagnosis = n_distinct(ICD10Code),
                        #            CountSameTopographyDifferentDiagnosisDifferentSide = n_distinct(LocalizationSide)) %>%
                        # filter(CountDifferences > 0)




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

