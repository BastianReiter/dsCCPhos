
#' HarmonizeData
#'
#' @param DataModel
#'
#' @return
#' @export
#'
#' @examples
HarmonizeData <- function(DataModel = list())
{

require(dplyr)
require(purrr)

DataModel <- LoadRawData()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Feature Names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_HarmonizedDataModel <- purrr::map(.x = names(DataModel),
                                     .f = function(TableName)
                                          {
                                              # Create named vector to look up matching feature names in meta data
                                              vc_Lookup <- dplyr::filter(CCPhos::Meta_FeatureNames, TableName_Harmonized == TableName)$FeatureName_Raw
                                              names(vc_Lookup) <- dplyr::filter(CCPhos::Meta_FeatureNames, TableName_Harmonized == TableName)$FeatureName_Harmonized

                                              # Rename feature names according to lookup-vector
                                              dplyr::rename(DataModel[[TableName]], any_of(vc_Lookup))      # Returns a tibble
                                          })

names(ls_HarmonizedDataModel) <- names(DataModel)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definition of Features to Monitor during Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Object Syntax: List of vectors
#     - Vector Names = Name of Feature to be monitored during Harmonization
#     - Vector Values = Set of eligible values defined in Meta Data
# If a Feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

ls_MonitorFeatures_df_SDM_Patients <- list(Sex = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "Sex")$Value,
                                           LastVitalStatus = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "LastVitalStatus")$Value)


ls_MonitorFeatures_df_SDM_Diagnosis <- list(ICDVersion = NULL)


ls_MonitorFeatures_df_SDM_Tumor <- list(LocalizationSide = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Tumor" & Feature == "LocalizationSide")$Value)


ls_MonitorFeatures_df_SDM_Histology <- list(Grading = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading")$Value)


ls_MonitorFeatures_df_SDM_Staging <- list(UICCStage = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "UICCStage")$Value,
                                          TNM_T = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T")$Value,
                                          TNM_N = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N")$Value,
                                          TNM_M = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M")$Value,
                                          TNM_T_Prefix = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T_Prefix")$Value,
                                          TNM_N_Prefix = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N_Prefix")$Value,
                                          TNM_M_Prefix = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M_Prefix")$Value)


ls_MonitorFeatures_df_SDM_Metastasis <- list(DistantMetastasisLocalization = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Metastasis" & Feature == "DistantMetastasisLocalization")$Value)


ls_MonitorFeatures_df_SDM_Progress <- list(IsSurgery = NULL,
                                           SurgeryIntention = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "SurgeryIntention")$Value,
                                           IsRadiotherapy = NULL,
                                           RadiotherapyIntention = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyIntention")$Value,
                                           RadiotherapyRelationToSurgery = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyRelationToSurgery")$Value,
                                           IsChemotherapy = NULL,
                                           ChemotherapyIntention = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyIntention")$Value,
                                           ChemotherapyRelationToSurgery = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyRelationToSurgery")$Value,
                                           IsImmunotherapy = NULL,
                                           IsHormonetherapy = NULL,
                                           IsBonemarrowtransplant = NULL,
                                           TherapeuticResponse = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "TherapeuticResponse")$Value,
                                           LocalRelapse = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LocalRelapse")$Value,
                                           LymphnodalRelapse = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalRelapse")$Value,
                                           DistantMetastasis = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "DistantMetastasis")$Value)


ls_MonitorFeatures_df_SDM_Surgery <- list(ResidualAssessmentLocal = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal")$Value,
                                          ResidualAssessmentTotal = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal")$Value)


ls_MonitorFeatures_df_SDM_BioSampling <- list(SampleType = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType")$Value,
                                              SampleTissue = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleTissue")$Value,
                                              FixationType = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "FixationType")$Value)



# Put all objects in one list object to make them passable to functions
ls_MonitorFeatures_All <- list(ls_MonitorFeatures_df_SDM_Patients,
                               ls_MonitorFeatures_df_SDM_Diagnosis,
                               ls_MonitorFeatures_df_SDM_Tumor,
                               ls_MonitorFeatures_df_SDM_Histology,
                               ls_MonitorFeatures_df_SDM_Staging,
                               ls_MonitorFeatures_df_SDM_Metastasis,
                               ls_MonitorFeatures_df_SDM_Progress,
                               ls_MonitorFeatures_df_SDM_Surgery,
                               ls_MonitorFeatures_df_SDM_BioSampling)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values of Raw Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Raw <- list(df_SDM_Patients,
                             df_SDM_Diagnosis,
                             df_SDM_Tumor,
                             df_SDM_Histology,
                             df_SDM_Staging,
                             df_SDM_Metastasis,
                             df_SDM_Progress,
                             df_SDM_Surgery,
                             df_SDM_BioSampling)


ls_Monitors_Raw <- map2(.x = ls_MonitoredData_Raw,
                        .y = ls_MonitorFeatures_All,
                        .f = function(inp_df, inp_ls_MonitorFeatures)
                             {
                                inp_df %>%
                                    f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                         inp_ProcessingStatus = "Raw")
                             })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HARMONIZATION / Transformation Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df_SDM_Patients <- df_SDM_Patients %>%
                        select(-DateFirstProgressDoc) %>%      # Exclude unneeded variable
                        mutate(YearFirstCancerDiagnosis = as.integer(YearFirstCancerDiagnosis),
                               #----------------------------------------------------
                               YearOfBirth = as.integer(year(as_date(YearOfBirth, format = "%d.%m.%Y"))),
                               #----------------------------------------------------
                               Sex = str_to_upper(Sex),      # Convert all lower to upper letters
                               Sex = str_remove_all(Sex, " "),      # Eliminate spaces
                               Sex = f_Recode(Sex, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "Sex"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                        set_names(Value, OriginalValue))),      # This returns a vector of the form c("OriginalValue1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                               #----------------------------------------------------
                               DateLastVitalStatus = as_date(DateLastVitalStatus, format = "%m.%Y") + days(14),
                               #----------------------------------------------------
                               LastVitalStatus = f_Recode(LastVitalStatus, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "LastVitalStatus"),
                                                                                set_names(Value, OriginalValue))))


df_SDM_Diagnosis <- df_SDM_Diagnosis %>%
                        mutate(YearCancerDiagnosis = as.integer(YearCancerDiagnosis),
                               AgeCancerDiagnosis = as.integer(AgeCancerDiagnosis),
                               ICDVersion = as.integer(str_extract(ICDVersion, "\\d+")))      # Extract ICD version year from string


df_SDM_Tumor <- df_SDM_Tumor %>%
                    mutate(LocalizationSide = str_to_upper(LocalizationSide),
                           LocalizationSide = str_remove_all(LocalizationSide, " "),
                           LocalizationSide = f_Recode(LocalizationSide, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Tumor" & Feature == "LocalizationSide"),
                                                                              set_names(Value, OriginalValue))))


df_SDM_Histology <- df_SDM_Histology %>%
                        mutate(HistologyID = as.integer(str_extract(HistologyID, "\\d+")),      # Extract integer number from string in HistologyID. Serves as surrogate for chronological order of events.
                               #----------------------------------------------------
                               Grading = str_to_upper(Grading),
                               Grading = str_remove_all(Grading, " "),
                               Grading = str_replace_all(Grading, "\\|", "I"),      # Replace symbol for Roman "One"
                               Grading = f_Recode(Grading, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading"),
                                                                set_names(Value, OriginalValue))))


df_SDM_Staging <- df_SDM_Staging %>%
                      mutate(DateTNMDocumentation = as_date(DateTNMDocumentation, format = "%d.%m.%Y"),
                             #------------------------------------------------------
                             UICCStage = str_to_upper(UICCStage),
                             UICCStage = str_remove_all(UICCStage, " "),
                             UICCStage = str_replace_all(UICCStage, c("0A" = "0a",      # For specific values, (re)turn upper to lower letters
                                                                      "0IS" = "0is")),
                             UICCStage = str_replace_all(UICCStage, "\\|", "I"),      # Replace vertical bar symbol with Roman "One"
                             UICCStage = f_Recode(UICCStage, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "UICCStage"),
                                                                  set_names(Value, OriginalValue))),
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
                             TNM_T = f_Recode(TNM_T, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T"),
                                                          set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             TNM_N = str_to_lower(TNM_N),
                             TNM_N = str_remove_all(TNM_N, " "),
                             TNM_N = str_replace_all(TNM_N, c("x" = "X",
                                                              "x\\(sn\\)" = "X(sn)",
                                                              "0sn" = "0(sn)")),
                             TNM_N = f_Recode(TNM_N, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N"),
                                                          set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             TNM_M = str_to_lower(TNM_M),      # Convert all upper to lower letters
                             TNM_M = str_remove_all(TNM_M, " "),
                             TNM_M = str_replace_all(TNM_M, c("x" = "X")),
                             TNM_M = f_Recode(TNM_M, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M"),
                                                          set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             TNM_T_Prefix = str_to_lower(TNM_T_Prefix),
                             TNM_T_Prefix = str_remove_all(TNM_T_Prefix, " "),
                             TNM_T_Prefix = str_replace_all(TNM_T_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_T_Prefix = f_Recode(TNM_T_Prefix, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T_Prefix"),
                                                                        set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             TNM_N_Prefix = str_to_lower(TNM_N_Prefix),
                             TNM_N_Prefix = str_remove_all(TNM_N_Prefix, " "),
                             TNM_N_Prefix = str_replace_all(TNM_N_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_N_Prefix = f_Recode(TNM_N_Prefix, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N_Prefix"),
                                                                        set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             TNM_M_Prefix = str_to_lower(TNM_M_Prefix),
                             TNM_M_Prefix = str_remove_all(TNM_M_Prefix, " "),
                             TNM_M_Prefix = str_replace_all(TNM_M_Prefix, c("yc" = "c",
                                                                            "yu" = "u",
                                                                            "yp" = "p")),
                             TNM_M_Prefix = f_Recode(TNM_M_Prefix, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M_Prefix"),
                                                                        set_names(Value, OriginalValue))))


df_SDM_Metastasis <- df_SDM_Metastasis %>%
                          mutate(DateMetastasisDiagnosis = as_date(DateMetastasisDiagnosis, format = "%d.%m.%Y"),
                                 #--------------------------------------------------
                                 IsDistantMetastasis = case_match(IsDistantMetastasis,
                                                                  "ja" ~ TRUE,
                                                                  "nein" ~ FALSE),
                                 #--------------------------------------------------
                                 DistantMetastasisLocalization = str_to_upper(DistantMetastasisLocalization),
                                 DistantMetastasisLocalization = str_remove_all(DistantMetastasisLocalization, " "))


df_SDM_Progress <- df_SDM_Progress %>%
                        mutate(DateOfProgress = as_date(DateOfProgress, format = "%d.%m.%Y"),
                               #----------------------------------------------------
                               IsSurgery = case_match(IsSurgery,                    # Recode true and false values
                                                      "true" ~ TRUE,
                                                      "false" ~ FALSE),
                               #----------------------------------------------------
                               SurgeryIntention = f_Recode(SurgeryIntention, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "SurgeryIntention"),
                                                                                  set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               IsRadiotherapy = case_match(IsRadiotherapy,
                                                           "true" ~ TRUE,
                                                           "false" ~ FALSE),
                               #----------------------------------------------------
                               RadiotherapyIntention = f_Recode(RadiotherapyIntention, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyIntention"),
                                                                                            set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               RadiotherapyRelationToSurgery = f_Recode(RadiotherapyRelationToSurgery, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyRelationToSurgery"),
                                                                                                            set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               IsChemotherapy = case_match(IsChemotherapy,
                                                           "true" ~ TRUE,
                                                           "false" ~ FALSE),
                               #----------------------------------------------------
                               ChemotherapyIntention = f_Recode(ChemotherapyIntention, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyIntention"),
                                                                                            set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               ChemotherapyRelationToSurgery = f_Recode(ChemotherapyRelationToSurgery, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyRelationToSurgery"),
                                                                                                            set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               IsImmunotherapy = case_match(IsImmunotherapy,
                                                            "true" ~ TRUE,
                                                            "false" ~ FALSE),
                               #----------------------------------------------------
                               IsHormonetherapy = case_match(IsHormonetherapy,
                                                             "true" ~ TRUE,
                                                             "false" ~ FALSE),
                               #----------------------------------------------------
                               IsBonemarrowtransplant = case_match(IsBonemarrowtransplant,
                                                                   "true" ~ TRUE,
                                                                   "false" ~ FALSE),
                               #----------------------------------------------------
                               TherapeuticResponse = f_Recode(TherapeuticResponse, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "TherapeuticResponse"),
                                                                                        set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               LocalRelapse = f_Recode(LocalRelapse, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LocalRelapse"),
                                                                          set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               LymphnodalRelapse = f_Recode(LymphnodalRelapse, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalRelapse"),
                                                                                    set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               DistantMetastasis = f_Recode(DistantMetastasis, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "DistantMetastasis"),
                                                                                    set_names(Value, OriginalValue))),
                               #----------------------------------------------------
                               IsSyntheticProgressEntry = case_match(IsSyntheticProgressEntry,
                                                                     "true" ~ TRUE,
                                                                     "false" ~ FALSE))


df_SDM_Surgery <- df_SDM_Surgery %>%
                      mutate(SurgeryID = as.integer(str_extract(SurgeryID, "\\d+")),      # Extract integer number from string in SurgeryID. Serves as surrogate for chronological order of events.
                             #------------------------------------------------------
                             ResidualAssessmentLocal = f_Recode(ResidualAssessmentLocal, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal"),
                                                                                              set_names(Value, OriginalValue))),
                             #------------------------------------------------------
                             ResidualAssessmentTotal = f_Recode(ResidualAssessmentTotal, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal"),
                                                                                              set_names(Value, OriginalValue))))


df_SDM_SystemicTherapy <- df_SDM_SystemicTherapy %>%
                              mutate(DateSystemicTherapyStart = as_date(DateSystemicTherapyStart, format = "%d.%m.%Y"),
                                     DateSystemicTherapyEnd = as_date(DateSystemicTherapyEnd, format = "%d.%m.%Y"))


df_SDM_Radiotherapy <- df_SDM_Radiotherapy %>%
                            mutate(DateRadiotherapyStart = as_date(DateRadiotherapyStart, format = "%d.%m.%Y"),
                                   DateRadiotherapyEnd = as_date(DateRadiotherapyEnd, format = "%d.%m.%Y"))


df_SDM_BioSampling <- df_SDM_BioSampling %>%
                          select(-PatientWithSample) %>%
                          mutate(DateSampleTaking = as_date(DateSampleTaking, format = "%d.%m.%Y"),
                                 #--------------------------------------------------
                                 SampleType = f_Recode(SampleType, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType"),
                                                                        set_names(Value, OriginalValue))),
                                 #--------------------------------------------------
                                 SampleTissue = f_Recode(SampleTissue, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleTissue"),
                                                                            set_names(Value, OriginalValue))),
                                 #--------------------------------------------------
                                 FixationType = f_Recode(FixationType, with(dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "FixationType"),
                                                                            set_names(Value, OriginalValue))))


df_SDM_MolecularDiagnostics <- df_SDM_MolecularDiagnostics %>%
                                    mutate(DateMolecularDiagnostics = as_date(DateMolecularDiagnostics, format = "%d.%m.%Y"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values after Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Transformed <- list(df_SDM_Patients,
                                     df_SDM_Diagnosis,
                                     df_SDM_Tumor,
                                     df_SDM_Histology,
                                     df_SDM_Staging,
                                     df_SDM_Metastasis,
                                     df_SDM_Progress,
                                     df_SDM_Surgery,
                                     df_SDM_BioSampling)


ls_Monitors_Transformed <- map2(.x = ls_MonitoredData_Transformed,
                                .y = ls_MonitorFeatures_All,
                                .f = function(inp_df, inp_ls_MonitorFeatures)
                                {
                                    inp_df %>%
                                        f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                               inp_ProcessingStatus = "Transformed")
                                })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finalize Harmonization of Data: Ineligible data (includes data that could not be harmonized) is turned into NA using factor conversion or other methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df_SDM_Patients <- df_SDM_Patients %>%
                        mutate(Sex = factor(Sex, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "Sex")$Value),      # Convert to factor to mark ineligible values as NA and establish level order where appropriate
                               LastVitalStatus = factor(LastVitalStatus, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Patients" & Feature == "LastVitalStatus")$Value))


df_SDM_Tumor <- df_SDM_Tumor %>%
                    mutate(LocalizationSide = factor(LocalizationSide, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Tumor" & Feature == "LocalizationSide")$Value))


df_SDM_Histology <- df_SDM_Histology %>%
                        mutate(Grading = factor(Grading, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading")$Value),
                               GradingLabel = factor(Grading,
                                                     levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading")$Value,
                                                     labels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading")$Label))


df_SDM_Staging <- df_SDM_Staging %>%
                      mutate(UICCStage = factor(UICCStage, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "UICCStage")$Value),
                             TNM_T = factor(TNM_T, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T")$Value),
                             TNM_N = factor(TNM_N, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N")$Value),
                             TNM_M = factor(TNM_M, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M")$Value),
                             TNM_T_Prefix = factor(TNM_T_Prefix, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_T_Prefix")$Value),
                             TNM_N_Prefix = factor(TNM_N_Prefix, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_N_Prefix")$Value),
                             TNM_M_Prefix = factor(TNM_M_Prefix, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Staging" & Feature == "TNM_M_Prefix")$Value))


df_SDM_Progress <- df_SDM_Progress %>%
                        mutate(SurgeryIntention = factor(SurgeryIntention, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "SurgeryIntention")$Value),
                               RadiotherapyIntention = factor(RadiotherapyIntention, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyIntention")$Value),
                               RadiotherapyRelationToSurgery = factor(RadiotherapyRelationToSurgery, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "RadiotherapyRelationToSurgery")$Value),
                               ChemotherapyIntention = factor(ChemotherapyIntention, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyIntention")$Value),
                               ChemotherapyRelationToSurgery = factor(ChemotherapyRelationToSurgery, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "ChemotherapyRelationToSurgery")$Value),
                               TherapeuticResponse = factor(TherapeuticResponse, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "TherapeuticResponse")$Value),
                               LocalRelapse = factor(LocalRelapse, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LocalRelapse")$Value),
                               LymphnodalRelapse = factor(LymphnodalRelapse, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalRelapse")$Value),
                               DistantMetastasis = factor(DistantMetastasis, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Progress" & Feature == "DistantMetastasis")$Value))


df_SDM_Surgery <- df_SDM_Surgery %>%
                      mutate(ResidualAssessmentLocal = factor(ResidualAssessmentLocal, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal")$Value),
                             ResidualAssessmentTotal = factor(ResidualAssessmentTotal, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal")$Value))


df_SDM_BioSampling <- df_SDM_BioSampling %>%
                          mutate(SampleType = factor(SampleType, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType")$Value),
                                 SampleTissue = factor(SampleTissue, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleTissue")$Value),
                                 FixationType = factor(FixationType, levels = dplyr::filter(CCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "FixationType")$Value))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values after Finalized Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Final <- list(df_SDM_Patients,
                               df_SDM_Diagnosis,
                               df_SDM_Tumor,
                               df_SDM_Histology,
                               df_SDM_Staging,
                               df_SDM_Metastasis,
                               df_SDM_Progress,
                               df_SDM_Surgery,
                               df_SDM_BioSampling)


ls_Monitors_Final <- map2(.x = ls_MonitoredData_Final,
                          .y = ls_MonitorFeatures_All,
                          .f = function(inp_df, inp_ls_MonitorFeatures)
                          {
                              inp_df %>%
                                   f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                       inp_ProcessingStatus = "Final")
                          })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Merge Monitor Objects into Coherent Summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ls_MonitorSummaries <- list()

for (i in 1:length(ls_Monitors_Raw))
{
    df_CurrentSummary <- ls_Monitors_Raw[[i]] %>%
                              full_join(ls_Monitors_Transformed[[i]]) %>%
                              full_join(ls_Monitors_Final[[i]]) %>%
                              pivot_wider(names_from = ProcessingStatus,
                                          values_from = Frequency) %>%
                              group_by(Feature, Value, IsValueEligible) %>%
                              summarize(Raw = sum(Raw, na.rm = TRUE),
                                        Transformed = sum(Transformed, na.rm = TRUE),
                                        Final = sum(Final, na.rm = TRUE)) %>%
                              arrange(Feature, desc(IsValueEligible))

    ls_MonitorSummaries <- c(ls_MonitorSummaries, list(df_CurrentSummary))
}


names(ls_MonitorSummaries) <- c("Monitor_df_SDM_Patients",
                                "Monitor_df_SDM_Diagnosis",
                                "Monitor_df_SDM_Tumor",
                                "Monitor_df_SDM_Histology",
                                "Monitor_df_SDM_Staging",
                                "Monitor_df_SDM_Metastasis",
                                "Monitor_df_SDM_Progress",
                                "Monitor_df_SDM_Surgery",
                                "Monitor_df_SDM_BioSampling")

# Monitor_df_SDM_Patients <- ls_MonitorSummaries[[1]]
# Monitor_df_SDM_Diagnosis <- ls_MonitorSummaries[[2]]
# Monitor_df_SDM_Tumor <- ls_MonitorSummaries[[3]]
# Monitor_df_SDM_Histology <- ls_MonitorSummaries[[4]]
# Monitor_df_SDM_Staging <- ls_MonitorSummaries[[5]]
# Monitor_df_SDM_Metastasis <- ls_MonitorSummaries[[6]]
# Monitor_df_SDM_Progress <- ls_MonitorSummaries[[7]]
# Monitor_df_SDM_Surgery <- ls_MonitorSummaries[[8]]
# Monitor_df_SDM_BioSampling <- ls_MonitorSummaries[[9]]



# Save Monitor Objects to make them accessible for Reporting
save(list = "ls_MonitorSummaries",
     file = here::here("Reporting/Monitoring", paste0("MonitorData_", SiteName, ".RData")))



}
