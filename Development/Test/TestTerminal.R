

library(dsCCPhos)

# Load test data into WD
load("./Development/Data/RealData/CCPTestData_Total.RData")

# Create virtual data base with test data
DBConnection <- MakeTestDB(CCPTestData_Total)

# Load raw test data from data base into WD
RawData <- LoadRawData(DBConnection)



#===============================================================================
# Augmented Data Model (ADM)
#===============================================================================
#                      ________________
#                     / df_ADM_Patient \
#                     \________________/
#
#                      __________________
#                     / df_ADM_Diagnosis \
#                     \__________________/
#
#                      _______________
#                     / df_ADM_Events \
#                     \_______________/




# Use require() to load package namespaces
require(dplyr)
require(lubridate)
require(purrr)
require(stringr)
require(tidyr)


CuratedData <- dsCCPhos::CurateDataDS("RawData")$CuratedData

df_CDM_BioSampling <- CuratedData$BioSampling
df_CDM_Diagnosis <- CuratedData$Diagnosis
df_CDM_Histology <- CuratedData$Histology
df_CDM_Metastasis <- CuratedData$Metastasis
df_CDM_MolecularDiagnostics <- CuratedData$MolecularDiagnostics
df_CDM_Patient <- CuratedData$Patient
df_CDM_Progress <- CuratedData$Progress
df_CDM_RadiationTherapy <- CuratedData$RadiationTherapy
df_CDM_Staging <- CuratedData$Staging
df_CDM_Surgery <- CuratedData$Surgery
df_CDM_SystemicTherapy <- CuratedData$SystemicTherapy



# Initiate df_ADM_Patient
df_ADM_Patient <- CuratedData$Patient


df_Work <- df_ADM_Patient %>%
                left_join(CuratedData$Diagnosis, by = join_by(PatientID)) %>%
                group_by(PatientID) %>%
                    mutate(CountDifferentDiagnoses = n_distinct(DiagnosisID),
                           CountDifferentCancers = n_distinct(ICD10Code))      # Get Counts of different diagnoses and different cancer entities (by ICD-10-Code). These counts should be equal in majority of cases.



df_Work_Tumor <- CuratedData$Diagnosis %>%
                      left_join(CuratedData$Histology, by = join_by(PatientID, DiagnosisID)) %>%
                      group_by(DiagnosisID) %>%
                          mutate(HistologiesPerDiagnosis = n(),
                                 DifferentHistologiesPerTumor = n_distinct(ICDOMorphology, Grading)) %>%
                          arrange(HistologyDate, HistologyID) %>%
                          filter(row_number() == n())      # Keep only the last row of each diagnosis (should be the most recent Histology report)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate df_ADM_Events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


# Initiate df_ADM_Events integrating initial diagnosis event
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_ADM_Events <- df_CDM_Patient %>%
                      left_join(df_CDM_Diagnosis, join_by(PatientID)) %>%
                      mutate(EventType = "Point",
                             EventDate = InitialDiagnosisDate,
                             EventDateEnd = NULL,      # For events of type "Period"
                             EventClass = "Diagnosis",
                             EventSubclass = "Initial diagnosis",
                             EventOrderSignificance = NULL,
                             EventFeature_A = NULL,
                             EventFeature_B = NULL,
                             EventFeature_C = NULL,
                             EventDetails = NULL) %>%
                      select(PatientID,
                             DateOfBirth,
                             DiagnosisID,
                             InitialDiagnosisDate,
                             starts_with("Event"))



# Transform Staging data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Staging <- df_CDM_Staging %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(StagingReportDate) %>%
                              mutate(EventType = "Point",
                                     EventDate = StagingReportDate,
                                     EventClass = "Diagnosis",
                                     EventSubclass = "Staging",
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Staging",
                                                                        row_number() == n() ~ "Last Staging",
                                                                        TRUE ~ NA),
                                     EventFeature_A = UICCStage,
                                     EventFeature_B = paste0("T", TNM_T, "N", TNM_N, "M", TNM_M),
                                     EventFeature_C = NULL) %>%
                              nest(EventDetails = (c(TNM_T_Prefix,
                                                     TNM_T,
                                                     TNM_N_Prefix,
                                                     TNM_N,
                                                     TNM_M_Prefix,
                                                     TNM_M,
                                                     TNM_mSymbol,
                                                     TNM_rSymbol,
                                                     TNM_ySymbol,
                                                     TNMVersion))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))



# Transform Progress data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Progress <- df_CDM_Progress %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(ProgressReportDate) %>%
                              mutate(EventType = "Point",
                                     EventDate = ProgressReportDate,
                                     EventClass = "Diagnosis",
                                     EventSubclass = "Progress",
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Progress Report",
                                                                        row_number() == n() ~ "Last Progress Report",
                                                                        TRUE ~ NA),
                                     EventFeature_A = GlobalStatus,
                                     EventFeature_B = NULL,
                                     EventFeature_C = NULL) %>%
                              nest(EventDetails = (c(LocalRelapseDate,
                                                     LocalStatus,
                                                     LymphnodalStatus,
                                                     MetastasisStatus))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))



# Transform RadiationTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_RadiationTherapy <- df_CDM_RadiationTherapy %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      arrange(RadiationTherapyStart) %>%
                                      mutate(EventType = "Period",
                                             EventDate = RadiationTherapyStart,
                                             EventDateEnd = RadiationTherapyEnd,
                                             EventClass = "Therapy",
                                             EventSubclass = "Radiation Therapy",
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First Radiation Period",
                                                                                row_number() == n() ~ "Last Radiation Period",
                                                                                TRUE ~ NA),
                                             EventFeature_A = RadiationTherapyIntention,
                                             EventFeature_B = RadiationTherapyRelationToSurgery,
                                             EventFeature_C = NULL,
                                             EventDetails = NULL) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))



# Transform Surgery data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Surgery <- df_CDM_Surgery %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(SurgeryDate) %>%
                              mutate(EventType = "Point",
                                     EventDate = SurgeryDate,
                                     EventClass = "Therapy",
                                     EventSubclass = "Surgery",
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Surgery",
                                                                        row_number() == n() ~ "Last Surgery",
                                                                        TRUE ~ NA),
                                     EventFeature_A = SurgeryIntention,
                                     EventFeature_B = OPSCode,
                                     EventFeature_C = NULL) %>%
                              nest(EventDetails = (c(ResidualAssessmentLocal,
                                                     ResidualAssessmentTotal))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))



# Transform SystemicTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_SystemicTherapy <- df_CDM_SystemicTherapy %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      arrange(SystemicTherapyStart) %>%
                                      mutate(EventType = "Period",
                                             EventDate = SystemicTherapyStart,
                                             EventDateEnd = SystemicTherapyEnd,
                                             EventClass = "Therapy",
                                             EventSubclass = case_when(IsChemotherapy == TRUE ~ "Chemotherapy",
                                                                       IsHormoneTherapy == TRUE ~ "Hormone Therapy",
                                                                       IsImmunotherapy == TRUE ~ "Immunotherapy",
                                                                       IsBoneMarrowTransplant == TRUE ~ "Bone Marrow Transplant",
                                                                       IsObservantStrategy == TRUE ~ "Observant Strategy",
                                                                       TRUE ~ "Unknown"),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First Systemic Therapy Period",
                                                                                row_number() == n() ~ "Last Systemic Therapy Period",
                                                                                TRUE ~ NA),
                                             EventFeature_A = SystemicTherapyIntention,
                                             EventFeature_B = SystemicTherapySubstances,
                                             EventFeature_C = SystemicTherapyRelationToSurgery,
                                             EventDetails = NULL) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))

