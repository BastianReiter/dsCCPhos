
#' AugmentDataDS
#'
#' @param Name_CurationOutput
#'
#' @return
#' @export
#'
#' @examples
AugmentDataDS <- function(Name_CurationOutput = "CurationOutput")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(Name_CurationOutput))
{
    CurationOutput <- eval(parse(text = Name_CurationOutput), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'Name_CurationOutput' must be specified as a character string"
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
require(tidyr)


# Extract curated data from curation output
# Curated Data Model (CDM) = list of data frames
CuratedData <- CurationOutput$CuratedData

# Extract data frames from list
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augmented Data Model (ADM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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




df_Work_Patient <- df_CDM_Patient %>%
                        left_join(df_CDM_Diagnosis, by = join_by(PatientID)) %>%
                        group_by(PatientID) %>%
                            mutate(CountDifferentDiagnoses = n_distinct(DiagnosisID),
                                   CountDifferentCancers = n_distinct(ICD10Code),
                                   CountDifferences = CountDifferentDiagnoses - CountDifferentCancers) %>%      # Get Counts of different diagnoses and different cancer entities (by ICD-10-Code). These counts should be equal in majority of cases.
                        filter(CountDifferences > 0)




df_Work_Diagnosis <- CuratedData$Diagnosis %>%
                          left_join(CuratedData$Histology, by = join_by(PatientID, DiagnosisID)) %>%
                          group_by(DiagnosisID) %>%
                              mutate(HistologiesPerDiagnosis = n(),
                                     DifferentHistologiesPerTumor = n_distinct(ICDOMorphology, Grading)) %>%
                              arrange(HistologyDate, HistologyID) %>%
                              filter(row_number() == n())      # Keep only the last row of each diagnosis (should be the most recent Histology report)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing of CDM tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_CDM_SystemicTherapy <- df_CDM_SystemicTherapy %>%
                              mutate(SystemicTherapySubclass = case_when((IsChemotherapy == TRUE
                                                                            & IsHormoneTherapy == FALSE
                                                                            & IsImmunotherapy == FALSE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Chemotherapy Mono",
                                                                          (IsChemotherapy == FALSE
                                                                            & IsHormoneTherapy == TRUE
                                                                            & IsImmunotherapy == FALSE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Hormone Therapy Mono",
                                                                          (IsChemotherapy == FALSE
                                                                            & IsHormoneTherapy == FALSE
                                                                            & IsImmunotherapy == TRUE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Immunoherapy Mono",
                                                                          (IsChemotherapy == FALSE
                                                                            & IsHormoneTherapy == FALSE
                                                                            & IsImmunotherapy == FALSE
                                                                            & IsBoneMarrowTransplant == TRUE) ~ "Bone Marrow Transplant",
                                                                          (IsChemotherapy == TRUE
                                                                            & IsHormoneTherapy == TRUE
                                                                            & IsImmunotherapy == FALSE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Hormone Combination",
                                                                          (IsChemotherapy == TRUE
                                                                            & IsHormoneTherapy == FALSE
                                                                            & IsImmunotherapy == TRUE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Immuno Combination",
                                                                          (IsChemotherapy == TRUE
                                                                            & IsHormoneTherapy == FALSE
                                                                            & IsImmunotherapy == FALSE
                                                                            & IsBoneMarrowTransplant == TRUE) ~ "Chemo and BMT",
                                                                          (IsChemotherapy == FALSE
                                                                            & IsHormoneTherapy == TRUE
                                                                            & IsImmunotherapy == TRUE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Hormone/Immuno Combination",
                                                                          (IsChemotherapy == TRUE
                                                                            & IsHormoneTherapy == TRUE
                                                                            & IsImmunotherapy == TRUE
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Hormone/Immuno Combination",
                                                                          IsObservantStrategy == TRUE ~ "Observant Strategy",
                                                                          TRUE ~ "Other"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate df_ADM_Events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Diagnosis-related events


# Initiate df_ADM_Events integrating initial diagnosis event
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_ADM_Events <- df_CDM_Patient %>%
                      left_join(df_CDM_Diagnosis, join_by(PatientID)) %>%
                      group_by(PatientID, DiagnosisID) %>%
                          mutate(EventType = "Point",
                                 EventDate = InitialDiagnosisDate,
                                 EventDateEnd = NULL,      # For events of type "Period"
                                 EventClass = "Diagnosis",
                                 EventSubclass = "Initial diagnosis",
                                 EventRankWithinSubclass = row_number(),
                                 EventOrderSignificance = NULL,
                                 EventDetails = NULL) %>%
                          select(PatientID,
                                 DateOfBirth,
                                 DiagnosisID,
                                 InitialDiagnosisDate,
                                 starts_with("Event"))



# Transform BioSampling data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_BioSampling <- df_CDM_BioSampling %>%
                              group_by(PatientID) %>%
                                  arrange(SampleTakingDate, .by_group = TRUE) %>%
                                  mutate(EventType = "Point",
                                         EventDate = SampleTakingDate,
                                         EventClass = "Diagnostics",
                                         EventSubclass = "Sample Taking",
                                         EventRankWithinSubclass = row_number(),
                                         EventOrderSignificance = case_when(row_number() == 1 ~ "First Sample Taking",
                                                                            row_number() == n() ~ "Last Sample Taking",
                                                                            TRUE ~ NA)) %>%
                                  nest(EventDetails = (c(SampleAliquot,
                                                         SampleType,
                                                         SampleStatus,
                                                         SampleQuantity,
                                                         SampleUnit,
                                                         SampleProjectName))) %>%
                              ungroup() %>%
                              select(PatientID,
                                     starts_with("Event"))



# Transform Histology data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Histology <- df_CDM_Histology %>%
                            group_by(PatientID, DiagnosisID) %>%
                                arrange(HistologyDate, HistologyID, .by_group = TRUE) %>%
                                mutate(EventType = "Point",
                                       EventDate = HistologyDate,
                                       EventClass = "Diagnostics",
                                       EventSubclass = "Histology",
                                       EventRankWithinSubclass = row_number(),
                                       EventOrderSignificance = case_when(row_number() == 1 ~ "First Histology",
                                                                          row_number() == n() ~ "Last Histology",
                                                                          TRUE ~ NA)) %>%
                                nest(EventDetails = c(ICDOMorphology,
                                                      ICDOMorphologyCode,
                                                      ICDOMorphologyVersion,
                                                      Grading)) %>%
                            ungroup() %>%
                            select(PatientID,
                                   DiagnosisID,
                                   starts_with("Event"))



# Transform Metastasis data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Metastasis <- df_CDM_Metastasis %>%
                            group_by(PatientID, DiagnosisID) %>%
                                arrange(MetastasisDiagnosisDate, MetastasisID, .by_group = TRUE) %>%
                                mutate(EventType = "Point",
                                       EventDate = MetastasisDiagnosisDate,
                                       EventClass = "Diagnosis",
                                       EventSubclass = "Metastasis",
                                       EventRankWithinSubclass = row_number(),
                                       EventOrderSignificance = case_when(row_number() == 1 ~ "First Metastasis Diagnosis",
                                                                          row_number() == n() ~ "Last Metastasis Diagnosis",
                                                                          TRUE ~ NA)) %>%
                                nest(EventDetails = c(HasMetastasis,
                                                       MetastasisLocalization)) %>%
                            ungroup() %>%
                            select(PatientID,
                                   DiagnosisID,
                                   starts_with("Event"))



# Transform MolecularDiagnostics data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_MolecularDiagnostics <- df_CDM_MolecularDiagnostics %>%
                                      group_by(PatientID, DiagnosisID) %>%
                                          arrange(MolecularDiagnosticsDate, .by_group = TRUE) %>%
                                          mutate(EventType = "Point",
                                                 EventDate = MolecularDiagnosticsDate,
                                                 EventClass = "Diagnostics",
                                                 EventSubclass = "Molecular Diagnostics",
                                                 EventRankWithinSubclass = row_number(),
                                                 EventOrderSignificance = case_when(row_number() == 1 ~ "First Molecular Diagnostics",
                                                                                    row_number() == n() ~ "Last Molecular Diagnostics",
                                                                                    TRUE ~ NA)) %>%
                                          nest(EventDetails = c(MolecularDiagnosticsName,
                                                                MolecularDiagnosticsStatus,
                                                                MolecularDiagnosticsDocumentation)) %>%
                                      ungroup() %>%
                                      select(PatientID,
                                             DiagnosisID,
                                             starts_with("Event"))



# Transform VitalStatus data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Patient <- df_CDM_Patient %>%
                          group_by(PatientID) %>%
                              mutate(EventType = "Point",
                                     EventDate = LastVitalStatusDate,
                                     EventClass = "Vital Status",
                                     EventSubclass = "Vital Status") %>%
                              nest(EventDetails = c(LastVitalStatus,
                                                    DeathCancerRelated,
                                                    CausesOfDeath)) %>%
                          ungroup() %>%
                          select(PatientID,
                                 starts_with("Event"))



# Transform Progress data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Progress <- df_CDM_Progress %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(ProgressReportDate, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = ProgressReportDate,
                                     EventClass = "Diagnosis",
                                     EventSubclass = "Progress",
                                     EventRankWithinSubclass = row_number(),
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Progress Report",
                                                                        row_number() == n() ~ "Last Progress Report",
                                                                        TRUE ~ NA)) %>%
                              nest(EventDetails = (c(GlobalStatus,
                                                     LocalRelapseDate,
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
                                      arrange(RadiationTherapyStart, .by_group = TRUE) %>%
                                      mutate(EventType = "Period",
                                             EventDate = RadiationTherapyStart,
                                             EventDateEnd = RadiationTherapyEnd,
                                             EventClass = "Therapy",
                                             EventSubclass = "Radiation Therapy",
                                             EventRankWithinSubclass = row_number(),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First Radiation Period",
                                                                                row_number() == n() ~ "Last Radiation Period",
                                                                                TRUE ~ NA)) %>%
                                      nest(EventDetails = c(RadiationTherapyIntention,
                                                            RadiationTherapyRelationToSurgery)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))



# Transform Staging data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Staging <- df_CDM_Staging %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(StagingReportDate, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = StagingReportDate,
                                     EventClass = "Diagnosis",
                                     EventSubclass = "Staging",
                                     EventRankWithinSubclass = row_number(),
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Staging",
                                                                        row_number() == n() ~ "Last Staging",
                                                                        TRUE ~ NA)) %>%
                              nest(EventDetails = (c(UICCStage,
                                                     TNM_T_Prefix,
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



# Transform Surgery data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Surgery <- df_CDM_Surgery %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(SurgeryDate, SurgeryID, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = SurgeryDate,
                                     EventClass = "Therapy",
                                     EventSubclass = "Surgery",
                                     EventRankWithinSubclass = row_number(),
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Surgery",
                                                                        row_number() == n() ~ "Last Surgery",
                                                                        TRUE ~ NA)) %>%
                              nest(EventDetails = (c(SurgeryIntention,
                                                     OPSCode,
                                                     ResidualAssessmentLocal,
                                                     ResidualAssessmentTotal))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))



# Transform SystemicTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_SystemicTherapy <- df_CDM_SystemicTherapy %>%
                                  group_by(PatientID, DiagnosisID, SystemicTherapySubclass) %>%
                                      arrange(SystemicTherapyStart, .by_group = TRUE) %>%
                                      mutate(EventType = "Period",
                                             EventDate = SystemicTherapyStart,
                                             EventDateEnd = SystemicTherapyEnd,
                                             EventClass = "Therapy",
                                             EventSubclass = SystemicTherapySubclass,
                                             EventRankWithinSubclass = row_number(),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First in Systemic Therapy Subclass",
                                                                                row_number() == n() ~ "Last in Systemic Therapy Subclass",
                                                                                TRUE ~ NA)) %>%
                                      nest(EventDetails = c(SystemicTherapyIntention,
                                                            SystemicTherapySubstances,
                                                            SystemicTherapyRelationToSurgery)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))


df_ADM_Events <- df_ADM_Events %>%
                      bind_rows(df_Events_BioSampling,
                                df_Events_Histology,
                                df_Events_Metastasis,
                                df_Events_MolecularDiagnostics,
                                df_Events_Patient,
                                df_Events_Progress,
                                df_Events_RadiationTherapy,
                                df_Events_Staging,
                                df_Events_Surgery,
                                df_Events_SystemicTherapy) %>%
                      group_by(DiagnosisID) %>%
                          arrange(EventDate, .by_group = TRUE) %>%
                          fill(DateOfBirth,
                               InitialDiagnosisDate) %>%
                          mutate(EventRank = row_number(),
                                 EventDaysSinceDiagnosis = round(as.numeric(difftime(EventDate, InitialDiagnosisDate, units = "days")), digits = 1),
                                 EventPatientAge = floor(time_length(difftime(EventDate, DateOfBirth), unit = "years"))) %>%
                      select(PatientID,
                             DateOfBirth,
                             DiagnosisID,
                             InitialDiagnosisDate,
                             EventRank,
                             EventType,
                             EventDate,
                             EventDateEnd,
                             EventPatientAge,
                             EventDaysSinceDiagnosis,
                             EventClass,
                             EventSubclass,
                             EventRankWithinSubclass,
                             EventOrderSignificance,
                             EventDetails)


return(list(ADM_Events = df_ADM_Events))
}


