
#' AugmentDataDS
#'
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param Name_CurationOutput String | Name of the list object created by CurateDataDS() | Default: 'CurationOutput'
#'
#' @return
#' @export
#'
#' @examples
#' @author Bastian Reiter
AugmentDataDS <- function(Name_CurationOutput = "CurationOutput")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   SETUP
#     - Evaluation and parsing of input
#     - Loading of required package namespaces
#     - Unpacking Curated Data Set (CDS)
#
#   MODUL 1)  Creation of df_ADS_Events
#       - Diagnosis-related
#       - Patient-related
#
#   MODUL 2)  Creation of df_ADS_Diagnosis
#       - Consolidate information from df_ADS_Events
#
#   MODUL 3)  Creation of df_ADS_Patient
#       - Consolidate information from df_ADS_Events and df_ADS_Diagnosis
#


#
#
#   Return statement


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


# Extract Curated Data Set (list of data frames) from curation output
CuratedDataSet <- CurationOutput$CuratedDataSet

# Extract data frames from list
df_CDS_BioSampling <- CuratedDataSet$BioSampling
df_CDS_Diagnosis <- CuratedDataSet$Diagnosis
df_CDS_Histology <- CuratedDataSet$Histology
df_CDS_Metastasis <- CuratedDataSet$Metastasis
df_CDS_MolecularDiagnostics <- CuratedDataSet$MolecularDiagnostics
df_CDS_Patient <- CuratedDataSet$Patient
df_CDS_Progress <- CuratedDataSet$Progress
df_CDS_RadiationTherapy <- CuratedDataSet$RadiationTherapy
df_CDS_Staging <- CuratedDataSet$Staging
df_CDS_Surgery <- CuratedDataSet$Surgery
df_CDS_SystemicTherapy <- CuratedDataSet$SystemicTherapy


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augmented Data Model (ADM)
# Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ________________
#                     / df_ADS_Patient \
#                     \________________/
#
#                      __________________
#                     / df_ADS_Diagnosis \
#                     \__________________/
#
#                      _______________
#                     / df_ADS_Events \
#                     \_______________/



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing of CDS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_CDS_SystemicTherapy <- df_CDS_SystemicTherapy %>%
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
                                                                            & IsBoneMarrowTransplant == FALSE) ~ "Immunotherapy Mono",
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
# MODUL 1)  Generate df_ADS_Events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Diagnosis-related events


# Set up progress bar
CountProgressItems <- 14
ProgressBar <- progress_bar$new(format = "Generating diagnosis-related events [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
ProgressBar$tick()



# Initiate df_ADS_Events, integrating patient-specific and initial diagnosis events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiation 1: Initial diagnosis event
df_ADS_Events <- df_CDS_Patient %>%
                      right_join(df_CDS_Diagnosis, join_by(PatientID)) %>%
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
ProgressBar$tick()


# Initiation 2: Last known vital status
df_Events_LastVitalStatus <- df_CDS_Patient %>%
                                  right_join(df_CDS_Diagnosis, join_by(PatientID)) %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      mutate(EventType = "Point",
                                             EventDate = LastVitalStatusDate,
                                             EventClass = "Vital Status",
                                             EventSubclass = case_when(LastVitalStatus == "Deceased" ~ "Death",
                                                                       LastVitalStatus == "Alive" ~ "Alive",
                                                                       LastVitalStatus == NA ~ "Unknown")) %>%
                                      nest(EventDetails = c(DeathCancerRelated,
                                                            CausesOfDeath)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DateOfBirth,
                                         DiagnosisID,
                                         InitialDiagnosisDate,
                                         starts_with("Event"))
ProgressBar$tick()


# Initiation 3: Row-bind data frames from Initiation 1 and 2
df_ADS_Events <- df_ADS_Events %>%
                      bind_rows(df_Events_LastVitalStatus)
ProgressBar$tick()



# Transform BioSampling data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_BioSampling <- df_CDS_BioSampling %>%
                              group_by(PatientID) %>%
                                  arrange(SampleTakingDate, .by_group = TRUE) %>%
                                  mutate(EventType = "Point",
                                         EventDate = SampleTakingDate,,
                                         EventClass = "Diagnostics",
                                         EventSubclass = "Sample Taking",
                                         EventRankWithinSubclass = row_number(),
                                         EventOrderSignificance = case_when(row_number() == 1 ~ "First Sample Taking",
                                                                            row_number() == n() ~ "Last Sample Taking",
                                                                            TRUE ~ NA)) %>%
                                  nest(EventDetails = c(SampleAliquot,
                                                        SampleType,
                                                        SampleStatus,
                                                        SampleQuantity,
                                                        SampleUnit,
                                                        SampleProjectName)) %>%
                              ungroup() %>%
                              select(PatientID,
                                     starts_with("Event"))
ProgressBar$tick()



# Transform Histology data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Histology <- df_CDS_Histology %>%
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
                                nest(EventDetails = c(ICDOMorphologyCode,
                                                      ICDOMorphologyVersion,
                                                      Grading,
                                                      ICDOMorphologyComment)) %>%
                            ungroup() %>%
                            select(PatientID,
                                   DiagnosisID,
                                   starts_with("Event"))
ProgressBar$tick()



# Transform Metastasis data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Metastasis <- df_CDS_Metastasis %>%
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
ProgressBar$tick()



# Transform MolecularDiagnostics data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>%
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
ProgressBar$tick()



# Transform Progress data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Progress <- df_CDS_Progress %>%
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
ProgressBar$tick()



# Transform RadiationTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_RadiationTherapy <- df_CDS_RadiationTherapy %>%
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
ProgressBar$tick()



# Transform Staging data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Staging <- df_CDS_Staging %>%
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
ProgressBar$tick()



# Transform Surgery data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Surgery <- df_CDS_Surgery %>%
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
ProgressBar$tick()



# Transform SystemicTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_SystemicTherapy <- df_CDS_SystemicTherapy %>%
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
ProgressBar$tick()



# Consolidate Event-oriented data from CDS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_ADS_Events <- df_ADS_Events %>%
                      bind_rows(df_Events_BioSampling,
                                df_Events_Histology,
                                df_Events_Metastasis,
                                df_Events_MolecularDiagnostics,
                                df_Events_Progress,
                                df_Events_RadiationTherapy,
                                df_Events_Staging,
                                df_Events_Surgery,
                                df_Events_SystemicTherapy) %>%
                      group_by(PatientID, DiagnosisID) %>%
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
ProgressBar$tick()
ProgressBar$terminate()



# Test <- df_ADS_Events %>%
#             unnest(cols = c(EventDetails))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODUL 2)  Generate df_ADS_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df_Events_Summary <- df_ADS_Events %>%
#                           group_by(DiagnosisID) %>%
#                               mutate(TimeDiagnosisToDeath = ifelse())


# Construct diagnosis-related endpoints for clinical outcome measures from event history
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   - Overall survival (OS)
#   -
#   - Progression-free survival (PFS)
#   - Disease-free survival (DFS)
#
#   - Overall response rate

#   - Disease specific survival (Death from disease or from treatment)
#
#

df_DiagnosisSummary_Histology <- df_CDS_Histology %>%
                                      group_by(DiagnosisID) %>%
                                      summarize(CountSubdiagnoses = n_distinct(SubDiagnosisID),
                                                CountHistologyReports = n_distinct(HistologyID))


df_ADS_Diagnosis <- df_CDS_Diagnosis %>%
                        left_join(df_DiagnosisSummary_Histology, by = join_by(DiagnosisID))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODUL 3)  Generate df_ADS_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df_PatientSummary_Diagnosis <- df_CDS_Diagnosis %>%
                                    group_by(PatientID) %>%
                                    summarize(CountDiagnoses = n_distinct(DiagnosisID))


df_ADS_Patient <- df_CDS_Patient %>%
                       left_join(df_PatientSummary_Diagnosis, by = join_by(PatientID))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RETURN STATEMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return(list(ADS_Patient = df_ADS_Patient,
            ADS_Diagnosis = df_ADS_Diagnosis,
            ADS_Events = df_ADS_Events))
}


