
#' AugmentDataDS
#'
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param CuratedDataSetName.S String | Name of the Curated Data Set object on server | Default: 'CuratedDataSet'
#'
#' @return A list containing the following objects:
#'         \itemize{\item AugmentedDataSet (list)
#'                  \item AugmentationReport (list)
#'                  \item AugmentationMessages (list)}
#' @export
#' @author Bastian Reiter
AugmentDataDS <- function(CuratedDataSetName.S = "CuratedDataSet")
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
#   MODULE 1)  Creation of df_ADS_Events
#       - Diagnosis-related
#       - Patient-related
#
#   MODULE 2)  Creation of df_ADS_Diagnoses
#       - Consolidate information from df_ADS_Events
#
#   MODULE 3)  Creation of df_ADS_Patients
#       - Consolidate information from df_ADS_Events and df_ADS_Diagnoses
#
#   Return statement


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(CuratedDataSetName.S))
{
    CuratedDataSet <- eval(parse(text = CuratedDataSetName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'CuratedDataSetName.S' must be specified as a character string"
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


# Suppress summarize info messages
options(dplyr.summarise.inform = FALSE)

# Initiate output objects
ls_AugmentedDataSet <- NULL
ls_AugmentationReport <- NULL

# Initiate Messaging objects
Messages <- list()
Messages$CheckAugmentationCompletion <- "red"
Messages$FinalMessage <- "Augmentation not completed"


# Use tryCatch to catch warnings and errors
# Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
# tryCatch({


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
#                     / df_ADS_Patients \
#                     \________________/
#
#                      __________________
#                     / df_ADS_Diagnoses \
#                     \__________________/
#
#                      _______________
#                     / df_ADS_Events \
#                     \_______________/



# Temporary !!!

df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
                        filter(IsReferenceEntry == TRUE)



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
# MODULE 1)  Generate df_ADS_Events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- 14
ProgressBar <- progress_bar$new(format = "Generating diagnosis-related events [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------



# Initiate df_ADS_Events, integrating patient-specific and initial diagnosis events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiation 1: Initial diagnosis event
df_ADS_Events <- df_CDS_Patient %>%
                      right_join(df_CDS_Diagnosis, join_by(PatientID)) %>%
                      group_by(PatientID, DiagnosisID) %>%
                          mutate(EventType = "Point",
                                 EventDate = DiagnosisDate,
                                 EventDateEnd = NULL,      # For events of type "Period"
                                 EventDateIsAdjusted = FALSE,      # In case event date is adjusted later for plausibility reasons
                                 EventClass = "Diagnosis",
                                 EventSubclass = "InitialDiagnosis",
                                 EventRankWithinSubclass = row_number(),
                                 EventOrderSignificance = NULL) %>%
                          nest(EventDetails = c(ICD10Code,
                                                ICDOTopographyCode,
                                                ICDOMorphologyCode,
                                                Grading)) %>%
                          select(PatientID,
                                 DateOfBirth,
                                 DiagnosisID,
                                 DiagnosisDate,
                                 starts_with("Event"))
                          #--- Update PB ---
                          try(ProgressBar$tick())


# Initiation 2: Last known vital status
df_Events_LastVitalStatus <- df_CDS_Patient %>%
                                  right_join(df_CDS_Diagnosis, join_by(PatientID)) %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      mutate(EventType = "Point",
                                             EventDate = LastVitalStatusDate,
                                             EventDateIsAdjusted = FALSE,
                                             EventClass = "VitalStatus",
                                             EventSubclass = case_when(NA ~ "Unknown",
                                                                       .default = LastVitalStatus)) %>%
                                      nest(EventDetails = c(DeathCancerRelated,
                                                            CausesOfDeath)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DateOfBirth,
                                         DiagnosisID,
                                         DiagnosisDate,
                                         starts_with("Event"))
                                  #--- Update PB ---
                                  try(ProgressBar$tick())


# Initiation 3: Row-bind data frames from Initiation 1 and 2
df_ADS_Events <- df_ADS_Events %>%
                      bind_rows(df_Events_LastVitalStatus)
try(ProgressBar$tick())



# Transform BioSampling data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_BioSampling <- NULL

if (nrow(df_CDS_BioSampling) > 0)
{
    df_Events_BioSampling <- df_CDS_BioSampling %>%
                                  group_by(PatientID) %>%
                                      arrange(BioSamplingDate, .by_group = TRUE) %>%
                                      mutate(EventType = "Point",
                                             EventDate = BioSamplingDate,
                                             EventDateIsAdjusted = FALSE,
                                             EventClass = "Diagnostics",
                                             EventSubclass = "Sample Taking",
                                             EventRankWithinSubclass = row_number(),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First Sample Taking",
                                                                                row_number() == n() ~ "Last Sample Taking",
                                                                                TRUE ~ NA)) %>%
                                      nest(EventDetails = c(Aliquot,
                                                            Type,
                                                            Status,
                                                            Quantity,
                                                            Unit,
                                                            ProjectName)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         starts_with("Event"))
}
#--- Update PB ---
try(ProgressBar$tick())



# Transform Histology data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Histology <- df_CDS_Histology %>%
                            group_by(PatientID, DiagnosisID) %>%
                                arrange(HistologyDate, HistologyID, .by_group = TRUE) %>%
                                mutate(EventType = "Point",
                                       EventDate = HistologyDate,
                                       EventDateIsAdjusted = FALSE,
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
                            #--- Update PB ---
                            try(ProgressBar$tick())



# Transform Metastasis data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Metastasis <- df_CDS_Metastasis %>%
                            group_by(PatientID, DiagnosisID) %>%
                                arrange(MetastasisDate, MetastasisID, .by_group = TRUE) %>%
                                mutate(EventType = "Point",
                                       EventDate = MetastasisDate,
                                       EventDateIsAdjusted = FALSE,
                                       EventClass = "Diagnosis",
                                       EventSubclass = "Metastasis",
                                       EventRankWithinSubclass = row_number(),
                                       EventOrderSignificance = case_when(row_number() == 1 ~ "First Metastasis Diagnosis",
                                                                          row_number() == n() ~ "Last Metastasis Diagnosis",
                                                                          TRUE ~ NA)) %>%
                                nest(EventDetails = c(HasMetastasis,
                                                      Localization)) %>%
                            ungroup() %>%
                            select(PatientID,
                                   DiagnosisID,
                                   starts_with("Event"))
                            #--- Update PB ---
                            try(ProgressBar$tick())



# Transform MolecularDiagnostics data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_MolecularDiagnostics <- NULL

if (nrow(df_CDS_MolecularDiagnostics) > 0)
{
    df_Events_MolecularDiagnostics <- df_CDS_MolecularDiagnostics %>%
                                          group_by(PatientID, DiagnosisID) %>%
                                              arrange(MolecularDiagnosticsDate, .by_group = TRUE) %>%
                                              mutate(EventType = "Point",
                                                     EventDate = MolecularDiagnosticsDate,
                                                     EventDateIsAdjusted = FALSE,
                                                     EventClass = "Diagnostics",
                                                     EventSubclass = "Molecular Diagnostics",
                                                     EventRankWithinSubclass = row_number(),
                                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Molecular Diagnostics",
                                                                                        row_number() == n() ~ "Last Molecular Diagnostics",
                                                                                        TRUE ~ NA)) %>%
                                              nest(EventDetails = c(Name,
                                                                    Status,
                                                                    Documentation)) %>%
                                          ungroup() %>%
                                          select(PatientID,
                                                 DiagnosisID,
                                                 starts_with("Event"))
}
#--- Update PB ---
try(ProgressBar$tick())



# Transform Progress data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Progress <- df_CDS_Progress %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(ProgressDate, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = ProgressDate,
                                     EventDateIsAdjusted = FALSE,
                                     EventClass = "Diagnosis",
                                     EventSubclass = "Progress",
                                     EventRankWithinSubclass = row_number(),
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Progress Report",
                                                                        row_number() == n() ~ "Last Progress Report",
                                                                        TRUE ~ NA)) %>%
                              nest(EventDetails = (c(GlobalStatus,
                                                     PrimarySiteStatus,
                                                     LymphnodalStatus,
                                                     MetastasisStatus))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))
                          #--- Update PB ---
                          try(ProgressBar$tick())



# Transform RadiationTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_RadiationTherapy <- df_CDS_RadiationTherapy %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      arrange(RadiationTherapyStartDate, .by_group = TRUE) %>%
                                      mutate(EventType = "Period",
                                             EventDate = RadiationTherapyStartDate,
                                             EventDateEnd = RadiationTherapyEndDate,
                                             EventDateIsAdjusted = FALSE,
                                             EventClass = "Therapy",
                                             EventSubclass = "Radiation Therapy",
                                             EventRankWithinSubclass = row_number(),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First Radiation Period",
                                                                                row_number() == n() ~ "Last Radiation Period",
                                                                                TRUE ~ NA)) %>%
                                      nest(EventDetails = c(Intention,
                                                            RelationToSurgery)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))
                                  #--- Update PB ---
                                  try(ProgressBar$tick())



# Transform Staging data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Staging <- df_CDS_Staging %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(StagingDate, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = StagingDate,
                                     EventDateIsAdjusted = FALSE,
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
                                                     TNMVersion,
                                                     TNM_L,
                                                     TNM_V,
                                                     TNM_Pn,
                                                     TNM_S))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))
                          #--- Update PB ---
                          try(ProgressBar$tick())



# Transform Surgery data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_Surgery <- df_CDS_Surgery %>%
                          group_by(PatientID, DiagnosisID) %>%
                              arrange(SurgeryDate, SurgeryID, .by_group = TRUE) %>%
                              mutate(EventType = "Point",
                                     EventDate = SurgeryDate,
                                     EventDateIsAdjusted = FALSE,
                                     EventClass = "Therapy",
                                     EventSubclass = "Surgery",
                                     EventRankWithinSubclass = row_number(),
                                     EventOrderSignificance = case_when(row_number() == 1 ~ "First Surgery",
                                                                        row_number() == n() ~ "Last Surgery",
                                                                        TRUE ~ NA)) %>%
                              nest(EventDetails = (c(Intention,
                                                     OPSCode,
                                                     ResidualAssessmentLocal,
                                                     ResidualAssessmentTotal))) %>%
                          ungroup() %>%
                          select(PatientID,
                                 DiagnosisID,
                                 starts_with("Event"))
                          #--- Update PB ---
                          try(ProgressBar$tick())



# Transform SystemicTherapy data into Event-oriented data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Events_SystemicTherapy <- df_CDS_SystemicTherapy %>%
                                  group_by(PatientID, DiagnosisID, SystemicTherapySubclass) %>%
                                      arrange(SystemicTherapyStartDate, .by_group = TRUE) %>%
                                      mutate(EventType = "Period",
                                             EventDate = SystemicTherapyStartDate,
                                             EventDateEnd = SystemicTherapyEndDate,
                                             EventDateIsAdjusted = FALSE,
                                             EventClass = "Therapy",
                                             EventSubclass = SystemicTherapySubclass,
                                             EventRankWithinSubclass = row_number(),
                                             EventOrderSignificance = case_when(row_number() == 1 ~ "First in Systemic Therapy Subclass",
                                                                                row_number() == n() ~ "Last in Systemic Therapy Subclass",
                                                                                TRUE ~ NA)) %>%
                                      nest(EventDetails = c(Intention,
                                                            Substances,
                                                            RelationToSurgery)) %>%
                                  ungroup() %>%
                                  select(PatientID,
                                         DiagnosisID,
                                         starts_with("Event"))
                                  #--- Update PB ---
                                  try(ProgressBar$tick())



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
                      group_by(PatientID) %>%
                          fill(DateOfBirth,
                               .direction = "downup") %>%
                      group_by(PatientID, DiagnosisID) %>%
                          filter(!is.na(EventDate)) %>%
                          arrange(EventDate, .by_group = TRUE) %>%
                          # Important adjustment!
                          mutate(FirstEventDate = min(EventDate, na.rm = TRUE),
                                 LastEventDate = max(EventDate, na.rm = TRUE),
                                 EventDateIsAdjusted = case_when(EventSubclass == "InitialDiagnosis" & EventDate > FirstEventDate ~ TRUE,
                                                                 EventClass == "VitalStatus" & EventDate < LastEventDate ~ TRUE,
                                                                 .default = FALSE),
                                 EventDate = case_when(EventSubclass == "InitialDiagnosis" ~ FirstEventDate,
                                                       EventClass == "VitalStatus" ~ LastEventDate,
                                                       .default = EventDate),
                                 InitialDiagnosisDate = FirstEventDate,
                                 EventRank = row_number(),
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
                             EventDateIsAdjusted,
                             EventPatientAge,
                             EventDaysSinceDiagnosis,
                             EventClass,
                             EventSubclass,
                             EventRankWithinSubclass,
                             EventOrderSignificance,
                             EventDetails) %>%
                      ungroup()
                      #--- Update PB ---
                      try(ProgressBar$tick())

#--- Terminate PB ---
try(ProgressBar$terminate())



#Temporary
# UnnestedEvents <- df_ADS_Events %>%
#                       unnest(cols = c(EventDetails), keep_empty = TRUE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 2)  Generate df_ADS_Diagnoses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- 4
ProgressBar <- progress_bar$new(format = "Composing diagnosis-specific data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------


# Summarize diagnosis-specific event data using dsCCPhos::SummarizeEventData()
df_Aux_DiagnosisSummary_Events <- df_ADS_Events %>%
                                      unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                                      group_by(PatientID, DiagnosisID) %>%
                                          group_modify(~ SummarizeEventData(EventEntries = .x,
                                                                            ProgressBarObject = NULL)) %>%
                                      ungroup()
                                      #--- Update PB ---
                                      try(ProgressBar$tick())


df_Aux_DiagnosisData <- df_CDS_Diagnosis %>%
                            left_join(df_CDS_Staging, by = join_by(PatientID, DiagnosisID, SubDiagnosisID), suffix = c("_Diagnosis", "_Staging")) %>%
                            group_by(DiagnosisID) %>%
                                arrange(DiagnosisDate) %>%
                                slice_head() %>%
                            ungroup()
                            #--- Update PB ---
                            try(ProgressBar$tick())


df_ADS_Diagnoses <- df_Aux_DiagnosisData %>%
                        left_join(df_Aux_DiagnosisSummary_Events, by = join_by(PatientID, DiagnosisID)) %>%
                        #filter(is.na(TimeDiagnosisToDeath) | TimeDiagnosisToDeath >= 0) %>%
                        ungroup()
                        #--- Update PB ---
                        try(ProgressBar$tick())

#--- Terminate PB ---
try(ProgressBar$terminate())



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

# df_DiagnosisSummary_Histology <- df_CDS_Histology %>%
#                                       group_by(DiagnosisID) %>%
#                                       summarize(CountSubdiagnoses = n_distinct(SubDiagnosisID),
#                                                 CountHistologyReports = n_distinct(HistologyID))
#
#
# df_ADS_Diagnoses <- df_CDS_Diagnosis %>%
#                         left_join(df_DiagnosisSummary_Histology, by = join_by(DiagnosisID))
#
#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 3)  Generate df_ADS_Patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- 3
ProgressBar <- progress_bar$new(format = "Composing patient-specific data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------



df_Aux_PatientSummary_Diagnosis <- df_CDS_Diagnosis %>%
                                        group_by(PatientID) %>%
                                            summarize(CountDiagnoses = n_distinct(DiagnosisID)) %>%
                                        ungroup()
                                        #--- Update PB ---
                                        try(ProgressBar$tick())



# !!! TEMPORARY !!! (For easier testing, slice is performed to filter for only one diagnosis per patient)
df_ADS_Patients <- df_CDS_Patient %>%
                        left_join(df_Aux_PatientSummary_Diagnosis, by = join_by(PatientID)) %>%
                        left_join(df_ADS_Diagnoses, by = join_by(PatientID)) %>%      # <--- TEMPORARY: Joining with ADS_Diagnoses
                        group_by(PatientID) %>%
                            arrange(DiagnosisDate) %>%
                            slice_head() %>%      # <--- TEMPORARY: Slice performed
                        ungroup()
                        #--- Update PB ---
                        try(ProgressBar$tick())

#--- Terminate PB ---
try(ProgressBar$terminate())




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Put data frames into list to get compact Augmented Data Set (ADS) object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Conversion from tibble to data.frame (if necessary), because dataSHIELD can handle data.frames better
#-------------------------------------------------------------------------------
ls_AugmentedDataSet <- list(Patients = as.data.frame(df_ADS_Patients),
                            Diagnoses = as.data.frame(df_ADS_Diagnoses),
                            Events = as.data.frame(df_ADS_Events))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define content of AugmentationReport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_AugmentationReport <- list(Test = c("TestReport"))


Messages$CheckAugmentationCompletion <- "green"
Messages$FinalMessage <- "Augmentation performed successfully!"

# },
#
# # In case of occurring warning:
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# warning = function(w)
#           {
#               Messages$CheckAugmentationCompletion <- "yellow"
#               Messages$FinalMessage <- paste0("Completed Augmentation with following warning: \n", w)
#           },
#
# # In case of occurring error:
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# error = function(e)
#         {
#             Messages$CheckAugmentationCompletion <- "red"
#             Messages$FinalMessage <- paste0("An error occured: \n", e)
#         },
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # RETURN STATEMENT
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# finally =
# {
#   # Return the Augmented Data Set (ADS), an Augmentation Report (defined above) and Messages
  return(list(AugmentedDataSet = ls_AugmentedDataSet,
              AugmentationReport = ls_AugmentationReport,
              AugmentationMessages = Messages))
# })

}


