
#' AugmentDataDS
#'
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param CuratedDataSetName.S \code{character} - Name of the Curated Data Set object on server - Default: 'CuratedDataSet'
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item AugmentedDataSet \code{list}
#'                  \item AugmentationReport \code{list}
#'                  \item AugmentationMessages \code{list}}
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
#
#   MODULE A)  Creation of ADS$Events
#       - Diagnosis-related
#       - Patient-related
#
#   MODULE B)  Creation of ADS$Diagnoses
#       - Consolidate information from df_ADS_Events
#
#   MODULE C)  Creation of ADS$Patients
#       - Consolidate information from df_ADS_Events and df_ADS_Diagnoses
#
#   Return statement


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(CuratedDataSetName.S))
{
    CDS <- eval(parse(text = CuratedDataSetName.S), envir = parent.frame())
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
ADS <- list()
AugmentationReport <- NULL

# Initiate Messaging objects
Messages <- list()
Messages$CheckAugmentationCompletion <- "red"
Messages$FinalMessage <- "Augmentation not completed"


# Use tryCatch to catch warnings and errors
# Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
# tryCatch({


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augmented Data Model (ADM)
# Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ______________
#                     / ADS$Patients \
#                     \______________/
#
#                      _______________
#                     / ADS$Diagnoses \
#                     \_______________/
#
#                      ____________
#                     / ADS$Events \
#                     \____________/



# Temporary !!!

# df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
#                         filter(IsReferenceEntry == TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing of CDS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Deprecated

# CDS$SystemicTherapy <- CDS$SystemicTherapy %>%
#                               mutate(SystemicTherapySubclass = case_when((IsChemotherapy == TRUE
#                                                                             & IsHormoneTherapy == FALSE
#                                                                             & IsImmunotherapy == FALSE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Chemotherapy Mono",
#                                                                           (IsChemotherapy == FALSE
#                                                                             & IsHormoneTherapy == TRUE
#                                                                             & IsImmunotherapy == FALSE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Hormone Therapy Mono",
#                                                                           (IsChemotherapy == FALSE
#                                                                             & IsHormoneTherapy == FALSE
#                                                                             & IsImmunotherapy == TRUE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Immunotherapy Mono",
#                                                                           (IsChemotherapy == FALSE
#                                                                             & IsHormoneTherapy == FALSE
#                                                                             & IsImmunotherapy == FALSE
#                                                                             & IsBoneMarrowTransplant == TRUE) ~ "Bone Marrow Transplant",
#                                                                           (IsChemotherapy == TRUE
#                                                                             & IsHormoneTherapy == TRUE
#                                                                             & IsImmunotherapy == FALSE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Hormone Combination",
#                                                                           (IsChemotherapy == TRUE
#                                                                             & IsHormoneTherapy == FALSE
#                                                                             & IsImmunotherapy == TRUE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Immuno Combination",
#                                                                           (IsChemotherapy == TRUE
#                                                                             & IsHormoneTherapy == FALSE
#                                                                             & IsImmunotherapy == FALSE
#                                                                             & IsBoneMarrowTransplant == TRUE) ~ "Chemo and BMT",
#                                                                           (IsChemotherapy == FALSE
#                                                                             & IsHormoneTherapy == TRUE
#                                                                             & IsImmunotherapy == TRUE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Hormone/Immuno Combination",
#                                                                           (IsChemotherapy == TRUE
#                                                                             & IsHormoneTherapy == TRUE
#                                                                             & IsImmunotherapy == TRUE
#                                                                             & IsBoneMarrowTransplant == FALSE) ~ "Chemo/Hormone/Immuno Combination",
#                                                                           IsObservantStrategy == TRUE ~ "Observant Strategy",
#                                                                           TRUE ~ "Other"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE A)  Generate ADS$Events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   1) Initiation
#   2) Loop through CDS tables and generate event data
#   3) Enhance data set with useful features
#-------------------------------------------------------------------------------


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- 17
ProgressBar <- progress_bar$new(format = "Generating diagnosis-related events [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Initiate ADS$Events, integrating patient-specific and initial diagnosis events
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initiation 1: Initial diagnosis event
ADS$Events <- CDS$Patient %>%
                  right_join(CDS$Diagnosis, join_by(PatientID)) %>%
                  filter(IsReferenceEntry == TRUE) %>%
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
df_Events_LastVitalStatus <- CDS$Patient %>%
                                  right_join(CDS$Diagnosis, join_by(PatientID)) %>%
                                  filter(IsReferenceEntry == TRUE) %>%
                                  group_by(PatientID, DiagnosisID) %>%
                                      mutate(EventType = "Point",
                                             EventDate = LastVitalStatusDate,
                                             EventDateIsAdjusted = FALSE,
                                             EventClass = "VitalStatus",
                                             EventSubclass = if_else(is.na(LastVitalStatus), "Unknown", LastVitalStatus)) %>%
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
ADS$Events <- ADS$Events %>%
                  bind_rows(df_Events_LastVitalStatus)

try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Loop through CDS tables to generate event data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EventData <- CDS[names(CDS) %in% c("Patient", "Diagnosis") == FALSE] %>%      # Deselect tables 'Patient' and 'Diagnosis' from CDS
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          GroupingFeature <- NULL
                          DateFeature <- NULL
                          EndDateFeature <- NULL
                          Val_EventType <- NA
                          Val_EventClass <- NA
                          Val_EventSubclass <- NA
                          EventDetailsFeatures <- NULL

                          if (tablename == "BioSampling")
                          {
                              GroupingFeature <- "PatientID"
                              DateFeature <- "BioSamplingDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnostics"
                              Val_EventSubclass <- "Sample Taking"
                              EventDetailsFeatures <- c("Aliquot",
                                                        "Type",
                                                        "Status",
                                                        "Quantity",
                                                        "Unit",
                                                        "ProjectName")
                          }
                          if (tablename == "Histology")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "HistologyDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnostics"
                              Val_EventSubclass <- "Histology"
                              EventDetailsFeatures = c("ICDOMorphologyCode",
                                                       "ICDOMorphologyVersion",
                                                       "Grading",
                                                       "ICDOMorphologyComment")
                          }
                          if (tablename == "Metastasis")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "MetastasisDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnosis"
                              Val_EventSubclass <- "Metastasis"
                              EventDetailsFeatures = c("HasMetastasis",
                                                       "Localization")
                          }
                          if (tablename == "MolecularDiagnostics")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "MolecularDiagnosticsDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnostics"
                              Val_EventSubclass <- "Molecular Diagnostics"
                              EventDetailsFeatures = c("MolecularMarker",
                                                       "MolecularMarkerStatus",
                                                       "Documentation")
                          }
                          if (tablename == "Progress")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "ProgressDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnosis"
                              Val_EventSubclass <- "Progress"
                              EventDetailsFeatures = c("GlobalStatus",
                                                       "PrimarySiteStatus",
                                                       "LymphnodalStatus",
                                                       "MetastasisStatus")
                          }
                          if (tablename == "RadiationTherapy")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "RadiationTherapyStartDate"
                              EndDateFeature <- "RadiationTherapyEndDate"
                              Val_EventType <- "Period"
                              Val_EventClass <- "Therapy"
                              Val_EventSubclass <- "Radiation Therapy"
                              EventDetailsFeatures = c("Intention",
                                                       "RelationToSurgery")
                          }
                          if (tablename == "Staging")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "StagingDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnosis"
                              Val_EventSubclass <- "Staging"
                              EventDetailsFeatures = c("UICCStage",
                                                       "TNM_T_Prefix",
                                                       "TNM_T",
                                                       "TNM_N_Prefix",
                                                       "TNM_N",
                                                       "TNM_M_Prefix",
                                                       "TNM_M",
                                                       "TNM_mSymbol",
                                                       "TNM_rSymbol",
                                                       "TNM_ySymbol",
                                                       "TNMVersion",
                                                       "TNM_L",
                                                       "TNM_V",
                                                       "TNM_Pn",
                                                       "TNM_S")
                          }
                          if (tablename == "Surgery")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "SurgeryDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Therapy"
                              Val_EventSubclass <- "Surgery"
                              EventDetailsFeatures = c("Intention",
                                                       "OPSCode",
                                                       "ResidualAssessmentLocal",
                                                       "ResidualAssessmentTotal")
                          }
                          if (tablename == "SystemicTherapy")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "SystemicTherapyStartDate"
                              EndDateFeature <- "SystemicTherapyEndDate"
                              Val_EventType <- "Period"
                              Val_EventClass <- "Therapy"
                              Val_EventSubclass <- "SystemicTherapy"
                              EventDetailsFeatures = c("Intention",
                                                       "Substances",
                                                       "RelationToSurgery")
                          }

                          # Create table of events for every CDS table, incorporating values / features defined above
                          if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                          {
                              if (length(GroupingFeature) == 1) { TableEventData <- Table %>% group_by(PatientID) }
                              else { TableEventData <- Table %>% group_by(PatientID, DiagnosisID) }

                              TableEventData <- TableEventData %>%
                                                        arrange(!!sym(DateFeature), .by_group = TRUE) %>%
                                                        mutate(EventType = Val_EventType,
                                                               EventDate = !!sym(DateFeature),
                                                               EventDateIsAdjusted = FALSE,      # In case event date is adjusted later for plausibility reasons
                                                               EventClass = Val_EventClass,
                                                               EventSubclass = Val_EventSubclass,
                                                               EventRankWithinSubclass = row_number(),
                                                               EventOrderSignificance = case_when(row_number() == 1 ~ paste("First", Val_EventSubclass),
                                                                                                  row_number() == n() ~ paste("Last", Val_EventSubclass),
                                                                                                  TRUE ~ NA_character_)) %>%
                                                        { if (!is.null(EndDateFeature))
                                                          { mutate(., EventDateEnd = !!sym(EndDateFeature), .after = EventDate) }      # For events of type "Period"
                                                          else {.}
                                                        } %>%
                                                        nest(EventDetails = all_of(EventDetailsFeatures)) %>%
                                                    ungroup() %>%
                                                    select(all_of(GroupingFeature),
                                                           starts_with("Event"))
                              return(TableEventData)
                          }

                          else { return(NULL) }

                       }) %>%
                  list_rbind()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Consolidate Event-oriented data from CDS tables in one coherent table
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ADS$Events <- ADS$Events %>%
                  bind_rows(EventData) %>%
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
                             InitialDiagnosisDate = FirstEventDate) %>%
                      arrange(EventDate, .by_group = TRUE) %>%      # Sort by date again after possible date adjustments
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
# 4) Enhance ADS$Events with customizably engineered features
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#--- Set up progress bar -------------------------------------------------------
# CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
# ProgressBar <- progress_bar$new(format = "Augmenting event data [:bar] :percent in :elapsed  :spin",
#                                 total = CountProgressItems, clear = FALSE, width= 100)
# #-------------------------------------------------------------------------------
#
# ADS$Events <- ADS$Events







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE B)  Generate ADS$Diagnoses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Summarizing event data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

# Summarize diagnosis-specific event data using dsCCPhos::SummarizeEventData()
df_Aux_DiagnosisSummary_Events <- ADS$Events %>%
                                      unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                                      group_by(PatientID, DiagnosisID) %>%
                                          group_modify(~ SummarizeEventData(EventEntries = .x,
                                                                            ProgressBarObject = ProgressBar)) %>%
                                      ungroup()




df_Aux_DiagnosisData <- CDS$Diagnosis %>%
                            left_join(CDS$Staging, by = join_by(PatientID, DiagnosisID), relationship = "many-to-many") %>%
                            group_by(DiagnosisID) %>%
                                arrange(DiagnosisDate) %>%
                                slice_head() %>%
                            ungroup() %>%
                            mutate(UICCStageCategory = case_match(UICCStage,
                                                                  c("0", "0is", "0a") ~ "0",
                                                                  c("I", "IA", "IA1", "IA2", "IA3", "IB", "IB1", "IB2", "IC", "IS") ~ "I",
                                                                  c("II", "IIA", "IIA1", "IIA2", "IIB", "IIC") ~ "II",
                                                                  c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2", "IIID") ~ "III",
                                                                  c("IV", "IVA", "IVB", "IVC") ~ "IV",
                                                                  .default = NA_character_),
                                                       .after = UICCStage)
                            #--- Update PB ---
                            # try(ProgressBar$tick())


ADS$Diagnoses <- df_Aux_DiagnosisData %>%
                      left_join(df_Aux_DiagnosisSummary_Events, by = join_by(PatientID, DiagnosisID)) %>%
                      #filter(is.na(TimeDiagnosisToDeath) | TimeDiagnosisToDeath >= 0) %>%
                      ungroup()
                      #--- Update PB ---
                      # try(ProgressBar$tick())





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
#      - TimeSurgeryToDeath
#      - TimeLastTherapyToDeath
#
#   - TimeTherapyToProgression
#   - TimeTherapyToDeath

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
# MODULE C)  Generate ADS$Patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- 3
ProgressBar <- progress_bar$new(format = "Composing patient-specific data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------



df_Aux_PatientSummary_Diagnosis <- CDS$Diagnosis %>%
                                        group_by(PatientID) %>%
                                            summarize(CountDiagnoses = n_distinct(DiagnosisID)) %>%
                                        ungroup()
                                        #--- Update PB ---
                                        try(ProgressBar$tick())



# !!! TEMPORARY !!! (For easier testing, slice is performed to filter for only one diagnosis per patient)
ADS$Patients <- CDS$Patient %>%
                    left_join(df_Aux_PatientSummary_Diagnosis, by = join_by(PatientID)) %>%
                    left_join(ADS$Diagnoses, by = join_by(PatientID)) %>%      # <--- TEMPORARY: Joining with ADS_Diagnoses
                    group_by(PatientID) %>%
                        arrange(DiagnosisDate) %>%
                        slice_head() %>%      # <--- TEMPORARY: Slice performed
                    ungroup()
                    #--- Update PB ---
                    try(ProgressBar$tick())

#--- Terminate PB ---
try(ProgressBar$terminate())




# Conversion of ADS tables from tibble to data.frame, because DataSHIELD can handle data.frames better
ADS <- ADS %>%
            map(\(Table) as.data.frame(Table))



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
  return(list(AugmentedDataSet = ADS,
              AugmentationReport = ls_AugmentationReport,
              AugmentationMessages = Messages))
# })

}


