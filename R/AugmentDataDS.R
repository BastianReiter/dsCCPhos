
#' AugmentDataDS
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms Curated Data Set (CDS) into Augmented Data Set (ADS)
#'
#' Server-side ASSIGN method
#'
#' @param CuratedDataSetName.S \code{character} - Name of the Curated Data Set object on server - Default: 'CuratedDataSet'
#' @param Settings.S \code{list} - Settings passed to function
#'                   \itemize{\item DiagnosisAssociation \code{list}
#'                                \itemize{\item Check \code{logical} - Whether or not to classify associated diagnosis entries
#'                                         \item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DiagnosisAssociation}
#'                                         \item Profile \code{character} - Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{DiagnosisAssociation$RuleSet} - Default: 'Default'}
#'                            \item EventFeatures \code{list}
#'                                \itemize{\item RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_EventFeatures}
#'                                         \item Profile \code{character} - Profile name defining rule set to be used for event feature engineering. Profile name must be stated in \code{EventFeatures$RuleSet} - Default: 'Default'}}
#'
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item AugmentedDataSet \code{list}
#'                      \itemize{\item Events
#'                               \item Therapies
#'                               \item Diagnoses
#'                               \item Patients}
#'                  \item AugmentationReport \code{list}
#'                  \item AugmentationMessages \code{list}}
#' @export
#' @author Bastian Reiter
AugmentDataDS <- function(CuratedDataSetName.S = "CuratedDataSet",
                          Settings.S = list(DiagnosisAssociation = list(Check = TRUE,
                                                                        RuleSet = dsCCPhos::Meta_DiagnosisAssociation,
                                                                        Profile = "Default"),
                                            EventFeatures = list(RuleSet = dsCCPhos::Meta_EventFeatures,
                                                                 Profile = "Default"),
                                            OverallSurvival = list(ReferenceEvent = c(EventClass = "Diagnosis",
                                                                                      EventSubclass = "InitialDiagnosis")),
                                            TherapyOfInterest = list(EventSubclass = "Surgery",
                                                                     EventSubclassRank = 1),
                                            TimeToEvent = list(ReferenceEvent = list(EventClass = "Diagnosis",
                                                                                     EventSubclass = "InitialDiagnosis"),
                                                               TargetEvent = list(EventClass = "VitalStatus",
                                                                                  EventSubclass = "Deceased"))))
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
#   MODULE B)  Creation of ADS$Therapies
#       - Consolidate information from ADS$Events
#
#   MODULE C)  Creation of ADS$Diagnoses
#       - Consolidate information from ADS$Events
#
#   MODULE D)  Creation of ADS$Patients
#       - Consolidate information from ADS$Events, ADS$Therapies and ADS$Diagnoses
#
#   Return statement


### For testing purposes
# Settings.S <- list(DiagnosisAssociation = list(Check = TRUE,
#                                                RuleSet = dsCCPhos::Meta_DiagnosisAssociation,
#                                                Profile = "Default"),
#                    EventFeatures = list(RuleSet = dsCCPhos::Meta_EventFeatures,
#                                         Profile = "Default"),
#                    TimeToEvent = list(ReferenceEvent = c(EventClass = "Diagnosis",
#                                                          EventSubclass = "InitialDiagnosis"),
#                                       TargetEvent = c(EventClass = "VitalStatus",
#                                                       EventSubclass = "Deceased")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Package requirements -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(lubridate)
require(purrr)
require(slider)
require(stringr)
require(tidyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Equip 'Settings.S' with default values in case of missing arguments -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename 'Settings.S' argument for better code readability
Settings <- Settings.S

# If list of 'Settings' passed to function is incomplete, complete it with default values
if (is.null(Settings$DiagnosisAssociation$Check)) { Settings$DiagnosisAssociation$Check <- TRUE }
if (is.null(Settings$DiagnosisAssociation$RuleSet)) { Settings$DiagnosisAssociation$RuleSet <- dsCCPhos::Meta_DiagnosisAssociation }
if (is.null(Settings$DiagnosisAssociation$Profile)) { Settings$DiagnosisAssociation$Profile <- "Default" }
if (is.null(Settings$EventFeatures$RuleSet)) { Settings$EventFeatures$RuleSet <- dsCCPhos::Meta_EventFeatures }
if (is.null(Settings$EventFeatures$Profile)) { Settings$EventFeatures$Profile <- "Default" }
if (is.null(Settings$TimeToEvent)) { Settings$TimeToEvent <- list(ReferenceEvent = c(EventClass = "Diagnosis",
                                                                                     EventSubclass = "InitialDiagnosis"),
                                                                  TargetEvent = c(EventClass = "VitalStatus",
                                                                                  EventSubclass = "Deceased")) }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Evaluate and parse input before proceeding -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(CuratedDataSetName.S))
{
    CDS <- eval(parse(text = CuratedDataSetName.S), envir = parent.frame())

} else {

    ClientMessage <- "ERROR: 'CuratedDataSetName.S' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

# if (Settings$EventFeatures$Profile %in% names(Settings$EventFeatures$RuleSet) == FALSE)
# {
#     ClientMessage <- "ERROR: Value of settings argument 'EventFeatures$Profile' must be column name of data.frame passed in settings argument 'EventFeatures$RuleSet'."
#     stop(ClientMessage, call. = FALSE)
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Initial statements -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Print starting message
cat("\n")
Message <- paste0("Starting Data Augmentation...")
cli::cat_bullet(Message, bullet = "star")
cat("\n")

# Suppress summarize info messages
options(dplyr.summarise.inform = FALSE)

# Initiate output objects
ADS <- list()
AugmentationReport <- NULL

# Initiate Messaging objects
Messages <- list()
Messages$DiagnosisAssociation <- character()
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
#                      _______________
#                     / ADS$Therapies \
#                     \_______________/
#
#                      ____________
#                     / ADS$Events \
#                     \____________/



# Temporary !!!

# df_CDS_Diagnosis <- df_CDS_Diagnosis %>%
#                         filter(IsReferenceEntry == TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Processing of CDS tables -
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



# Module A) Classify associations between diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple distinct diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisAssociations() to distinguish pseudo-different from actually different diagnoses
#   - Rules for classification of associated diagnosis entries are defined in customizable data object delivered with dsCCPhos
#   - Replace DiagnosisIDs in all related tables with ReferenceDiagnosisID
#-------------------------------------------------------------------------------



# A 1) Join tables 'Diagnosis' and 'Histology'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Create composed ID ('DiagnosisID / HistologyID')
#   - Add auxiliary features for filtering purposes
#-------------------------------------------------------------------------------

CDS$Diagnosis <- CDS$Diagnosis %>%
                          left_join(CDS$Histology, by = join_by(PatientID, DiagnosisID)) %>%
                          mutate(OriginalDiagnosisID = DiagnosisID,
                                 DiagnosisID = paste0(DiagnosisID, "/", HistologyID)) %>%
                          relocate(DiagnosisID, .after = PatientID) %>%
                          group_by(PatientID) %>%
                              mutate(PatientCountEntries = n()) %>%
                          ungroup()


if (Settings$DiagnosisAssociation$Check == TRUE)
{
#///////////////////////////////////////////////////////////////////////////////

# Compile rule calls (unevaluated dplyr::case_when-Statements) with dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Call_IsLikelyAssociated <- CompileClassificationCall(TargetFeature = "IsLikelyAssociated",
                                                     RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                     RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                     ValueIfNoRuleMet = FALSE)

Call_InconsistencyCheck <- CompileClassificationCall(TargetFeature = "InconsistencyCheck",
                                                     RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                     RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                     ValueIfNoRuleMet = "No apparent inconsistency")

Call_ImplausibilityCheck <- CompileClassificationCall(TargetFeature = "ImplausibilityCheck",
                                                      RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                      RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                      ValueIfNoRuleMet = "No apparent implausibility")

Call_Relation_ICD10 <- CompileClassificationCall(TargetFeature = "Relation_ICD10",
                                                 RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                 RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                 ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOTopography <- CompileClassificationCall(TargetFeature = "Relation_ICDOTopography",
                                                          RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                          RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                          ValueIfNoRuleMet = NA_character_)

Call_Relation_LocalizationSide <- CompileClassificationCall(TargetFeature = "Relation_LocalizationSide",
                                                            RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                            RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOMorphology <- CompileClassificationCall(TargetFeature = "Relation_ICDOMorphology",
                                                            RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                            RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_Grading <- CompileClassificationCall(TargetFeature = "Relation_Grading",
                                                   RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                   RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                   ValueIfNoRuleMet = NA_character_)

Call_IsLikelyProgression <- CompileClassificationCall(TargetFeature = "IsLikelyProgression",
                                                      RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                      RuleProfile = Settings$DiagnosisAssociation$Profile,
                                                      ValueIfNoRuleMet = NA)

Call_IsLikelyRecoding <- CompileClassificationCall(TargetFeature = "IsLikelyRecoding",
                                                   RuleSet = Settings$DiagnosisAssociation$RuleSet,
                                                   RuleProfile = Settings$DiagnosisAssociation$Profile,
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


# Set up progress bar
#-------------------------------------------------------------------------------
CountProgressItems <- CDS$Diagnosis %>% filter(PatientCountEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying associated diagnosis entries... [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

# Filter patients with multiple distinct diagnosis entries and apply dsCCPhos::ClassifyDiagnosisAssociations()
df_Aux_Diagnosis_ClassifiedAssociations <- CDS$Diagnosis %>%
                                                filter(PatientCountEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisAssociation(DiagnosisEntries = .x,
                                                                                                RuleCalls = RuleCalls_DiagnosisAssociation,
                                                                                                ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble CDS$Diagnosis after processing of associated diagnosis entries
CDS$Diagnosis <- CDS$Diagnosis %>%
                          filter(PatientCountEntries == 1) %>%
                          mutate(ReferenceDiagnosisID = DiagnosisID,
                                 IsLikelyAssociated = FALSE) %>%
                          bind_rows(df_Aux_Diagnosis_ClassifiedAssociations) %>%
                          mutate(IsReferenceEntry = (ReferenceDiagnosisID == DiagnosisID),
                                                    .after = DiagnosisID) %>%
                          arrange(PatientID) %>%
                          relocate(c(PatientID, ReferenceDiagnosisID), .before = DiagnosisID) %>%
                          rename(all_of(c(SubDiagnosisID = "DiagnosisID",
                                          DiagnosisID = "ReferenceDiagnosisID")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Associated diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# a) Number of associated diagnosis entries and
# b) Number of patients that have associated diagnosis entries
CountDiagnosisAssociations <- sum(CDS$Diagnosis$IsLikelyAssociated, na.rm = TRUE)
CountPatientsWithDiagnosisAssociations <- df_Aux_Diagnosis_ClassifiedAssociations %>%
                                              filter(IsLikelyAssociated == TRUE) %>%
                                              pull(PatientID) %>%
                                              n_distinct()

# Print message for live monitoring in local tests
Message <- paste0("Classified ", CountDiagnosisAssociations, " associated diagnosis entries related to ", CountPatientsWithDiagnosisAssociations, " patient IDs.")
cli::cat_bullet(Message, bullet = "info")
cat("\n")

# Save message in output object
Messages$DiagnosisAssociation <- Message


#///////////////////////////////////////////////////////////////////////////////
} else {      # In case Diagnosis Association Check is skipped

    # Add empty/trivial features to keep CDS output consistent
    CDS$Diagnosis <- CDS$Diagnosis %>%
                              mutate(DiagnosisID = OriginalDiagnosisID,
                                     SubDiagnosisID = OriginalDiagnosisID,
                                     IsReferenceEntry = TRUE,
                                     .after = DiagnosisID) %>%
                              mutate(IsLikelyAssociated = FALSE,
                                     Association = NA)

    # Set relevant Counts NA
    CountDiagnosisAssociations <- NA
    CountPatientsWithDiagnosisAssociations <- NA

    # Print warning message if Diagnosis Association Check was skipped
    Message <- "Diagnosis Association Check was skipped!"
    cli::cat_bullet(Message, bullet = "warning")
    cat("\n")

    # Save message in output object
    Messages$DiagnosisAssociation <- Message
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update related tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     - Replace DiagnosisIDs to associate entries
#     - Rearrange column order
#-------------------------------------------------------------------------------

# Create table for DiagnosisID replacement in related tables
Aux_Diagnosis_IDMappingAssociations <- CDS$Diagnosis %>%
                                            ungroup() %>%
                                            select(PatientID, OriginalDiagnosisID, DiagnosisID) %>%
                                            rename(all_of(c(DiagnosisID = "OriginalDiagnosisID",
                                                            NewDiagnosisID = "DiagnosisID"))) %>%
                                            distinct()

CDS <- CDS %>%
          imap(function(Table, tablename)
               {
                  # For all tables that use 'DiagnosisID', make sure the previously removed DiagnosisIDs are replaced by respective reference DiagnosisIDs...
                  if (nrow(Table) > 0 & tablename != "Diagnosis" & all(c("PatientID", "DiagnosisID") %in% names(Table)))
                  {
                      Table <- Table %>%
                                    left_join(Aux_Diagnosis_IDMappingAssociations, by = join_by(PatientID,
                                                                                                DiagnosisID)) %>%
                                    mutate(DiagnosisID = ifelse(!is.na(NewDiagnosisID),
                                                                NewDiagnosisID,
                                                                OldDiagnosisID)) %>%
                                    select(-NewDiagnosisID) %>%
                                    relocate(c(PatientID, DiagnosisID))   # Moves features 'PatientID' and 'DiagnosisID' to front of table

                  } else { return(Table) }
               })



# Module D 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Reconstruct CDS$Histology from CDS$Diagnosis
#-------------------------------------------------------------------------------

# Reconstruction of CDS$Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (Settings$DiagnosisAssociation$Check == TRUE)
{
    # Reconstruct CDS$Histology
    CDS$Histology <- CDS$Diagnosis %>%
                              select(all_of(c("SubDiagnosisID", names(CDS$Histology)))) %>%
                              relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = HistologyID)
} else {

    CDS$Histology <- CDS$Diagnosis %>%
                              select(all_of(names(CDS$Histology))) %>%
                              mutate(SubDiagnosisID = DiagnosisID, .after = DiagnosisID)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE B)  Generate ADS$Events
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
                             EventSubclassRank = row_number(),
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
                          if (tablename == "DiseaseStatus")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "DiseaseStatusDate"
                              Val_EventType <- "Point"
                              Val_EventClass <- "Diagnosis"
                              Val_EventSubclass <- "DiseaseStatus"
                              EventDetailsFeatures = c("GlobalStatus",
                                                       "PrimarySiteStatus",
                                                       "LymphnodalStatus",
                                                       "MetastasisStatus")
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
                                                       "NumberLymphnodesExamined",
                                                       "NumberLymphnodesAffected",
                                                       "NumberSentinelLymphnodesExamined",
                                                       "NumberSentinelLymphnodesAffected")
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
                          if (tablename == "RadiationTherapy")
                          {
                              GroupingFeature <- c("PatientID", "DiagnosisID")
                              DateFeature <- "RadiationTherapyStartDate"
                              EndDateFeature <- "RadiationTherapyEndDate"
                              Val_EventType <- "Period"
                              Val_EventClass <- "Therapy"
                              Val_EventSubclass <- "Radiation Therapy"
                              EventDetailsFeatures = c("Intention",
                                                       "RelationToSurgery",
                                                       "ApplicationType",
                                                       "RadiationType",
                                                       "TargetArea",
                                                       "TargetAreaSide",
                                                       "TotalDose",
                                                       "TotalDoseUnit",
                                                       "SingleDailyDose",
                                                       "SingleDailyDoseUnit",
                                                       "Boost",
                                                       "EndReason",
                                                       "AdverseEventGrade",
                                                       "AdverseEventType",
                                                       "AdverseEventVersion")
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
                                                       "OPSVersion",
                                                       "ResidualAssessmentLocal",
                                                       "ResidualAssessmentTotal",
                                                       "SurgeryComplicationsICD10",
                                                       "SurgeryComplicationsADT")
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
                                                       "RelationToSurgery",
                                                       "Type",
                                                       "IsChemotherapy",
                                                       "IsHormoneTherapy",
                                                       "IsImmunotherapy",
                                                       "IsBoneMarrowTransplant",
                                                       "IsObservantStrategy",
                                                       "Protocol",
                                                       "Substances",
                                                       "ATC",
                                                       "ATCVersion",
                                                       "CTCAEGrade",
                                                       "CTCAEType",
                                                       "CTCAEVersion")
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
                                                               EventSubclassRank = row_number(),
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
# 3) Consolidate Event data from CDS tables in one coherent table
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
                  group_by(PatientID, DiagnosisID, EventClass) %>%
                      mutate(EventClassRank = row_number()) %>%
                  select(PatientID,
                         DateOfBirth,
                         DiagnosisID,
                         InitialDiagnosisDate,
                         EventRank,
                         EventType,
                         EventDate,
                         EventDateEnd,
                         EventClass,
                         EventClassRank,
                         EventSubclass,
                         EventSubclassRank,
                         EventOrderSignificance,
                         EventDateIsAdjusted,
                         EventPatientAge,
                         EventDaysSinceDiagnosis,
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
# 4) Enhance ADS$Events with engineered features (customizable through function argument / meta data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#--- Set up progress bar -------------------------------------------------------
# CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
# ProgressBar <- progress_bar$new(format = "Engineering event data [:bar] :percent in :elapsed  :spin",
#                                 total = CountProgressItems, clear = FALSE, width= 100)
# #-------------------------------------------------------------------------------




# ADS$Events <- ADS$Events %>%
#                   group_by(PatientID, DiagnosisID) %>%
#                       unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
#                       group_modify( ~ CreateEventFeatures(EventData = .x,
#                                                           RuleSet = Settings$EventFeatures$RuleSet,
#                                                           Profile = Settings$EventFeatures$Profile,
#                                                           ProgressBarObject = ProgressBar))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE C)  Generate ADS$DiseaseCourses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Auxiliary function
AssignComparatorCodes <- function(ValueVector, FeatureName)
                         {
                             # Get value to comparator code matches from meta data
                             ComparatorCodeData <- dsCCPhos::Meta_Values %>%
                                                      filter(Feature == FeatureName) %>%
                                                      select(Value_Curated, ComparatorCode)

                             # Create named vector
                             ComparatorCodes <- set_names(ComparatorCodeData$ComparatorCode,
                                                          ComparatorCodeData$Value_Curated)

                             # Use named vector to transform original values into comparator codes
                             return(suppressWarnings(as.integer(ComparatorCodes[ValueVector])))
                         }



ADS$DiseaseCourses <- ADS$Events %>%
                          filter(EventSubclass == "DiseaseStatus") %>%
                          unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
                          mutate(PrimarySiteStatus_Comp = AssignComparatorCodes(PrimarySiteStatus, "PrimarySiteStatus"),
                                 LymphnodalStatus_Comp = AssignComparatorCodes(LymphnodalStatus, "LymphnodalStatus"),
                                 MetastasisStatus_Comp = AssignComparatorCodes(MetastasisStatus, "MetastasisStatus")) %>%
                          group_by(PatientID, DiagnosisID) %>%
                              mutate(PrimarySiteStatus_Diff = PrimarySiteStatus_Comp - lag(PrimarySiteStatus_Comp),
                                     LymphnodalStatus_Diff = LymphnodalStatus_Comp - lag(LymphnodalStatus_Comp),
                                     MetastasisStatus_Diff = MetastasisStatus_Comp - lag(MetastasisStatus_Comp)) %>%
                          ungroup() %>%
                              mutate(PrimarySiteChange = case_when(PrimarySiteStatus_Diff == 0 ~ "Stable",
                                                                   PrimarySiteStatus_Diff < 0 ~ "Regression",
                                                                   PrimarySiteStatus_Diff > 0 ~ "Progression",
                                                                   .default = "Unclear"),
                                     LymphnodalChange = case_when(LymphnodalStatus_Diff == 0 ~ "Stable",
                                                                  LymphnodalStatus_Diff < 0 ~ "Regression",
                                                                  LymphnodalStatus_Diff > 0 ~ "Progression",
                                                                  .default = "Unclear"),
                                     MetastasisChange = case_when(MetastasisStatus_Diff == 0 ~ "Stable",
                                                                  MetastasisStatus_Diff < 0 ~ "Regression",
                                                                  MetastasisStatus_Diff > 0 ~ "Progression",
                                                                  .default = "Unclear"),
                                     IsProgression = case_when(GlobalStatus %in% c('D', 'P') ~ TRUE,
                                                               PrimarySiteStatus %in% c('rpT', 'R') | LymphnodalStatus %in% c('pL', 'L') | MetastasisStatus %in% c('pM', 'M') ~ TRUE,
                                                               PrimarySiteChange == "Progression" | LymphnodalChange == "Progression" | MetastasisChange == "Progression" ~ TRUE,
                                                               .default = FALSE),
                                     IsContradiction = GlobalStatus %in% c('CR', 'CRr') & IsProgression == TRUE,
                                     IsResponse = case_when(GlobalStatus %in% c('CR', 'CRr', 'PartRemission', 'MinResp') ~ TRUE,
                                                            IsProgression == TRUE ~ FALSE,
                                                            IsContradiction == TRUE ~ NA,
                                                            is.na(GlobalStatus) & (PrimarySiteChange == "Regression" |
                                                                                   LymphnodalChange == "Regression" |
                                                                                   MetastasisChange == "Regression") ~ TRUE,
                                                            .default = FALSE),
                                     IsRemission = case_when(GlobalStatus %in% c('CR', 'CRr', 'PartRemission') ~ TRUE,
                                                             (is.na(GlobalStatus) | GlobalStatus == 'NC') &
                                                                  PrimarySiteStatus == 'N' &
                                                                  LymphnodalStatus == 'N' &
                                                                  MetastasisStatus == 'N' ~ TRUE,
                                                             .default = FALSE))


# Test <- ADS$Events %>%
#             filter(EventSubclass == "Staging") %>%
#             unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
#                 mutate(UICCStage_Comp = AssignComparatorCodes(PrimarySiteStatus, "PrimarySiteStatus"),
#                        LymphnodalStatus_Comp = AssignComparatorCodes(LymphnodalStatus, "LymphnodalStatus"),
#                        MetastasisStatus_Comp = AssignComparatorCodes(MetastasisStatus, "MetastasisStatus")) %>%
#



            # CreateEventFeatures(RuleSet = Settings$EventFeatures$RuleSet,
            #                     Profile = Settings$EventFeatures$Profile)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE D)  Generate ADS$Therapies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Test <- ADS$Events %>%
#             filter(EventClass == "Therapy") %>%
#             unnest(cols = c(EventDetails), keep_empty = TRUE) %>%
#             group_by(PatientID, DiagnosisID) %>%
#                 arrange(EventDate, .by_group = TRUE) %>%       # Sort again by EventDate to make sure
#                 mutate(IsFirstTherapy = )


# Features in ADS$Therapies

# FirstTherapyType
# FirstTherapyLatency   (Abstand Diagnose - FirstTherapy)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE E)  Generate ADS$Diagnoses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#--- Set up progress bar -------------------------------------------------------
CountProgressItems <- ADS$Events %>% select(PatientID, DiagnosisID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Summarizing event data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

# Summarize diagnosis-specific event data using dsCCPhos::SummarizeEventData()
df_Aux_DiagnosisSummary_Events <- ADS$Events %>%
                                      group_by(PatientID, DiagnosisID) %>%
                                          group_modify(~ SummarizeEventData(EventData = .x,
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
# MODULE F)  Generate ADS$Patients
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


