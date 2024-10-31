
#' CurateDataDS
#'
#' Takes Raw Data Set (RDS) and transforms it into Curated Data Set (CDS) while tracing data transformation.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param RuleSet_RawDataHarmonization.S Data frame
#' @param RuleProfile_RawDataHarmonization.S String | Profile name defining rule set to be used for data harmonization. Profile name must be stated in \code{\link{RuleSet_RawDataHarmonization.S}. | Default: 'Default'
#' @param RuleSet_DiagnosisRedundancy.S Data frame
#' @param RuleProfile_DiagnosisRedundancy.S String | Profile name defining rule set to be used for classification of diagnosis redundancies. Profile name must be stated in \code{\link{RuleSet_DiagnosisRedundancy.S}. | Default: 'Default'
#' @param RuleSet_DiagnosisAssociation.S Data frame
#' @param RuleProfile_DiagnosisAssociation.S String | Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{\link{RuleSet_DiagnosisAssociation.S}. | Default: 'Default'
#'
#' @return A list containing the following objects:
#'         \itemize{\item CuratedDataSet (list)
#'                      \itemize{\item BioSampling
#'                               \item Diagnosis
#'                               \item Histology
#'                               \item Metastasis
#'                               \item MolecularDiagnostics
#'                               \item Patient
#'                               \item Progress
#'                               \item RadiationTherapy
#'                               \item Staging
#'                               \item Surgery
#'                               \item SystemicTherapy}
#'                  \item CurationReport (list)
#'                      \itemize{\item UnlinkedEntries (named vector)
#'                               \item Transformation (list of data frames)
#'                                    \itemize{\item BioSampling
#'                                             \item Diagnosis
#'                                             \item ...}
#'                               \item DiagnosisClassification (named vector)}
#'                  \item CurationMessages (list)}
#' @export
#' @author Bastian Reiter
CurateDataDS <- function(RawDataSetName.S = "RawDataSet",
                         RuleSet_RawDataHarmonization.S = dsCCPhos::RuleSet_RawDataHarmonization,
                         RuleProfile_RawDataHarmonization.S = "Default",
                         RuleSet_DiagnosisRedundancy.S = dsCCPhos::RuleSet_DiagnosisRedundancy,
                         RuleProfile_DiagnosisRedundancy.S = "Default",
                         RuleSet_DiagnosisAssociation.S = dsCCPhos::RuleSet_DiagnosisAssociation,
                         RuleProfile_DiagnosisAssociation.S = "Default")
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   SETUP
#     - Evaluation and parsing of input
#     - Loading of required package namespaces
#
#   MODULE 1)  Harmonization of Raw Data Set meta data and structure
#     - Transform table names
#     - Add empty tables in data set if they are missing in raw data
#     - Rename features
#     - In tables with missing features, add empty features accordingly
#
#   MODULE 2)  Primary table cleaning
#     - Remove entries that are not linked to related tables
#     - Remove duplicate entries
#
#   MODULE 3)  Data Harmonization / Transformation
#     - Transform feature names
#     - Definition of features to monitor during value transformation
#     - Value transforming operations
#     - Compilation of curation report
#
#   MODULE 4)  Processing of diagnosis data
#     - Joining of df_Diagnosis and df_Histology
#     - Classification and removal of redundant diagnosis entries
#     - Classification and bundling of associated diagnosis entries
#     - Update DiagnosisIDs in other tables
#
#   Return list containing
#     - Curated Data Set (list)
#     - Curation Report (list)
#     - Curation Messages (list)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(RawDataSetName.S))
{
    RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'RawDataSetName.S' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements and global settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(dsCCPhos)
require(lubridate)
require(progress)
require(purrr)
require(stats)
require(stringr)
require(tidyr)


# Suppress summarize info messages
options(dplyr.summarise.inform = FALSE)

# Initiate output objects
ls_CuratedDataSet <- NULL
ls_CurationReport <- NULL

# Initiate Messaging objects
Messages <- list()
Messages$UnlinkedEntries <- character()
Messages$DiagnosisRedundancies <- character()
Messages$DiagnosisAssociation <- character()
Messages$CheckCurationCompletion <- "red"
Messages$FinalMessage <- "Curation not completed"


# For testing purposes
# RuleSet_RawDataHarmonization.S <- dsCCPhos::RuleSet_RawDataHarmonization
# RuleProfile_RawDataHarmonization.S <- "Default"
# RuleSet_DiagnosisRedundancy.S <- dsCCPhos::RuleSet_DiagnosisRedundancy
# RuleProfile_DiagnosisRedundancy.S <- "Default"
# RuleSet_DiagnosisAssociation.S <- dsCCPhos::RuleSet_DiagnosisAssociation
# RuleProfile_DiagnosisAssociation.S <- "Default"


# Use tryCatch to catch warnings and errors
# Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
tryCatch({


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 1)  Harmonization of Raw Data Set meta data and structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Transform table names
#   - Add empty tables in data set if they are missing in raw data
#   - Rename features
#   - In tables with missing features, add empty features accordingly
#-------------------------------------------------------------------------------

# Setting up 'ls_DataSet' as object that holds all data throughout the function. It will be un- and repacked a couple of times during processing.
ls_DataSet <- RawDataSet


# Rename tables (Remove the 'RDS_'-prefix)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(ls_DataSet) <- sapply(names(ls_DataSet),
                            function(TableName) { str_remove(TableName, "RDS_") })


# If tables are missing, create corresponding empty tables for easier management throughout following processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AllTableNames <- dsCCPhos::Meta_TableNames$TableName_Curated
MissingTableNames <- AllTableNames[!(AllTableNames %in% names(ls_DataSet))]

# Create empty data frames for missing tables
if (length(MissingTableNames) > 0)
{
    for (i in 1:length(MissingTableNames))
    {
        ls_DataSet[[MissingTableNames[i]]] <- data.frame()
    }
}

# Reestablish original order of tables in ls_DataSet
ls_DataSet <- ls_DataSet[AllTableNames]


# Rename features
#~~~~~~~~~~~~~~~~

# Looping through tables to rename features
ls_DataSet <- ls_DataSet %>%
                  imap(function(dataframe, name)
                       {
                          # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                          vc_Lookup <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == name)$FeatureName_Raw
                          names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == name)$FeatureName_Curated

                          if (!is_empty(dataframe))
                          {
                              # Rename feature names according to look-up vector
                              dplyr::rename(dataframe, any_of(vc_Lookup))      # Returns a tibble
                          }
                          else
                          {
                              # Create empty data.frame with pre-defined column names
                              df <- data.frame(matrix(nrow = 0,
                                                      ncol = length(names(vc_Lookup)))) %>%
                                        setNames(names(vc_Lookup)) %>%
                                        mutate(across(everything(), ~ as.character(.x)))

                              return(df)
                          }
                       })


# In tables with missing features, add empty features accordingly
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_DataSet <- ls_DataSet %>%
                  imap(function(dataframe, name)
                       {
                          # Determine missing features
                          RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == name)$FeatureName_Curated
                          PresentFeatureNames <- names(dataframe)
                          MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

                          # If a table misses features, add empty columns accordingly
                          if (length(MissingFeatures) > 0)
                          {
                              ComplementedDataFrame <- dataframe %>%
                                                          mutate(!!!set_names(rep(list(NA_character_), length(MissingFeatures)), MissingFeatures))

                              return(ComplementedDataFrame)
                          }
                          else
                          { return(dataframe) }
                       })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 2)  Primary table cleaning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Remove entries that are not linked to related tables
#   - Remove duplicate entries
#-------------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count unlinked entries (Part 1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count entries in data frames before cleaning
CountEntriesBeforeCleaning <- ls_DataSet %>%
                                  map_int(\(dataframe) ifelse (!is.null(nrow(dataframe)), nrow(dataframe), 0))


# Set up progress bar
#-------------------------------------------------------------------------------
CountProgressItems <- 15
ProgressBar <- progress_bar$new(format = "Cleaning table entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())
#-------------------------------------------------------------------------------



ls_DataSet <- ls_DataSet %>%
                  imap(function(dataframe, name)
                         {
                            if (name == "Patient")
                            {
                                # Get vector of PatientIDs that are linked with one or more DiagnosisIDs
                                vc_EligiblePatientIDs <- dataframe %>%
                                                              left_join(ls_DataSet$Diagnosis, by = join_by(PatientID)) %>%
                                                              filter(!is.na(DiagnosisID)) %>%
                                                              pull(PatientID)

                                # Filter out patient entries with no related diagnosis data and keep only distinct rows (removal of duplicates)
                                return(dataframe %>%
                                            filter(PatientID %in% vc_EligiblePatientIDs) %>%
                                            distinct())
                            }

                            if (name == "Diagnosis")
                            {
                                # Get vector of DiagnosisIDs that are linked with a PatientID
                                vc_EligibleDiagnosisIDs <- dataframe %>%
                                                                left_join(ls_DataSet$Patient, by = join_by(PatientID)) %>%
                                                                filter(!is.na(PatientID)) %>%
                                                                pull(DiagnosisID)

                                # Filter out diagnosis entries with no related patient data and keep only distinct rows (removal of duplicates)
                                return(dataframe %>%
                                            filter(DiagnosisID %in% vc_EligibleDiagnosisIDs) %>%
                                            distinct(across(-DiagnosisID), .keep_all = TRUE))      # Keep only rows that are distinct (everywhere but DiagnosisID)
                            }

                            if (name == "Histology")
                            {
                                # Get vector of HistologyIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleHistologyIDs <- dataframe %>%
                                                                left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                filter(!is.na(DiagnosisID)) %>%
                                                                pull(HistologyID)

                                # Filter out histology entries with no related diagnosis data and keep only distinct rows (removal of duplicates)
                                return(dataframe %>%
                                            filter(HistologyID %in% vc_EligibleHistologyIDs) %>%
                                            distinct(across(-HistologyID), .keep_all = TRUE))
                            }

                            if (name == "BioSampling")
                            {
                                if (!(is.null(dataframe) | length(dataframe) == 0))
                                {
                                    # Get vector of SampleIDs that are linked with a PatientID
                                    vc_EligibleSampleIDs <- dataframe %>%
                                                                left_join(ls_DataSet$Patient, by = join_by(PatientID)) %>%
                                                                filter(!is.na(PatientID)) %>%
                                                                pull(SampleID)

                                    # Filter out...
                                    return(dataframe %>%
                                                filter(SampleID %in% vc_EligibleSampleIDs) %>%
                                                distinct())
                                }
                                else (return(dataframe))
                            }

                            if (name == "GeneralCondition")
                            {
                                if (!(is.null(dataframe) | length(dataframe) == 0))
                                {
                                    # Get vector of GeneralConditionIDs that are linked with a PatientID
                                    vc_EligibleGeneralConditionIDs <- dataframe %>%
                                                                          left_join(ls_DataSet$Patient, by = join_by(PatientID)) %>%
                                                                          filter(!is.na(PatientID)) %>%
                                                                          pull(GeneralConditionID)

                                    # Filter out...
                                    return(dataframe %>%
                                                filter(GeneralConditionID %in% vc_EligibleGeneralConditionIDs) %>%
                                                distinct())
                                }
                                else (return(dataframe))
                            }

                            if (name == "Metastasis")
                            {
                                # Get vector of MetastasisIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleMetastasisIDs <- dataframe %>%
                                                                left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                filter(!is.na(DiagnosisID)) %>%
                                                                pull(MetastasisID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(MetastasisID %in% vc_EligibleMetastasisIDs) %>%
                                            distinct(across(-MetastasisID), .keep_all = TRUE))
                            }

                            if (name == "MolecularDiagnostics")
                            {
                                if (!(is.null(dataframe) | length(dataframe) == 0))
                                {
                                    # Get vector of MolecularDiagnosticsIDs that are linked with a PatientID / DiagnosisID
                                    vc_EligibleMolecularDiagnosticsIDs <- dataframe %>%
                                                                              left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                              filter(!is.na(DiagnosisID)) %>%
                                                                              pull(MolecularDiagnosticsID)

                                    # Filter out...
                                    return(dataframe %>%
                                                filter(MolecularDiagnosticsID %in% vc_EligibleMolecularDiagnosticsIDs) %>%
                                                distinct(across(-MolecularDiagnosticsID), .keep_all = TRUE))
                                }
                                else (return(dataframe))
                            }

                            if (name == "OtherClassification")
                            {
                                # Get vector of OtherClassifcationIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleOtherClassificationIDs <- dataframe %>%
                                                                          left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                          filter(!is.na(DiagnosisID)) %>%
                                                                          pull(OtherClassificationID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(OtherClassificationID %in% vc_EligibleOtherClassificationIDs) %>%
                                            distinct(across(-OtherClassificationID), .keep_all = TRUE))
                            }

                            if (name == "Progress")
                            {
                                # Get vector of ProgressIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleProgressIDs <- dataframe %>%
                                                              left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                              filter(!is.na(DiagnosisID)) %>%
                                                              pull(ProgressID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(ProgressID %in% vc_EligibleProgressIDs) %>%
                                            distinct(across(-ProgressID), .keep_all = TRUE))
                            }

                            if (name == "RadiationTherapy")
                            {
                                # Get vector of RadiationTherapyIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleRadiationTherapyIDs <- dataframe %>%
                                                                      left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                      filter(!is.na(DiagnosisID)) %>%
                                                                      pull(RadiationTherapyID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(RadiationTherapyID %in% vc_EligibleRadiationTherapyIDs) %>%
                                            distinct(across(-RadiationTherapyID), .keep_all = TRUE))
                            }

                            if (name == "Staging")
                            {
                                # Get vector of StagingIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleStagingIDs <- dataframe %>%
                                                              left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                              filter(!is.na(DiagnosisID)) %>%
                                                              pull(StagingID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(StagingID %in% vc_EligibleStagingIDs) %>%
                                            distinct(across(-StagingID), .keep_all = TRUE))
                            }

                            if (name == "Surgery")
                            {
                                # Get vector of SurgeryIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleSurgeryIDs <- dataframe %>%
                                                              left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                              filter(!is.na(DiagnosisID)) %>%
                                                              pull(SurgeryID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(SurgeryID %in% vc_EligibleSurgeryIDs) %>%
                                            distinct(across(-SurgeryID), .keep_all = TRUE))
                            }

                            if (name == "SystemicTherapy")
                            {
                                # Get vector of SystemicTherapyIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleSystemicTherapyIDs <- dataframe %>%
                                                                      left_join(ls_DataSet$Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                                                      filter(!is.na(DiagnosisID)) %>%
                                                                      pull(SystemicTherapyID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(SystemicTherapyID %in% vc_EligibleSystemicTherapyIDs) %>%
                                            distinct(across(-SystemicTherapyID), .keep_all = TRUE))
                            }

                            if (name == "TherapyRecommendation")
                            {
                                # Get vector of TherapyRecommendationIDs that are linked with a PatientID / DiagnosisID
                                vc_EligibleTherapyRecommendationIDs <- dataframe %>%
                                                                            left_join(ls_DataSet$Diagnosis, by = join_by(PatientID)) %>%
                                                                            filter(!is.na(DiagnosisID)) %>%
                                                                            pull(TherapyRecommendationID)

                                # Filter out...
                                return(dataframe %>%
                                            filter(TherapyRecommendationID %in% vc_EligibleTherapyRecommendationIDs) %>%
                                            distinct(across(-TherapyRecommendationID), .keep_all = TRUE))
                            }

                            try(ProgressBar$tick()) })

try(ProgressBar$terminate())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count unlinked entries (Part 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count entries in data frames after cleaning
CountEntriesAfterCleaning <- ls_DataSet %>%
                                  map_int(\(dataframe) ifelse (!is.null(nrow(dataframe)), nrow(dataframe), 0))

# Count unlinked entries
CountUnlinkedEntries <- CountEntriesBeforeCleaning - CountEntriesAfterCleaning



# Print messages for live monitoring in local tests
cat("\n")
for (i in 1:length(CountUnlinkedEntries))
{
    Message <- paste0("Removed ", CountUnlinkedEntries[i], " unlinked entries from '", names(CountUnlinkedEntries)[i], "' table.")
    cli::cat_bullet(Message, bullet = "info")

    # Save messages in output object
    Messages$UnlinkedEntries <- c(Messages$UnlinkedEntries,
                                  info = Message)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 3)  Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A) Definition of features to monitor during Transformation
#   B) Tracking of raw feature values
#   C) Data harmonization (correctional transformation)
#   D) Tracking of harmonized feature values
#   E) Data recoding and formatting
#   F) Tracking of recoded / formatted feature values
#   G) Finalize transformation of data
#        - Removing of ineligible values
#        - Optional conversion to factor
#   H) Tracking of finalized feature values
#   I) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------


# Set up progress bar
CountProgressItems <- 37
ProgressBar <- progress_bar$new(format = "Harmonizing data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
try(ProgressBar$tick())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 A)  Definition of tracked features and their sets of eligible values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     - Create a list of meta data objects to make them passable to functions (alphabetic order)
#     - Names must be in same order as in ls_DataSet
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data by auxiliary function f_GetEligibleValues()
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

# Auxiliary function to extract eligible values from meta data
f_GetEligibleValues <- function(TableName,
                                FeatureName)
{
    dsCCPhos::Meta_ValueSets %>%
        filter(Table == TableName,
               Feature == FeatureName) %>%
        select(Value_Raw,
               Value_Curated)
}

# List object containing meta data
ls_MonitorMetaData <- list(BioSampling = list(Aliquot = f_GetEligibleValues("BioSampling", "Aliquot"),
                                              Type = f_GetEligibleValues("BioSampling", "Type"),
                                              TypeCXX = f_GetEligibleValues("BioSampling", "TypeCXX"),
                                              Status = f_GetEligibleValues("BioSampling", "Status")),

                           Diagnosis = list(ICD10Version = NULL,
                                            LocalizationSide = f_GetEligibleValues("Diagnosis", "LocalizationSide"),
                                            DiagnosisConfirmation = f_GetEligibleValues("Diagnosis", "DiagnosisConfirmation")),

                           GeneralCondition = list(ECOG = f_GetEligibleValues("GeneralCondition", "ECOG")),

                           Histology = list(Grading = f_GetEligibleValues("Histology", "Grading")),

                           Metastasis = list(Localization = f_GetEligibleValues("Metastasis", "Localization")),

                           MolecularDiagnostics = list(),

                           OtherClassification = list(),

                           Patient = list(Gender = f_GetEligibleValues("Patient", "Gender"),
                                          LastVitalStatus = f_GetEligibleValues("Patient", "LastVitalStatus")),

                           Progress = list(GlobalStatus = f_GetEligibleValues("Progress", "GlobalStatus"),
                                           LymphnodalStatus = f_GetEligibleValues("Progress", "LymphnodalStatus"),
                                           MetastasisStatus = f_GetEligibleValues("Progress", "MetastasisStatus"),
                                           PrimarySiteStatus = f_GetEligibleValues("Progress", "PrimarySiteStatus")),

                           RadiationTherapy = list(ApplicationType = f_GetEligibleValues("RadiationTherapy", "ApplicationType"),
                                                   Boost = f_GetEligibleValues("RadiationTherapy", "Boost"),
                                                   Intention = f_GetEligibleValues("RadiationTherapy", "Intention"),
                                                   RadiationType = f_GetEligibleValues("RadiationTherapy", "RadiationType"),
                                                   RelationToSurgery = f_GetEligibleValues("RadiationTherapy", "RelationToSurgery"),
                                                   SingleDailyDoseUnit = f_GetEligibleValues("RadiationTherapy", "SingleDailyDoseUnit"),
                                                   EndReason = f_GetEligibleValues("RadiationTherapy", "EndReason"),
                                                   TargetArea = f_GetEligibleValues("RadiationTherapy", "TargetArea"),
                                                   TotalDoseUnit = f_GetEligibleValues("RadiationTherapy", "TotalDoseUnit")),

                           Staging = list(TNMVersion = f_GetEligibleValues("Staging", "TNMVersion"),
                                          TNM_L = f_GetEligibleValues("Staging", "TNM_L"),
                                          TNM_M = f_GetEligibleValues("Staging", "TNM_M"),
                                          TNM_M_Prefix = f_GetEligibleValues("Staging", "TNM_M_Prefix"),
                                          TNM_mSymbol = f_GetEligibleValues("Staging", "TNM_mSymbol"),
                                          TNM_N = f_GetEligibleValues("Staging", "TNM_N"),
                                          TNM_N_Prefix = f_GetEligibleValues("Staging", "TNM_N_Prefix"),
                                          TNM_Pn = f_GetEligibleValues("Staging", "TNM_Pn"),
                                          TNM_rSymbol = f_GetEligibleValues("Staging", "TNM_rSymbol"),
                                          TNM_S = f_GetEligibleValues("Staging", "TNM_S"),
                                          TNM_T = f_GetEligibleValues("Staging", "TNM_T"),
                                          TNM_T_Prefix = f_GetEligibleValues("Staging", "TNM_T_Prefix"),
                                          TNM_V = f_GetEligibleValues("Staging", "TNM_V"),
                                          TNM_ySymbol = f_GetEligibleValues("Staging", "TNM_ySymbol"),
                                          UICCStage = f_GetEligibleValues("Staging", "UICCStage")),

                           Surgery = list(Intention = f_GetEligibleValues("Surgery", "Intention"),
                                          OPSVersion = f_GetEligibleValues("Surgery", "OPSVersion"),
                                          ResidualAssessmentLocal = f_GetEligibleValues("Surgery", "ResidualAssessmentLocal"),
                                          ResidualAssessmentTotal = f_GetEligibleValues("Surgery", "ResidualAssessmentTotal"),
                                          SurgeryComplicationsADT = f_GetEligibleValues("Surgery", "SurgeryComplicationsADT")),

                           SystemicTherapy = list(CTCAEGrade = f_GetEligibleValues("SystemicTherapy", "CTCAEGrade"),
                                                  Intention = f_GetEligibleValues("SystemicTherapy", "Intention"),
                                                  IsChemotherapy = NULL,
                                                  IsImmunotherapy = NULL,
                                                  IsHormoneTherapy = NULL,
                                                  IsBoneMarrowTransplant = NULL,
                                                  RelationToSurgery = f_GetEligibleValues("SystemicTherapy", "RelationToSurgery"),
                                                  Type = f_GetEligibleValues("SystemicTherapy", "Type")),

                           TherapyRecommendation = list(Deviation = f_GetEligibleValues("TherapyRecommendation", "Deviation"),
                                                        Type = f_GetEligibleValues("TherapyRecommendation", "Type")))

# Make sure object names in ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
ls_MonitorMetaData <- ls_MonitorMetaData[names(ls_DataSet)]

try(ProgressBar$tick())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 B)  Track feature values of raw data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Get unique raw values and their frequencies for monitoring
#   - Copy values of monitored features and mark them with TrackID that has correspondent in actually processed data frames
#-------------------------------------------------------------------------------

# Check if object names in ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    # Initiate list of data frames containing transformation tracks
    # First step: Store copied raw values of monitored features and mark them with "TrackID" to track them along transformation process
    ls_TransformationTracks <- purrr::map2(.x = ls_DataSet,
                                           .y = ls_MonitorMetaData,
                                           .f = function(DataFrame, MonitorMetaData)
                                                {
                                                    if (purrr::is_empty(MonitorMetaData) == FALSE)
                                                    {
                                                         DataFrame %>%
                                                              select(names(MonitorMetaData)) %>%
                                                              rename_with(.fn = ~ str_c(., "__Raw"),   # Two underscores for later use in pivot_longer()
                                                                          .cols = everything()) %>%
                                                              mutate(TrackID = row_number(), .before = 1)   # Create TrackID to enable correct mapping and further processing
                                                    }
                                                    else { return(data.frame()) }
                                                })

    ls_ValueCounts_Raw <- purrr::map2(.x = ls_DataSet,
                                      .y = ls_MonitorMetaData,
                                      .f = function(DataFrame, MonitorMetaData)
                                           {
                                               DataFrame %>%
                                                   dsCCPhos::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                              TransformationStage = "Raw") %>%
                                                   select(Feature,
                                                          Value,
                                                          Frequency) %>%
                                                   rename(Value_Raw = Value,
                                                          Count_Raw = Frequency)
                                           })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}

try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 C)  Data harmonization (correctional transformation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - General rules using regular expressions and functions defined in rule set
#   - Direct value replacement using hash tables defined in rule set
#-------------------------------------------------------------------------------

ls_DataSet <- purrr::map2(.x = ls_DataSet,
                          .y = names(ls_DataSet),
                          .f = function(DataFrame, TableName)
                               {
                                   DataFrame %>%
                                        mutate(TrackID = row_number()) %>%      # Enables tracking of transformation (see above)
                                        dsCCPhos::HarmonizeData(TableName = TableName,
                                                                RuleSet = RuleSet_RawDataHarmonization.S,
                                                                RuleProfile = RuleProfile_RawDataHarmonization.S)
                               })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 D)  Track feature values after Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map raw values to their harmonized state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(ls_DataSet)) & all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                     ls_DataSet,
                                                     ls_MonitorMetaData),
                                           .f = function(TransformationTracks, HarmonizedDataFrame, MonitorMetaData)
                                                {
                                                     if (purrr::is_empty(MonitorMetaData) == FALSE)
                                                     {
                                                         HarmonizedValues <- HarmonizedDataFrame %>%
                                                                                   select(c("TrackID", names(MonitorMetaData))) %>%
                                                                                   rename_with(.fn = ~ str_c(., "__Harmonized"),   # Two underscores for later use in pivot_longer()
                                                                                               .cols = all_of(names(MonitorMetaData)))

                                                         TransformationTracks %>%
                                                             left_join(HarmonizedValues, by = join_by(TrackID)) %>%
                                                             distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                                     }
                                                     else { return(data.frame()) }
                                                })
} else {
    stop("Internal error: Object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in harmonized data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Harmonized <- map2(.x = ls_DataSet,
                                      .y = ls_MonitorMetaData,
                                      .f = function(DataFrame, MonitorMetaData)
                                           {
                                               DataFrame %>%
                                                  dsCCPhos::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                             TransformationStage = "Harmonized") %>%
                                                  select(Feature,
                                                         Value,
                                                         Frequency) %>%
                                                  rename(Value_Harmonized = Value,
                                                         Count_Harmonized = Frequency)
                                           })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 E)  Data recoding and formatting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Recoding data using dsCCPhos::RecodeData()
#   - dsCCPhos::RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#   - Data formatting instructions
#-------------------------------------------------------------------------------


# Recoding df_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_BioSampling <- ls_DataSet$BioSampling %>%
                      #--- Recoding --------------------------------------------
                      mutate(Aliquot = dsCCPhos::RecodeData(Aliquot, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "Aliquot"),
                                                                          set_names(Value_Curated, Value_Raw))),
                             Status = dsCCPhos::RecodeData(Status, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "Status"),
                                                                        set_names(Value_Curated, Value_Raw))),
                             Type = dsCCPhos::RecodeData(Type, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "Type"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                                    set_names(Value_Curated, Value_Raw))),      # This returns a vector of the form c("Value_Raw1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                             TypeCXX = dsCCPhos::RecodeData(TypeCXX, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "TypeCXX"),
                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                      #--- Formatting ------------------------------------------
                      mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Recoding df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Diagnosis <- ls_DataSet$Diagnosis %>%
                    #--- Recoding ------------------------------------------
                    mutate(LocalizationSide = dsCCPhos::RecodeData(LocalizationSide, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Diagnosis" & Feature == "LocalizationSide"),
                                                                                          set_names(Value_Curated, Value_Raw))),
                           DiagnosisConfirmation = dsCCPhos::RecodeData(DiagnosisConfirmation, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Diagnosis" & Feature == "DiagnosisConfirmation"),
                                                                                                    set_names(Value_Curated, Value_Raw)))) %>%
                    #--- Formatting ----------------------------------------
                    mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"),
                           ICD10Version = as.integer(str_extract(ICD10Version, "\\d+")))      # Extract ICD-10 catalogue version year from string
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Recoding df_GeneralCondition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_GeneralCondition <- ls_DataSet$GeneralCondition %>%
                            #--- Recoding ------------------------------------------
                            mutate(ECOG = dsCCPhos::RecodeData(ECOG, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "GeneralCondition" & Feature == "ECOG"),
                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                            #--- Formatting ----------------------------------------
                            mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                            #--- Update PB ---
                            try(ProgressBar$tick())


# Recoding df_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Histology <- ls_DataSet$Histology %>%
                    #--- Recoding ------------------------------------------
                    mutate(Grading = dsCCPhos::RecodeData(Grading, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading"),
                                                                        set_names(Value_Curated, Value_Raw)))) %>%
                    #--- Formatting ----------------------------------------
                    mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Recoding df_Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Metastasis <- ls_DataSet$Metastasis %>%
                      #--- Recoding ------------------------------------------
                      mutate(Localization = dsCCPhos::RecodeData(Localization, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Metastasis" & Feature == "Localization"),
                                                                                    set_names(Value_Curated, Value_Raw)))) %>%
                      #--- Formatting --------------------------------------
                      mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"),
                             HasMetastasis = as.logical(HasMetastasis))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Recoding df_MolecularDiagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_MolecularDiagnostics <- ls_DataSet$MolecularDiagnostics %>%
                                #--- Formatting ----------------------------
                                mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                                #--- Update PB ---
                                try(ProgressBar$tick())


# Recoding df_OtherClassfication
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_OtherClassification <- ls_DataSet$OtherClassification %>%
                              #--- Formatting ----------------------------
                              mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                              #--- Update PB ---
                              try(ProgressBar$tick())


# Recoding df_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Patient <- ls_DataSet$Patient %>%
                  #--- Recoding ------------------------------------------
                  mutate(Gender = dsCCPhos::RecodeData(Gender, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "Gender"),
                                                                    set_names(Value_Curated, Value_Raw))),
                         LastVitalStatus = dsCCPhos::RecodeData(LastVitalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "LastVitalStatus"),
                                                                                      set_names(Value_Curated, Value_Raw)))) %>%
                  #--- Formatting ----------------------------------------
                  mutate(LastVitalStatusDate = format(as_datetime(LastVitalStatusDate), format = "%Y-%m-%d"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Recoding df_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Progress <- ls_DataSet$Progress %>%
                    #--- Recoding ------------------------------------------
                    mutate(GlobalStatus = dsCCPhos::RecodeData(GlobalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "GlobalStatus"),
                                                                                  set_names(Value_Curated, Value_Raw))),
                           PrimarySiteStatus = dsCCPhos::RecodeData(PrimarySiteStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "PrimarySiteStatus"),
                                                                                            set_names(Value_Curated, Value_Raw))),
                           LymphnodalStatus = dsCCPhos::RecodeData(LymphnodalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "LymphnodalStatus"),
                                                                                          set_names(Value_Curated, Value_Raw))),
                           MetastasisStatus = dsCCPhos::RecodeData(MetastasisStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Progress" & Feature == "MetastasisStatus"),
                                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                    #--- Formatting ----------------------------------------
                    mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                           #LocalRelapseDate = format(as_datetime(LocalRelapseDate), format = "%Y-%m-%d"))
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Recoding df_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_RadiationTherapy <- ls_DataSet$RadiationTherapy %>%
                            #--- Recoding ----------------------------------
                            mutate(RelationToSurgery = dsCCPhos::RecodeData(RelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RelationToSurgery"),
                                                                                                    set_names(Value_Curated, Value_Raw))),
                                   Intention = dsCCPhos::RecodeData(Intention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "Intention"),
                                                                                    set_names(Value_Curated, Value_Raw))),
                                   ApplicationType = dsCCPhos::RecodeData(ApplicationType, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "ApplicationType"),
                                                                                                set_names(Value_Curated, Value_Raw))),
                                   RadiationType = dsCCPhos::RecodeData(RadiationType, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationType"),
                                                                                            set_names(Value_Curated, Value_Raw)))) %>%
                            #--- Formatting --------------------------------
                            mutate(StartDate = format(as_datetime(StartDate), format = "%Y-%m-%d"),
                                   EndDate = format(as_datetime(EndDate), format = "%Y-%m-%d"))
                            #--- Update PB ---
                            try(ProgressBar$tick())


# Recoding df_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Staging <- ls_DataSet$Staging %>%
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
                  mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Recoding df_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Surgery <- ls_DataSet$Surgery %>%
                  #--- Recoding --------------------------------------------
                  mutate(Intention = dsCCPhos::RecodeData(Intention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "Intention"),
                                                                          set_names(Value_Curated, Value_Raw))),
                         ResidualAssessmentLocal = dsCCPhos::RecodeData(ResidualAssessmentLocal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal"),
                                                                                                      set_names(Value_Curated, Value_Raw))),
                         ResidualAssessmentTotal = dsCCPhos::RecodeData(ResidualAssessmentTotal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal"),
                                                                                                      set_names(Value_Curated, Value_Raw)))) %>%
                  #--- Formatting ------------------------------------------
                  mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Recoding df_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SystemicTherapy <- ls_DataSet$SystemicTherapy %>%
                          #--- Recoding ------------------------------------
                          mutate(Intention = dsCCPhos::RecodeData(Intention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "Intention"),
                                                                                  set_names(Value_Curated, Value_Raw))),
                                 RelationToSurgery = dsCCPhos::RecodeData(RelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "SystemicTherapy" & Feature == "RelationToSurgery"),
                                                                                                  set_names(Value_Curated, Value_Raw)))) %>%
                          #--- Formatting ----------------------------------
                          mutate(IsChemotherapy = as.logical(IsChemotherapy),
                                 IsImmunotherapy = as.logical(IsImmunotherapy),
                                 IsHormoneTherapy = as.logical(IsHormoneTherapy),
                                 IsBoneMarrowTransplant = as.logical(IsBoneMarrowTransplant),
                                 StartDate = format(as_datetime(StartDate), format = "%Y-%m-%d"),
                                 EndDate = format(as_datetime(EndDate), format = "%Y-%m-%d"))
                          #--- Update PB ---
                          try(ProgressBar$tick())


# Recoding df_TherapyRecommendation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_TherapyRecommendation <- ls_DataSet$TherapyRecommendation %>%
                                #--- Formatting ----------------------------
                                mutate(Date = format(as_datetime(Date), format = "%Y-%m-%d"))
                                #--- Update PB ---
                                try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 F)  Track feature values after Recoding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Re-pack data frames into list in order to pass them to map-function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Names have to be exactly the same (and the same order) as in ls_MonitorMetaData_all
#-------------------------------------------------------------------------------
ls_DataSet <- list(BioSampling = df_BioSampling,
                   Diagnosis = df_Diagnosis,
                   GeneralCondition = df_GeneralCondition,
                   Histology = df_Histology,
                   Metastasis = df_Metastasis,
                   MolecularDiagnostics = df_MolecularDiagnostics,
                   OtherClassification = df_OtherClassification,
                   Patient = df_Patient,
                   Progress = df_Progress,
                   RadiationTherapy = df_RadiationTherapy,
                   Staging = df_Staging,
                   Surgery = df_Surgery,
                   SystemicTherapy = df_SystemicTherapy,
                   TherapyRecommendation = df_TherapyRecommendation)
                   #--- Update PB ---
                   try(ProgressBar$tick())


# Map raw values to their recoded state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(ls_DataSet)) & all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                     ls_DataSet,
                                                     ls_MonitorMetaData),
                                           .f = function(TransformationTracks, RecodedDataFrame, MonitorMetaData)
                                                {
                                                     if (purrr::is_empty(MonitorMetaData) == FALSE)
                                                     {
                                                         RecodedValues <- RecodedDataFrame %>%
                                                                               select(c("TrackID", names(MonitorMetaData))) %>%
                                                                               rename_with(.fn = ~ str_c(., "__Recoded"),   # Two underscores for later use in pivot_longer()
                                                                                           .cols = all_of(names(MonitorMetaData)))

                                                         TransformationTracks %>%
                                                             left_join(RecodedValues,
                                                                       by = join_by(TrackID)) %>%
                                                             distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                                     }
                                                     else { return(data.frame()) }
                                                })
} else {
    stop("Internal error: Object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in recoded data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Recoded <- map2(.x = ls_DataSet,
                                   .y = ls_MonitorMetaData,
                                   .f = function(DataFrame, MonitorMetaData)
                                        {
                                            DataFrame %>%
                                               dsCCPhos::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                          TransformationStage = "Recoded") %>%
                                               select(Feature,
                                                      Value,
                                                      Frequency) %>%
                                               rename(Value_Recoded = Value,
                                                      Count_Recoded = Frequency)
                                        })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 G)  Finalize transformation of data values using dsCCPhos::FinalizeDataTransformation()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - (Optional / Default) Exclusion of ineligible data (including data that could not be transformed)
#   - (Optional) Conversion to ordered factor
#   - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#   - All predefined information stored in dsCCPhos::Meta_ValueSets
#-------------------------------------------------------------------------------

# Finalize df_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_BioSampling <- df_BioSampling %>%
                      mutate(Type = dsCCPhos::FinalizeDataTransformation(Type, TableName = "BioSampling", FeatureName = "Type"),
                             TypeCXX = dsCCPhos::FinalizeDataTransformation(TypeCXX, TableName = "BioSampling", FeatureName = "TypeCXX"),
                             Aliquot = dsCCPhos::FinalizeDataTransformation(Aliquot, TableName = "BioSampling", FeatureName = "Aliquot"))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Finalize df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Diagnosis <- df_Diagnosis %>%
                    mutate(LocalizationSide = dsCCPhos::FinalizeDataTransformation(LocalizationSide, TableName = "Diagnosis", FeatureName = "LocalizationSide"))   # Assign factor labels?
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Finalize df_GeneralCondition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_GeneralCondition <- df_GeneralCondition %>%
                            mutate(ECOG = dsCCPhos::FinalizeDataTransformation(ECOG, TableName = "GeneralCondition", FeatureName = "ECOG"))   # Assign factor labels?
                            #--- Update PB ---
                            try(ProgressBar$tick())


# Finalize df_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Histology <- df_Histology %>%
                    mutate(Grading = dsCCPhos::FinalizeDataTransformation(Grading, TableName = "Histology", FeatureName = "Grading"))   # Assign factor labels?
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Finalize df_OtherClassification
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_OtherClassification <- df_OtherClassification %>%
                              mutate(Classification = dsCCPhos::FinalizeDataTransformation(Classification, TableName = "OtherClassification", FeatureName = "Classification"))
                              #--- Update PB ---
                              try(ProgressBar$tick())


# Finalize df_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Patient <- df_Patient %>%
                  mutate(Gender = dsCCPhos::FinalizeDataTransformation(Gender, TableName = "Patient", FeatureName = "Gender"),   # Assign factor labels?
                         LastVitalStatus = dsCCPhos::FinalizeDataTransformation(LastVitalStatus, TableName = "Patient", FeatureName = "LastVitalStatus"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Finalize df_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Progress <- df_Progress %>%
                    mutate(GlobalStatus = dsCCPhos::FinalizeDataTransformation(GlobalStatus, TableName = "Progress", FeatureName = "GlobalStatus"),   # Assign factor labels?
                           PrimarySiteStatus = dsCCPhos::FinalizeDataTransformation(PrimarySiteStatus, TableName = "Progress", FeatureName = "PrimarySiteStatus"),   # Assign factor labels?
                           LymphnodalStatus = dsCCPhos::FinalizeDataTransformation(LymphnodalStatus, TableName = "Progress", FeatureName = "LymphnodalStatus"),   # Assign factor labels?
                           MetastasisStatus = dsCCPhos::FinalizeDataTransformation(MetastasisStatus, TableName = "Progress", FeatureName = "MetastasisStatus"))   # Assign factor labels?
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Finalize df_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_RadiationTherapy <- df_RadiationTherapy %>%
                            mutate(RelationToSurgery = dsCCPhos::FinalizeDataTransformation(RelationToSurgery, TableName = "RadiationTherapy", FeatureName = "RelationToSurgery"),   # Assign factor labels?
                                   Intention = dsCCPhos::FinalizeDataTransformation(Intention, TableName = "RadiationTherapy", FeatureName = "Intention"))   # Assign factor labels?
                            #--- Update PB ---
                            try(ProgressBar$tick())


# Finalize df_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Staging <- df_Staging %>%
                  mutate(UICCStage = dsCCPhos::FinalizeDataTransformation(UICCStage, TableName = "Staging", FeatureName = "UICCStage"),
                         TNM_T = dsCCPhos::FinalizeDataTransformation(TNM_T, TableName = "Staging", FeatureName = "TNM_T"),
                         TNM_N = dsCCPhos::FinalizeDataTransformation(TNM_N, TableName = "Staging", FeatureName = "TNM_N"),
                         TNM_M = dsCCPhos::FinalizeDataTransformation(TNM_M, TableName = "Staging", FeatureName = "TNM_M"),
                         TNM_T_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_T_Prefix, TableName = "Staging", FeatureName = "TNM_T_Prefix"),
                         TNM_N_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_N_Prefix, TableName = "Staging", FeatureName = "TNM_N_Prefix"),
                         TNM_M_Prefix = dsCCPhos::FinalizeDataTransformation(TNM_M_Prefix, TableName = "Staging", FeatureName = "TNM_M_Prefix"),
                         TNM_ySymbol = dsCCPhos::FinalizeDataTransformation(TNM_ySymbol, TableName = "Staging", FeatureName = "TNM_ySymbol"),
                         TNM_rSymbol = dsCCPhos::FinalizeDataTransformation(TNM_rSymbol, TableName = "Staging", FeatureName = "TNM_rSymbol"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Finalize df_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Surgery <- df_Surgery %>%
                  mutate(Intention = dsCCPhos::FinalizeDataTransformation(Intention, TableName = "Surgery", FeatureName = "Intention"),   # Assign factor labels?
                         ResidualAssessmentLocal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentLocal, TableName = "Surgery", FeatureName = "ResidualAssessmentLocal"),
                         ResidualAssessmentTotal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentTotal, TableName = "Surgery", FeatureName = "ResidualAssessmentTotal"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Finalize df_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SystemicTherapy <- df_SystemicTherapy %>%
                          mutate(RelationToSurgery = dsCCPhos::FinalizeDataTransformation(RelationToSurgery, TableName = "SystemicTherapy", FeatureName = "RelationToSurgery"),   # Assign factor labels?
                                 Intention = dsCCPhos::FinalizeDataTransformation(Intention, TableName = "SystemicTherapy", FeatureName = "Intention"))   # Assign factor labels?
                          #--- Update PB ---
                          try(ProgressBar$tick())


# Finalize df_TherapyRecommendation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_TherapyRecommendation <- df_TherapyRecommendation %>%
                                mutate(Type = dsCCPhos::FinalizeDataTransformation(Type, TableName = "TherapyRecommendation", FeatureName = "Type"))
                                #--- Update PB ---
                                try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 H)  Track Feature Values after Finalized Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Re-pack data frames into list in order to pass them to map-function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Names have to be exactly the same (and the same order) as in ls_MonitorMetaData_all
#-------------------------------------------------------------------------------
ls_DataSet <- list(BioSampling = df_BioSampling,
                   Diagnosis = df_Diagnosis,
                   GeneralCondition = df_GeneralCondition,
                   Histology = df_Histology,
                   Metastasis = df_Metastasis,
                   MolecularDiagnostics = df_MolecularDiagnostics,
                   OtherClassification = df_OtherClassification,
                   Patient = df_Patient,
                   Progress = df_Progress,
                   RadiationTherapy = df_RadiationTherapy,
                   Staging = df_Staging,
                   Surgery = df_Surgery,
                   SystemicTherapy = df_SystemicTherapy,
                   TherapyRecommendation = df_TherapyRecommendation)
                   #--- Update PB ---
                   try(ProgressBar$tick())


# Map raw values to their finalized state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(ls_DataSet)) & all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                     ls_DataSet,
                                                     ls_MonitorMetaData),
                                           .f = function(TransformationTrack, FinalizedDataFrame, MonitorMetaData)
                                                {
                                                     if (purrr::is_empty(MonitorMetaData) == FALSE)
                                                     {
                                                         FinalizedValues <- FinalizedDataFrame %>%
                                                                                 select(c("TrackID", names(MonitorMetaData))) %>%
                                                                                 rename_with(.fn = ~ str_c(., "__Final"),   # Two underscores for later use in pivot_longer()
                                                                                             .cols = all_of(names(MonitorMetaData)))

                                                         TransformationTrack %>%
                                                             left_join(FinalizedValues,
                                                                       by = join_by(TrackID)) %>%
                                                             distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                                     }
                                                     else { return(data.frame()) }
                                                })
} else {
    stop("Internal error: Object names in ls_TransformationTracks, ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in finalized data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Final <- map2(.x = ls_DataSet,
                                 .y = ls_MonitorMetaData,
                                 .f = function(DataFrame, MonitorMetaData)
                                      {
                                          DataFrame %>%
                                              dsCCPhos::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                         TransformationStage = "Final") %>%
                                              select(Feature,
                                                     Value,
                                                     Frequency) %>%
                                              rename(Value_Final = Value,
                                                     Count_Final = Frequency)
                                      })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 I)  Merge monitor objects into coherent summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Summarize Transformation Tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_TransformationTracks_Summaries <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                           ls_MonitorMetaData),
                                                 .f = function(TrackData,
                                                               MonitorMetaData)
                                                      {
                                                           if (!purrr::is_empty(TrackData)) { if (nrow(TrackData) > 0)
                                                           {
                                                               Summary <- TrackData %>%
                                                                               mutate(across(everything(), ~ as.character(.x))) %>%      # Turn all columns into character (necessary for pivot_longer() to work correctly)
                                                                               tidyr::pivot_longer(cols = c(everything(), -TrackID),
                                                                                                   names_to = c("Feature", "Stage"),
                                                                                                   names_sep = "(__)",      # Separate by '__'-string (two underscores)
                                                                                                   values_to = "Value") %>%
                                                                               tidyr::pivot_wider(names_from = Stage,
                                                                                                  values_from = Value) %>%
                                                                               select(-TrackID) %>%
                                                                               distinct() %>%
                                                                               rename(Value_Raw = Raw,
                                                                                      Value_Harmonized = Harmonized,
                                                                                      Value_Recoded = Recoded,
                                                                                      Value_Final = Final) %>%
                                                                               rowwise() %>%
                                                                               mutate(IsOccurring = TRUE,
                                                                                      IsEligible_Raw = ifelse(is.na(Value_Raw) | is.null(MonitorMetaData[[Feature]]),      # If value is NA or if there is no set of eligible values, set variable NA...
                                                                                                              NA,
                                                                                                              Value_Raw %in% MonitorMetaData[[Feature]]$Value_Raw),      # ... else check if specific row value is in set of eligible values
                                                                                      IsEligible_Harmonized = ifelse(is.na(Value_Harmonized) | is.null(MonitorMetaData[[Feature]]),
                                                                                                                     NA,
                                                                                                                     Value_Harmonized %in% MonitorMetaData[[Feature]]$Value_Raw),
                                                                                      IsEligible_Recoded = ifelse(is.na(Value_Recoded) | is.null(MonitorMetaData[[Feature]]),
                                                                                                                  NA,
                                                                                                                  Value_Recoded %in% MonitorMetaData[[Feature]]$Value_Curated),
                                                                                      IsEligible_Final = ifelse(is.na(Value_Final),
                                                                                                                NA,
                                                                                                                TRUE)) %>%
                                                                               ungroup()

                                                               # Add set of all eligible values regardless of occurrence to summary
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               for (i in 1:length(MonitorMetaData))   # Loop through all monitored features of a table
                                                               {
                                                                   AllEligibleValues <- tibble(Feature = names(MonitorMetaData)[i],
                                                                                               Value_Raw = MonitorMetaData[[i]]$Value_Raw,
                                                                                               IsOccurring = FALSE,
                                                                                               IsEligible_Raw = TRUE)

                                                                   Summary <- bind_rows(Summary,
                                                                                        AllEligibleValues)
                                                               }

                                                               # Filter out eligible values marked as not occurring if they actually occur
                                                               # Result: All eligible values are included in summary, regardless of occurrence
                                                               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                               Summary <- Summary %>%
                                                                              group_by(Feature, Value_Raw) %>%
                                                                                  arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                                                  slice_head() %>%
                                                                              ungroup() %>%
                                                                              arrange(Feature,
                                                                                      desc(IsOccurring),
                                                                                      desc(IsEligible_Raw),
                                                                                      desc(IsEligible_Harmonized),
                                                                                      Value_Raw)

                                                               return(Summary)
                                                           }
                                                           else { return (data.frame()) } }
                                                           else { return (data.frame()) }
                                                      })


# Create detailed transformation monitors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Joining of info from transformation tracks and value counts
#-------------------------------------------------------------------------------
ls_TransformationMonitors <- purrr::pmap(.l = list(ls_TransformationTracks_Summaries,
                                                   ls_ValueCounts_Raw,
                                                   ls_ValueCounts_Harmonized,
                                                   ls_ValueCounts_Recoded,
                                                   ls_ValueCounts_Final),
                                         .f = function(TransformationTracksSummary,
                                                       ValueCountsRaw,
                                                       ValueCountsHarmonized,
                                                       ValueCountsRecoded,
                                                       ValueCountsFinal)
                                              {
                                                   if (nrow(TransformationTracksSummary) > 0)
                                                   {
                                                       TransformationTracksSummary %>%
                                                           left_join(ValueCountsRaw, by = c("Feature", "Value_Raw")) %>%
                                                           left_join(ValueCountsHarmonized, by = c("Feature", "Value_Harmonized")) %>%
                                                           left_join(ValueCountsRecoded, by = c("Feature", "Value_Recoded")) %>%
                                                           left_join(ValueCountsFinal, by = c("Feature", "Value_Final")) %>%
                                                           mutate(Count_Harmonized = case_when(IsOccurring == FALSE ~ NA,
                                                                                               TRUE ~ Count_Harmonized),
                                                                  Count_Recoded = case_when(IsOccurring == FALSE ~ NA,
                                                                                            TRUE ~ Count_Recoded),
                                                                  Count_Final = case_when(IsOccurring == FALSE ~ NA,
                                                                                          TRUE ~ Count_Final)) %>%
                                                           arrange(Feature,
                                                                   desc(IsOccurring),
                                                                   desc(IsEligible_Raw),
                                                                   desc(IsEligible_Harmonized),
                                                                   Value_Raw)
                                                   }
                                                   else { return(NULL) }
                                              })


# Create overview of value eligibility in different transformation stages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_EligibilityOverviews <- purrr::pmap(.l = list(ls_TransformationMonitors,
                                                 ls_MonitorMetaData),
                                       .f = function(MonitorData,
                                                     MonitorMetaData)
                                            {
                                                if (!(purrr::is_empty(MonitorData) | is.null(MonitorData)))
                                                {
                                                    # Filter out features that are not meant to be monitored, e.g. do not have applicable eligibility criteria
                                                    MonitorData <- MonitorData %>%
                                                                        filter(Feature %in% names(MonitorMetaData))

                                                    SummaryRaw <- MonitorData %>%
                                                                      group_by(Feature, IsEligible_Raw) %>%
                                                                          summarize(Raw = sum(Count_Raw, na.rm = TRUE)) %>%
                                                                          rename(Eligibility = IsEligible_Raw)

                                                    SummaryHarmonized <- MonitorData %>%
                                                                              distinct(pick(Feature, Value_Harmonized), .keep_all = TRUE) %>%
                                                                              group_by(Feature, IsEligible_Harmonized) %>%
                                                                                  summarize(Harmonized = sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                                                  rename(Eligibility = IsEligible_Harmonized)

                                                    SummaryRecoded <- MonitorData %>%
                                                                          distinct(pick(Feature, Value_Recoded), .keep_all = TRUE) %>%
                                                                          group_by(Feature, IsEligible_Recoded) %>%
                                                                              summarize(Recoded = sum(Count_Recoded, na.rm = TRUE)) %>%
                                                                              rename(Eligibility = IsEligible_Recoded)

                                                    SummaryFinal <- MonitorData %>%
                                                                        distinct(pick(Feature, Value_Final), .keep_all = TRUE) %>%
                                                                        group_by(Feature, IsEligible_Final) %>%
                                                                            summarize(Final = sum(Count_Final, na.rm = TRUE)) %>%
                                                                            rename(Eligibility = IsEligible_Final)

                                                    Overview <- SummaryRaw %>%
                                                                    full_join(SummaryHarmonized, by = join_by(Feature, Eligibility)) %>%
                                                                    full_join(SummaryRecoded, by = join_by(Feature, Eligibility)) %>%
                                                                    full_join(SummaryFinal, by = join_by(Feature, Eligibility)) %>%
                                                                    arrange(Feature, desc(Eligibility)) %>%
                                                                    mutate(Eligibility = case_match(Eligibility,
                                                                                                    TRUE ~ "Eligible",
                                                                                                    FALSE ~ "Ineligible",
                                                                                                    NA ~ "Missing"))

                                                    # Get tibble of all combinations of occurring features and eligibility category
                                                    AllCombinations <- tibble(Feature = rep(unique(Overview$Feature), each = 3),
                                                                              Eligibility = rep(c("Eligible", "Ineligible", "Missing"), times = length(unique(Overview$Feature))))

                                                    # Finalize overview
                                                    Overview <- Overview %>%
                                                                    right_join(AllCombinations, by = join_by(Feature, Eligibility)) %>%
                                                                    mutate(across(c(Raw, Harmonized, Recoded, Final), ~ case_when(is.na(.x) ~ 0, .default = .x))) %>%       # Turn all NAs into 0 in count columns
                                                                    group_by(Feature) %>%
                                                                        mutate(across(c(Raw, Harmonized, Recoded, Final), ~ .x / sum(.x), .names = "{.col}_Proportional")) %>%      # Create proportional value columns
                                                                    ungroup() %>%
                                                                    arrange(Feature, factor(Eligibility, levels = c("Eligible", "Ineligible", "Missing")))
                                                }
                                                else { return(data.frame()) }
                                           })


# Create overview of value eligibility in different transformation stages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_ValueSetOverviews <- purrr::map(.x = ls_TransformationMonitors,
                                   .f = function(MonitorData)
                                        {
                                            if (!purrr::is_empty(MonitorData))
                                            {
                                                ValueSets <- list()

                                                ValueSets$Raw <- MonitorData %>%
                                                                      select(Feature, Value_Raw, IsOccurring, IsEligible_Raw, Count_Raw) %>%
                                                                      group_by(Feature) %>%
                                                                          mutate(Proportion_Raw = Count_Raw / sum(Count_Raw, na.rm = TRUE)) %>%
                                                                      ungroup()

                                                ValueSets$Harmonized <- MonitorData %>%
                                                                            group_by(Feature, Value_Harmonized, IsEligible_Harmonized) %>%
                                                                                summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                                            ungroup() %>%
                                                                            distinct(Feature, Value_Harmonized, .keep_all = TRUE) %>%
                                                                            group_by(Feature) %>%
                                                                                mutate(Proportion_Harmonized = Count_Harmonized / sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                                            ungroup()

                                                ValueSets$Recoded <- MonitorData %>%
                                                                          group_by(Feature, Value_Recoded, IsEligible_Recoded) %>%
                                                                              summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE)) %>%
                                                                          ungroup() %>%
                                                                          distinct(Feature, Value_Recoded, .keep_all = TRUE) %>%
                                                                          group_by(Feature) %>%
                                                                              mutate(Proportion_Recoded = Count_Recoded / sum(Count_Recoded, na.rm = TRUE)) %>%
                                                                          ungroup()

                                                ValueSets$Final <- MonitorData %>%
                                                                        group_by(Feature, Value_Final, IsEligible_Final) %>%
                                                                            summarize(Count_Final = sum(Count_Final, na.rm = TRUE)) %>%
                                                                        ungroup() %>%
                                                                        distinct(Feature, Value_Final, .keep_all = TRUE) %>%
                                                                        group_by(Feature) %>%
                                                                            mutate(Proportion_Final = Count_Final / sum(Count_Final, na.rm = TRUE)) %>%
                                                                        ungroup()

                                                return(ValueSets)
                                            }
                                            else { return(list()) }
                                        })



# Delete artificial "TrackID"-column from data frames (not needed anymore)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
try(df_BioSampling <- df_BioSampling %>% select(-TrackID))
try(df_Diagnosis <- df_Diagnosis %>% select(-TrackID))
try(df_GeneralCondition <- df_GeneralCondition %>% select(-TrackID))
try(df_Histology <- df_Histology %>% select(-TrackID))
try(df_Metastasis <- df_Metastasis %>% select(-TrackID))
try(df_MolecularDiagnostics <- df_MolecularDiagnostics %>% select(-TrackID))
try(df_OtherClassification <- df_OtherClassification %>% select(-TrackID))
try(df_Patient <- df_Patient %>% select(-TrackID))
try(df_Progress <- df_Progress %>% select(-TrackID))
try(df_RadiationTherapy <- df_RadiationTherapy %>% select(-TrackID))
try(df_Staging <- df_Staging %>% select(-TrackID))
try(df_Surgery <- df_Surgery %>% select(-TrackID))
try(df_SystemicTherapy <- df_SystemicTherapy %>% select(-TrackID))
try(df_TherapyRecommendation <- df_TherapyRecommendation %>% select(-TrackID))


try(ProgressBar$tick())
try(ProgressBar$terminate())

# Print info message
cli::cat_bullet("Data transformation monitors are stored in 'CurationReport$Transformation'", bullet = "info")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 4)  Process diagnosis data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A) Joining of df_Diagnosis and df_Histology to get coherent diagnosis data
#   B) Classification and removal of redundant diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   C) Classification and bundling of associated diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   D) Reconstruct df_Histology from df_Diagnosis
#-------------------------------------------------------------------------------


# Module 4 A) Join df_Diagnosis and df_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Create composed ID ('DiagnosisID / HistologyID')
#   - Add auxiliary features for filtering purposes
#-------------------------------------------------------------------------------

df_Diagnosis <- df_Diagnosis %>%
                    left_join(df_Histology, by = join_by(PatientID, DiagnosisID), suffix = c("_Diagnosis", "_Histology")) %>%
                    mutate(OriginalDiagnosisID = DiagnosisID,
                           DiagnosisID = paste0(DiagnosisID, "/", HistologyID)) %>%
                    relocate(DiagnosisID, .after = PatientID) %>%
                    group_by(PatientID) %>%
                        mutate(PatientCountInitialEntries = n()) %>%
                    ungroup()



# Module 4 B) Classification and removal of redundant entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisRedundancy() to identify and consolidate redundant diagnosis entries
#   - Rules for classification of redundancy are defined in customizable data object delivered with dsCCPhos
#   - Replace redundant DiagnosisIDs in all related tables
#-------------------------------------------------------------------------------


# Compile rule calls from data in RuleSet_DiagnosisRedundancy.S using dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Names of features that are required to compile rule calls from RuleSet_DiagnosisRedundancy.S
PredictorFeatures_DiagnosisRedundancy = c("CountDeviatingValues",
                                          "Date_Diagnosis",
                                          "ICD10Code",
                                          "ICDOTopographyCode",
                                          "LocalizationSide",
                                          "Date_Histology",
                                          "ICDOMorphologyCode",
                                          "Grading")

# Pass required information to dsCCPhos::CompileClassificationCall to compile rule calls (dplyr::case_when-Statements)
Call_IsLikelyRedundant <- CompileClassificationCall(TargetFeature = "IsLikelyRedundant",
                                                    PredictorFeatures = PredictorFeatures_DiagnosisRedundancy,
                                                    RuleSet = RuleSet_DiagnosisRedundancy.S,
                                                    RuleProfile = RuleProfile_DiagnosisRedundancy.S,
                                                    ValueIfNoRuleMet = FALSE)

# Make list of rule calls to pass them to function
RuleCalls_DiagnosisRedundancy <- list(IsLikelyRedundant = Call_IsLikelyRedundant)


# Set up progress bar
#-------------------------------------------------------------------------------
CountProgressItems <- df_Diagnosis %>% filter(PatientCountInitialEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying redundant diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------


# Filter patients with multiple diagnosis entries and apply dsCCPhos::ClassifyDiagnosisRedundancy()
df_Aux_Diagnosis_ClassifiedRedundancies <- df_Diagnosis %>%
                                                filter(PatientCountInitialEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisRedundancy(DiagnosisEntries = .x,
                                                                                               RuleCalls = RuleCalls_DiagnosisRedundancy,
                                                                                               ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble df_Diagnosis after processing of redundant diagnosis entries
df_Diagnosis <- df_Diagnosis %>%
                    filter(PatientCountInitialEntries == 1) %>%
                    bind_rows(df_Aux_Diagnosis_ClassifiedRedundancies) %>%
                    arrange(PatientID) %>%
                    group_by(PatientID) %>%
                        mutate(PatientCountDistinctEntries = n()) %>%
                    ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Redundant diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# a) Number of redundant diagnosis entries and
# b) Number of patients that had redundant diagnosis entries
CountDiagnosisRedundancies <- sum(df_Diagnosis$CountRedundancies, na.rm = TRUE)
CountPatientsWithDiagnosisRedundancies <- df_Diagnosis %>%
                                              filter(CountRedundancies > 0) %>%
                                              pull(PatientID) %>%
                                              n_distinct()

# Print message for live monitoring in local tests
Message <- paste0("Found ", CountDiagnosisRedundancies, " redundancies related to ", CountPatientsWithDiagnosisRedundancies, " patient IDs.")
cli::cat_bullet(Message, bullet = "info")

# Save message in output object
Messages$DiagnosisRedundancies <- Message

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Replace IDs (Original DiagnosisID and not newly composed one) of redundant diagnosis entries in related tables
# Get table of affected DiagnosisIDs
df_Aux_Diagnosis_IDMappingRedundancies <- df_Diagnosis %>%
                                              ungroup() %>%
                                              filter(CountRedundancies > 0) %>%
                                              select(PatientID, RedundantOriginalIDs, OriginalDiagnosisID) %>%
                                              unnest(cols = c(RedundantOriginalIDs)) %>%
                                              rename(all_of(c(OldDiagnosisID = "RedundantOriginalIDs",
                                                              NewDiagnosisID = "OriginalDiagnosisID"))) %>%
                                              filter(OldDiagnosisID != NewDiagnosisID) %>%
                                              distinct()

# Replace IDs of redundant diagnosis entries in related tables
df_Metastasis <- df_Metastasis %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_MolecularDiagnostics <- df_MolecularDiagnostics %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_Progress <- df_Progress %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_RadiationTherapy <- df_RadiationTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_Staging <- df_Staging %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_Surgery <- df_Surgery %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)
df_SystemicTherapy <- df_SystemicTherapy %>% ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)

# Remove columns of redundant IDs (not needed anymore)
df_Diagnosis <-  df_Diagnosis %>%
                      select(-c(RedundantIDs,
                                RedundantOriginalIDs,
                                CountRedundancies))



# Module 4 C) Classify associations between diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple distinct diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisAssociations() to plausibly distinguish pseudo-different from actually different diagnoses
#   - Rules for classification of associated diagnosis entries are defined in customizable data object delivered with dsCCPhos
#   - Replace DiagnosisIDs in all related tables with ReferenceDiagnosisID
#-------------------------------------------------------------------------------

# Names of features that are required to compile rule calls from RuleSet_DiagnosisAssociation.S
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
                                                     RuleSet = RuleSet_DiagnosisAssociation.S,
                                                     RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                     ValueIfNoRuleMet = FALSE)

Call_InconsistencyCheck <- CompileClassificationCall(TargetFeature = "InconsistencyCheck",
                                                     PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                     RuleSet = RuleSet_DiagnosisAssociation.S,
                                                     RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                     ValueIfNoRuleMet = "No apparent inconsistency")

Call_ImplausibilityCheck <- CompileClassificationCall(TargetFeature = "ImplausibilityCheck",
                                                      PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                      RuleSet = RuleSet_DiagnosisAssociation.S,
                                                      RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                      ValueIfNoRuleMet = "No apparent implausibility")

Call_Relation_ICD10 <- CompileClassificationCall(TargetFeature = "Relation_ICD10",
                                                 PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                 RuleSet = RuleSet_DiagnosisAssociation.S,
                                                 RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                 ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOTopography <- CompileClassificationCall(TargetFeature = "Relation_ICDOTopography",
                                                          PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                          RuleSet = RuleSet_DiagnosisAssociation.S,
                                                          RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                          ValueIfNoRuleMet = NA_character_)

Call_Relation_LocalizationSide <- CompileClassificationCall(TargetFeature = "Relation_LocalizationSide",
                                                            PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                            RuleSet = RuleSet_DiagnosisAssociation.S,
                                                            RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOMorphology <- CompileClassificationCall(TargetFeature = "Relation_ICDOMorphology",
                                                            PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                            RuleSet = RuleSet_DiagnosisAssociation.S,
                                                            RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_Grading <- CompileClassificationCall(TargetFeature = "Relation_Grading",
                                                   PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                   RuleSet = RuleSet_DiagnosisAssociation.S,
                                                   RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                   ValueIfNoRuleMet = NA_character_)

Call_IsLikelyProgression <- CompileClassificationCall(TargetFeature = "IsLikelyProgression",
                                                      PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                      RuleSet = RuleSet_DiagnosisAssociation.S,
                                                      RuleProfile = RuleProfile_DiagnosisAssociation.S,
                                                      ValueIfNoRuleMet = NA)

Call_IsLikelyRecoding <- CompileClassificationCall(TargetFeature = "IsLikelyRecoding",
                                                   PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                   RuleSet = RuleSet_DiagnosisAssociation.S,
                                                   RuleProfile = RuleProfile_DiagnosisAssociation.S,
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
CountProgressItems <- df_Diagnosis %>% filter(PatientCountDistinctEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying associated diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

# Filter patients with multiple distinct diagnosis entries and apply dsCCPhos::ClassifyDiagnosisAssociations()
df_Aux_Diagnosis_ClassifiedAssociations <- df_Diagnosis %>%
                                                filter(PatientCountDistinctEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisAssociation(DiagnosisEntries = .x,
                                                                                                RuleCalls = RuleCalls_DiagnosisAssociation,
                                                                                                ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble df_Diagnosis after processing of associated diagnosis entries
df_Diagnosis <- df_Diagnosis %>%
                    filter(PatientCountDistinctEntries == 1) %>%
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
CountDiagnosisAssociations <- sum(df_Diagnosis$IsLikelyAssociated, na.rm = TRUE)
CountPatientsWithDiagnosisAssociations <- df_Aux_Diagnosis_ClassifiedAssociations %>%
                                              filter(IsLikelyAssociated == TRUE) %>%
                                              pull(PatientID) %>%
                                              n_distinct()

# Print message for live monitoring in local tests
Message <- paste0("Classified ", CountDiagnosisAssociations, " associated diagnosis entries related to ", CountPatientsWithDiagnosisAssociations, " patient IDs.")
cli::cat_bullet(Message, bullet = "info")

# Save message in output object
Messages$DiagnosisAssociation <- Message

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Create table for DiagnosisID replacement in related tables
df_Aux_Diagnosis_IDMappingAssociations <- df_Diagnosis %>%
                                              ungroup() %>%
                                              select(PatientID, OriginalDiagnosisID, DiagnosisID, SubDiagnosisID) %>%
                                              rename(all_of(c(OldDiagnosisID = "OriginalDiagnosisID",
                                                              NewDiagnosisID = "DiagnosisID"))) %>%
                                              distinct()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update related tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     - Replace DiagnosisIDs to associate entries (and add SubDiagnosisIDs in same move)
#     - Rearrange column order
#-------------------------------------------------------------------------------

if (nrow(df_GeneralCondition) > 0)
{
    df_GeneralCondition <- df_GeneralCondition %>%
                                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = GeneralConditionID)
}

df_Metastasis <- df_Metastasis %>%
                      ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                      relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = MetastasisID)

if (nrow(df_MolecularDiagnostics) > 0)
{
    df_MolecularDiagnostics <- df_MolecularDiagnostics %>%
                                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = MolecularDiagnosticsID)
}

if (nrow(df_OtherClassification) > 0)
{
    df_OtherClassification <- df_OtherClassification %>%
                                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = OtherClassificationID)
}

df_Progress <- df_Progress %>%
                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = ProgressID)

df_RadiationTherapy <- df_RadiationTherapy %>%
                            ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                            relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = RadiationTherapyID)

df_Staging <- df_Staging %>%
                  ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                  relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = StagingID)

df_Surgery <- df_Surgery %>%
                  ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                  relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = SurgeryID)

df_SystemicTherapy <- df_SystemicTherapy %>%
                          ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                          relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = SystemicTherapyID)

if (nrow(df_TherapyRecommendation ) > 0)
{
    df_TherapyRecommendation <- df_TherapyRecommendation %>%
                                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = TherapyRecommendationID)
}



# Module 4 D) Reconstruct df_Histology from df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove OriginalDiagnosisID column (not needed anymore)
df_Diagnosis <-  df_Diagnosis %>%
                      select(-OriginalDiagnosisID)

# Reconstruct df_Histology
df_Histology <- df_Diagnosis %>%
                    rename(Date = "Date_Histology") %>%
                    select(all_of(c("SubDiagnosisID", names(df_Histology)))) %>%
                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = HistologyID)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re-pack data frames into list to get compact Curated Data Set (CDS) object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Conversion from tibble to data.frame (if necessary), because dataSHIELD can handle data.frames better
#-------------------------------------------------------------------------------
ls_CuratedDataSet <- list(BioSampling = as.data.frame(df_BioSampling),
                          Diagnosis = as.data.frame(df_Diagnosis),
                          GeneralCondition = as.data.frame(df_GeneralCondition),
                          Histology = as.data.frame(df_Histology),
                          Metastasis = as.data.frame(df_Metastasis),
                          MolecularDiagnostics = as.data.frame(df_MolecularDiagnostics),
                          OtherClassification = as.data.frame(df_OtherClassification),
                          Patient = as.data.frame(df_Patient),
                          Progress = as.data.frame(df_Progress),
                          RadiationTherapy = as.data.frame(df_RadiationTherapy),
                          Staging = as.data.frame(df_Staging),
                          Surgery = as.data.frame(df_Surgery),
                          SystemicTherapy = as.data.frame(df_SystemicTherapy),
                          TherapyRecommendation = as.data.frame(df_TherapyRecommendation))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define content of CurationReport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_CurationReport <- list(UnlinkedEntries = CountUnlinkedEntries,      # Named vector
                          Transformation = list(Monitors = ls_TransformationMonitors,      # List of lists
                                                EligibilityOverviews = ls_EligibilityOverviews,
                                                ValueSetOverviews = ls_ValueSetOverviews),
                          DiagnosisClassification = c(DiagnosisRedundancies = CountDiagnosisRedundancies,      # Named vector
                                                      PatientsWithDiagnosisRedundancies = CountPatientsWithDiagnosisRedundancies,
                                                      DiagnosisAssociations = CountDiagnosisAssociations,
                                                      PatientsWithDiagnosisAssociations = CountPatientsWithDiagnosisAssociations))

Messages$CheckCurationCompletion <- "green"
Messages$FinalMessage <- "Curation performed successfully!"

},

# In case of occurring warning:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
warning = function(w)
          {
              Messages$CheckCurationCompletion <- "yellow"
              Messages$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
          },

# In case of occurring error:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
error = function(e)
        {
            Messages$CheckCurationCompletion <- "red"
            Messages$FinalMessage <- paste0("An error occured: \n", e)
        },

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RETURN STATEMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
finally =
{
  # Return the Curated Data Set (CDS) a Curation Report (defined above) and Messages
  return(list(CuratedDataSet = ls_CuratedDataSet,
              CurationReport = ls_CurationReport,
              CurationMessages = Messages))
})

}

