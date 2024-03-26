
#' CurateDataDS
#'
#' Takes Raw Data Set (RDS) and transforms it into Curated Data Set (CDS) while tracing data transformation.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param RuleSet_RawDataTransformation.S Data frame
#' @param RuleProfile_RawDataTransformation.S String | Profile name defining rule set to be used for data transformation. Profile name must be stated in \code{\link{RuleSet_RawDataTransformation.S}. | Default: 'Default'
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
                         RuleSet_RawDataTransformation.S = dsCCPhos::RuleSet_RawDataTransformation,
                         RuleProfile_RawDataTransformation.S = "Default",
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
#   MODULE 1)  Transformation of feature names
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

# Initiate Messaging objects
Messages <- list()
Messages$Completed <- FALSE


# For testing purposes
RuleSet_RawDataTransformation.S <- dsCCPhos::RuleSet_RawDataTransformation
RuleProfile_RawDataTransformation.S <- "Default"
RuleSet_DiagnosisRedundancy.S <- dsCCPhos::RuleSet_DiagnosisRedundancy
RuleProfile_DiagnosisRedundancy.S <- "Default"
RuleSet_DiagnosisAssociation.S <- dsCCPhos::RuleSet_DiagnosisAssociation
RuleProfile_DiagnosisAssociation.S <- "Default"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 1)  Transformation of table and feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setting up 'ls_DataSet' as object that holds all data throughout the function. It will be un- and repacked a couple of times during processing.
ls_DataSet <- RawDataSet


# Rename tables (Remove the 'RDS_'-prefix)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(ls_DataSet) <- sapply(names(ls_DataSet),
                            function(TableName) { str_remove(TableName, "RDS_") })


# Rename features
#~~~~~~~~~~~~~~~~

# Looping through tables to rename features
ls_DataSet <- purrr::map(.x = names(ls_DataSet),
                         .f = function(TableName)
                              {
                                  # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                                  vc_Lookup <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Raw
                                  names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == TableName)$FeatureName_Curated

                                  # Rename feature names according to look-up vector
                                  dplyr::rename(ls_DataSet[[TableName]], any_of(vc_Lookup))      # Returns a tibble
                              }) %>%
                  setNames(names(ls_DataSet))      # List member names are preserved using setNames()



# Unpack list into data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_BioSampling <- ls_DataSet$BioSampling
df_Diagnosis <- ls_DataSet$Diagnosis
df_Histology <- ls_DataSet$Histology
df_Metastasis <- ls_DataSet$Metastasis
df_MolecularDiagnostics <- ls_DataSet$MolecularDiagnostics
df_Patient <- ls_DataSet$Patient
df_Progress <- ls_DataSet$Progress
df_RadiationTherapy <- ls_DataSet$RadiationTherapy
df_Staging <- ls_DataSet$Staging
df_Surgery <- ls_DataSet$Surgery
df_SystemicTherapy <- ls_DataSet$SystemicTherapy




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 2)  Primary table cleaning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Remove entries that are not linked to related tables
#   - Remove duplicate entries
#-------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count unlinked entries (Part 1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CountEntriesBeforeCleaning <- c(BioSampling = nrow(df_BioSampling),
                                Diagnosis = nrow(df_Diagnosis),
                                Histology = nrow(df_Histology),
                                Metastasis = nrow(df_Metastasis),
                                MolecularDiagnostics = nrow(df_MolecularDiagnostics),
                                Patient = nrow(df_Patient),
                                Progress = nrow(df_Progress),
                                RadiationTherapy = nrow(df_RadiationTherapy),
                                Staging = nrow(df_Staging),
                                Surgery = nrow(df_Surgery),
                                SystemicTherapy = nrow(df_SystemicTherapy))


# Set up progress bar
CountProgressItems <- 12
ProgressBar <- progress_bar$new(format = "Cleaning table entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width = 100)
try(ProgressBar$tick())



#--- Patient -------------------------------------------------------------------

# Get vector of PatientIDs that are linked with one or more DiagnosisIDs
vc_EligiblePatientIDs <- df_Patient %>%
                              left_join(df_Diagnosis, by = join_by(PatientID)) %>%
                              filter(!is.na(DiagnosisID)) %>%
                              pull(PatientID)

# Filter out patient entries with no related diagnosis data and keep only distinct rows (removal of duplicates)
df_Patient <- df_Patient %>%
                      filter(PatientID %in% vc_EligiblePatientIDs) %>%
                      distinct()

try(ProgressBar$tick())


#--- Diagnosis -----------------------------------------------------------------

# Get vector of DiagnosisIDs that are linked with a PatientID
vc_EligibleDiagnosisIDs <- df_Diagnosis %>%
                                    left_join(df_Patient, by = join_by(PatientID)) %>%
                                    filter(!is.na(PatientID)) %>%
                                    pull(DiagnosisID)

# Filter out diagnosis entries with no related patient data and keep only distinct rows (removal of duplicates)
df_Diagnosis <- df_Diagnosis %>%
                        filter(DiagnosisID %in% vc_EligibleDiagnosisIDs) %>%
                        distinct(across(-DiagnosisID), .keep_all = TRUE)      # Keep only rows that are distinct (everywhere but DiagnosisID)

try(ProgressBar$tick())


#--- Histology -----------------------------------------------------------------

# Get vector of HistologyIDs that are linked with a PatientID / DiagnosisID
vc_EligibleHistologyIDs <- df_Histology %>%
                                left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                filter(!is.na(DiagnosisID)) %>%
                                pull(HistologyID)

# Filter out histology entries with no related diagnosis data and keep only distinct rows (removal of duplicates)
df_Histology <- df_Histology %>%
                        filter(HistologyID %in% vc_EligibleHistologyIDs) %>%
                        distinct(across(-HistologyID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- BioSampling ---------------------------------------------------------------

# Get vector of SampleIDs that are linked with a PatientID
vc_EligibleSampleIDs <- df_BioSampling %>%
                                  left_join(df_Patient, by = join_by(PatientID)) %>%
                                  filter(!is.na(PatientID)) %>%
                                  pull(SampleID)

# Filter out...
df_BioSampling <- df_BioSampling %>%
                          filter(SampleID %in% vc_EligibleSampleIDs) %>%
                          distinct()

try(ProgressBar$tick())


#--- Metastasis ----------------------------------------------------------------

# Get vector of MetastasisIDs that are linked with a PatientID / DiagnosisID
vc_EligibleMetastasisIDs <- df_Metastasis %>%
                                left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                filter(!is.na(DiagnosisID)) %>%
                                pull(MetastasisID)

# Filter out...
df_Metastasis <- df_Metastasis %>%
                          filter(MetastasisID %in% vc_EligibleMetastasisIDs) %>%
                          distinct(across(-MetastasisID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- MolecularDiagnostics ------------------------------------------------------

# Get vector of MolecularDiagnosticsIDs that are linked with a PatientID / DiagnosisID
vc_EligibleMolecularDiagnosticsIDs <- df_MolecularDiagnostics %>%
                                          left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                          filter(!is.na(DiagnosisID)) %>%
                                          pull(MolecularDiagnosticsID)

# Filter out...
df_MolecularDiagnostics <- df_MolecularDiagnostics %>%
                                    filter(MolecularDiagnosticsID %in% vc_EligibleMolecularDiagnosticsIDs) %>%
                                    distinct(across(-MolecularDiagnosticsID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- Progress ------------------------------------------------------------------

# Get vector of ProgressIDs that are linked with a PatientID / DiagnosisID
vc_EligibleProgressIDs <- df_Progress %>%
                              left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                              filter(!is.na(DiagnosisID)) %>%
                              pull(ProgressID)

# Filter out...
df_Progress <- df_Progress %>%
                        filter(ProgressID %in% vc_EligibleProgressIDs) %>%
                        distinct(across(-ProgressID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- RadiationTherapy ----------------------------------------------------------

# Get vector of RadiationTherapyIDs that are linked with a PatientID / DiagnosisID
vc_EligibleRadiationTherapyIDs <- df_RadiationTherapy %>%
                                      left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                      filter(!is.na(DiagnosisID)) %>%
                                      pull(RadiationTherapyID)

# Filter out...
df_RadiationTherapy <- df_RadiationTherapy %>%
                                filter(RadiationTherapyID %in% vc_EligibleRadiationTherapyIDs) %>%
                                distinct(across(-RadiationTherapyID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- Staging -------------------------------------------------------------------

# Get vector of StagingIDs that are linked with a PatientID / DiagnosisID
vc_EligibleStagingIDs <- df_Staging %>%
                              left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                              filter(!is.na(DiagnosisID)) %>%
                              pull(StagingID)

# Filter out...
df_Staging <- df_Staging %>%
                      filter(StagingID %in% vc_EligibleStagingIDs) %>%
                      distinct(across(-StagingID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- Surgery -------------------------------------------------------------------

# Get vector of SurgeryIDs that are linked with a PatientID / DiagnosisID
vc_EligibleSurgeryIDs <- df_Surgery %>%
                              left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                              filter(!is.na(DiagnosisID)) %>%
                              pull(SurgeryID)

# Filter out...
df_Surgery <- df_Surgery %>%
                      filter(SurgeryID %in% vc_EligibleSurgeryIDs) %>%
                      distinct(across(-SurgeryID), .keep_all = TRUE)

try(ProgressBar$tick())


#--- Systemic Therapy ----------------------------------------------------------

# Get vector of SystemicTherapyIDs that are linked with a PatientID / DiagnosisID
vc_EligibleSystemicTherapyIDs <- df_SystemicTherapy %>%
                                      left_join(df_Diagnosis, by = join_by(PatientID, DiagnosisID)) %>%
                                      filter(!is.na(DiagnosisID)) %>%
                                      pull(SystemicTherapyID)

# Filter out...
df_SystemicTherapy <- df_SystemicTherapy %>%
                              filter(SystemicTherapyID %in% vc_EligibleSystemicTherapyIDs) %>%
                              distinct(across(-SystemicTherapyID), .keep_all = TRUE)

try(ProgressBar$tick())
try(ProgressBar$terminate())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count unlinked entries (Part 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CountEntriesAfterCleaning <- c(BioSampling = nrow(df_BioSampling),
                               Diagnosis = nrow(df_Diagnosis),
                               Histology = nrow(df_Histology),
                               Metastasis = nrow(df_Metastasis),
                               MolecularDiagnostics = nrow(df_MolecularDiagnostics),
                               Patient = nrow(df_Patient),
                               Progress = nrow(df_Progress),
                               RadiationTherapy = nrow(df_RadiationTherapy),
                               Staging = nrow(df_Staging),
                               Surgery = nrow(df_Surgery),
                               SystemicTherapy = nrow(df_SystemicTherapy))

CountUnlinkedEntries <- CountEntriesBeforeCleaning - CountEntriesAfterCleaning

# Print messages for live monitoring in local tests
cat("\n")
for (i in 1:length(CountUnlinkedEntries))
{
    Message <- paste0("Removed ", CountUnlinkedEntries[i], " unlinked entries from '", names(CountUnlinkedEntries)[i], "' table.")
    cli::cat_bullet(Message, bullet = "info")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE 3)  Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A) Definition of features to monitor during Transformation
#   B) Tracking of raw feature values
#   C) Value transformation
#        - Harmonization and correction
#        - Recoding
#        - Formatting
#   D) Tracking of transformed feature values
#   E) Finalize transformation of data
#        - Removing of ineligible values
#        - Optional conversion to factor
#   F) Tracking of finalized feature values
#   G) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------


# Set up progress bar
CountProgressItems <- 29
ProgressBar <- progress_bar$new(format = "Harmonizing data [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
try(ProgressBar$tick())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 A)  Definition of features to monitor during Transformation
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
# Names must be in same order as in ls_DataSet
ls_MonitorFeatures_All <- list(BioSampling = ls_MonitorFeatures_BioSampling,
                               Diagnosis = ls_MonitorFeatures_Diagnosis,
                               Histology = ls_MonitorFeatures_Histology,
                               Metastasis = ls_MonitorFeatures_Metastasis,
                               MolecularDiagnostics = ls_MonitorFeatures_MolecularDiagnostics,
                               Patient = ls_MonitorFeatures_Patient,
                               Progress = ls_MonitorFeatures_Progress,
                               RadiationTherapy = ls_MonitorFeatures_RadiationTherapy,
                               Staging = ls_MonitorFeatures_Staging,
                               Surgery = ls_MonitorFeatures_Surgery,
                               SystemicTherapy = ls_MonitorFeatures_SystemicTherapy)

try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 B)  Track feature values of raw data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Get unique raw values and their frequencies for monitoring
#   - Copy values of monitored features


# Re-pack data frames into list in order to pass them to map-function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Names have to be exactly the same (and the same order) as in ls_MonitorFeatures_all
#-------------------------------------------------------------------------------
ls_DataSet <- list(BioSampling = df_BioSampling,
                   Diagnosis = df_Diagnosis,
                   Histology = df_Histology,
                   Metastasis = df_Metastasis,
                   MolecularDiagnostics = df_MolecularDiagnostics,
                   Patient = df_Patient,
                   Progress = df_Progress,
                   RadiationTherapy = df_RadiationTherapy,
                   Staging = df_Staging,
                   Surgery = df_Surgery,
                   SystemicTherapy = df_SystemicTherapy)
                   #--- Update PB ---
                   try(ProgressBar$tick())


# Check if object names in ls_DataSet and ls_MonitorFeatures_All are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorFeatures_All)))
{
    # Initiate list of data frames containing transformation tracks
    # First step: Store copied raw values of monitored features
    ls_TransformationTracks <- purrr::map2(.x = ls_DataSet,
                                           .y = ls_MonitorFeatures_All,
                                           .f = function(DataFrame, MonitorFeatures)
                                                {
                                                    if (purrr::is_empty(MonitorFeatures) == FALSE)
                                                    {
                                                         DataFrame %>%
                                                              select(names(MonitorFeatures)) %>%
                                                              rename_with(.fn = ~ str_c(., "__Raw"),   # Two underscores for later use in pivot_longer()
                                                                          .cols = everything()) %>%
                                                              mutate(TrackID = row_number(), .before = 1)   # Create TrackID to enable correct mapping and further processing
                                                    }
                                                    else { return(data.frame()) }
                                                })

    ls_ValueCounts_Raw <- purrr::map2(.x = ls_DataSet,
                                      .y = ls_MonitorFeatures_All,
                                      .f = function(DataFrame, MonitorFeatures)
                                           {
                                               DataFrame %>%
                                                   dsCCPhos::TrackValueCounts(Features = MonitorFeatures,
                                                                              TransformationStage = "Raw")
                                           })

} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorFeatures_All must be identical and in the same order.")
}

try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 C)  Execution of value transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Order of operations in each raw data frame:
#     1) Harmonizing and correctional transformation of data values using dsCCPhos::TransformData()
#           - General rules using regular expressions and functions defined in rule set
#           - Direct value transformation using hash tables defined in rule set
#     2) Recoding data using dsCCPhos::RecodeData()
#           - dsCCPhos::RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#     3) Data formatting instructions
#-------------------------------------------------------------------------------


# Transform df_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_BioSampling <- df_BioSampling %>%
                      mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                      #--- Transformation --------------------------------------
                      dsCCPhos::TransformData(TableName = "BioSampling", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                      #--- Recoding --------------------------------------------
                      mutate(SampleType = dsCCPhos::RecodeData(SampleType, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleType"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                                                set_names(Value_Curated, Value_Raw))),      # This returns a vector of the form c("Value_Raw1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                             SampleAliquot = dsCCPhos::RecodeData(SampleAliquot, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "BioSampling" & Feature == "SampleAliquot"),
                                                                                      set_names(Value_Curated, Value_Raw)))) %>%
                      #--- Formatting ------------------------------------------
                      mutate(SampleTakingDate = format(as_datetime(SampleTakingDate), format = "%Y-%m-%d"))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Transform df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Diagnosis <- df_Diagnosis %>%
                    mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                    #--- Transformation ------------------------------------
                    dsCCPhos::TransformData(TableName = "Diagnosis", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                    #--- Recoding ------------------------------------------
                    mutate(LocalizationSide = dsCCPhos::RecodeData(LocalizationSide, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Diagnosis" & Feature == "LocalizationSide"),
                                                                                          set_names(Value_Curated, Value_Raw)))) %>%
                    #--- Formatting ----------------------------------------
                    mutate(InitialDiagnosisDate = format(as_datetime(InitialDiagnosisDate), format = "%Y-%m-%d"),
                           ICD10Version = as.integer(str_extract(ICD10Version, "\\d+")))      # Extract ICD-10 catalogue version year from string
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Transform df_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Histology <- df_Histology %>%
                    mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                    #--- Transformation ------------------------------------
                    dsCCPhos::TransformData(TableName = "Histology", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                    #--- Recoding ------------------------------------------
                    mutate(Grading = dsCCPhos::RecodeData(Grading, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Histology" & Feature == "Grading"),
                                                                        set_names(Value_Curated, Value_Raw)))) %>%
                    #--- Formatting ----------------------------------------
                    mutate(HistologyDate = format(as_datetime(HistologyDate), format = "%Y-%m-%d"))
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Transform df_Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Metastasis <- df_Metastasis %>%
                      mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                      #--- Transformation ----------------------------------
                      dsCCPhos::TransformData(TableName = "Metastasis", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                      #--- Formatting --------------------------------------
                      mutate(MetastasisDiagnosisDate = format(as_datetime(MetastasisDiagnosisDate), format = "%Y-%m-%d"),
                             HasMetastasis = as.logical(HasMetastasis))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Transform df_MolecularDiagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_MolecularDiagnostics <- df_MolecularDiagnostics %>%
                                mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                                #--- Transformation ------------------------
                                dsCCPhos::TransformData(TableName = "MolecularDiagnostics", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                                #--- Formatting ----------------------------
                                mutate(MolecularDiagnosticsDate = format(as_datetime(MolecularDiagnosticsDate), format = "%Y-%m-%d"))
                                #--- Update PB ---
                                try(ProgressBar$tick())


# Transform df_Patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Patient <- df_Patient %>%
                  mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                  #--- Transformation ------------------------------------
                  dsCCPhos::TransformData(TableName = "Patient", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                  #--- Recoding ------------------------------------------
                  mutate(Gender = dsCCPhos::RecodeData(Gender, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "Gender"),
                                                                    set_names(Value_Curated, Value_Raw))),
                         LastVitalStatus = dsCCPhos::RecodeData(LastVitalStatus, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Patient" & Feature == "LastVitalStatus"),
                                                                                      set_names(Value_Curated, Value_Raw)))) %>%
                  #--- Formatting ----------------------------------------
                  mutate(LastVitalStatusDate = format(as_datetime(LastVitalStatusDate), format = "%Y-%m-%d"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Transform df_Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Progress <- df_Progress %>%
                    mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                    #--- Transformation ------------------------------------
                    dsCCPhos::TransformData(TableName = "Progress", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
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
                    mutate(ProgressReportDate = format(as_datetime(ProgressReportDate), format = "%Y-%m-%d"),
                           LocalRelapseDate = format(as_datetime(LocalRelapseDate), format = "%Y-%m-%d"))
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Transform df_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_RadiationTherapy <- df_RadiationTherapy %>%
                            mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                            #--- Transformation ----------------------------
                            dsCCPhos::TransformData(TableName = "RadiationTherapy", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                            #--- Recoding ----------------------------------
                            mutate(RadiationTherapyRelationToSurgery = dsCCPhos::RecodeData(RadiationTherapyRelationToSurgery, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyRelationToSurgery"),
                                                                                                                                    set_names(Value_Curated, Value_Raw))),
                                   RadiationTherapyIntention = dsCCPhos::RecodeData(RadiationTherapyIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "RadiationTherapy" & Feature == "RadiationTherapyIntention"),
                                                                                                                    set_names(Value_Curated, Value_Raw)))) %>%
                            #--- Formatting --------------------------------
                            mutate(RadiationTherapyStart = format(as_datetime(RadiationTherapyStart), format = "%Y-%m-%d"),
                                   RadiationTherapyEnd = format(as_datetime(RadiationTherapyEnd), format = "%Y-%m-%d"))
                            #--- Update PB ---
                            try(ProgressBar$tick())


# Transform df_Staging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Staging <- df_Staging %>%
                  mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                  #--- Transformation --------------------------------------
                  dsCCPhos::TransformData(TableName = "Staging", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
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
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Transform df_Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Surgery <- df_Surgery %>%
                  mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                  #--- Transformation --------------------------------------
                  dsCCPhos::TransformData(TableName = "Surgery", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
                  #--- Recoding --------------------------------------------
                  mutate(SurgeryIntention = dsCCPhos::RecodeData(SurgeryIntention, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "SurgeryIntention"),
                                                                                        set_names(Value_Curated, Value_Raw))),
                         ResidualAssessmentLocal = dsCCPhos::RecodeData(ResidualAssessmentLocal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentLocal"),
                                                                                                      set_names(Value_Curated, Value_Raw))),
                         ResidualAssessmentTotal = dsCCPhos::RecodeData(ResidualAssessmentTotal, with(dplyr::filter(dsCCPhos::Meta_ValueSets, Table == "Surgery" & Feature == "ResidualAssessmentTotal"),
                                                                                                      set_names(Value_Curated, Value_Raw)))) %>%
                  #--- Formatting ------------------------------------------
                  mutate(SurgeryDate = format(as_datetime(SurgeryDate), format = "%Y-%m-%d"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Transform df_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SystemicTherapy <- df_SystemicTherapy %>%
                          mutate(TrackID = row_number()) %>%   # Enables tracking of transformation
                          #--- Transformation ------------------------------
                          dsCCPhos::TransformData(TableName = "SystemicTherapy", RuleSet = RuleSet_RawDataTransformation.S, RuleProfile = RuleProfile_RawDataTransformation.S) %>%
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
                          #--- Update PB ---
                          try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 D)  Track feature values after Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Re-pack data frames into list in order to pass them to map-function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Names have to be exactly the same (and the same order) as in ls_MonitorFeatures_all
#-------------------------------------------------------------------------------
ls_DataSet <- list(BioSampling = df_BioSampling,
                   Diagnosis = df_Diagnosis,
                   Histology = df_Histology,
                   Metastasis = df_Metastasis,
                   MolecularDiagnostics = df_MolecularDiagnostics,
                   Patient = df_Patient,
                   Progress = df_Progress,
                   RadiationTherapy = df_RadiationTherapy,
                   Staging = df_Staging,
                   Surgery = df_Surgery,
                   SystemicTherapy = df_SystemicTherapy)
                   #--- Update PB ---
                   try(ProgressBar$tick())


# Map raw values to their transformed state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, ls_DataSet and ls_MonitorFeatures_All are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(ls_DataSet)) & all(names(ls_DataSet) == names(ls_MonitorFeatures_All)))
{
    ls_TransformationTracks <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                     ls_DataSet,
                                                     ls_MonitorFeatures_All),
                                           .f = function(TransformationTracks, TransformedDataFrame, MonitorFeatures)
                                                {
                                                     if (purrr::is_empty(MonitorFeatures) == FALSE)
                                                     {
                                                         TransformedValues <- TransformedDataFrame %>%
                                                                                   select(c("TrackID", names(MonitorFeatures))) %>%
                                                                                   rename_with(.fn = ~ str_c(., "__Transformed"),   # Two underscores for later use in pivot_longer()
                                                                                               .cols = all_of(names(MonitorFeatures)))

                                                         TransformationTracks %>%
                                                             left_join(TransformedValues,
                                                                       by = join_by(TrackID)) %>%
                                                             distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                                     }
                                                     else { return(data.frame()) }
                                                })
} else {
    stop("Internal error: Object names in ls_TransformationTracks, ls_DataSet and ls_MonitorFeatures_All must be identical and in the same order.")
}


# Get counts of all distinct values in transformed data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_DataSet and ls_MonitorFeatures_All are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorFeatures_All)))
{
    ls_ValueCounts_Transformed <- map2(.x = ls_DataSet,
                                       .y = ls_MonitorFeatures_All,
                                       .f = function(DataFrame, MonitorFeatures)
                                            {
                                                DataFrame %>%
                                                   dsCCPhos::TrackValueCounts(Features = MonitorFeatures,
                                                                              TransformationStage = "Transformed")
                                            })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorFeatures_All must be identical and in the same order.")
}


try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 E)  Finalize transformation of data values using dsCCPhos::FinalizeDataTransformation()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - (Optional) Exclusion of ineligible data (including data that could not be transformed)
#   - (Optional) Conversion to ordered factor
#   - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#   - All predefined information stored in dsCCPhos::Meta_ValueSets
#-------------------------------------------------------------------------------

# Finalize df_BioSampling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_BioSampling <- df_BioSampling %>%
                      mutate(SampleType = dsCCPhos::FinalizeDataTransformation(SampleType, TableName = "BioSampling", FeatureName = "SampleType"),
                             SampleAliquot = dsCCPhos::FinalizeDataTransformation(SampleAliquot, TableName = "BioSampling", FeatureName = "SampleAliquot"))
                      #--- Update PB ---
                      try(ProgressBar$tick())


# Finalize df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Diagnosis <- df_Diagnosis %>%
                    mutate(LocalizationSide = dsCCPhos::FinalizeDataTransformation(LocalizationSide, TableName = "Diagnosis", FeatureName = "LocalizationSide"))   # Assign factor labels?
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Finalize df_Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Histology <- df_Histology %>%
                    mutate(Grading = dsCCPhos::FinalizeDataTransformation(Grading, TableName = "Histology", FeatureName = "Grading"))   # Assign factor labels?
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
                           LocalStatus = dsCCPhos::FinalizeDataTransformation(LocalStatus, TableName = "Progress", FeatureName = "LocalStatus"),   # Assign factor labels?
                           LymphnodalStatus = dsCCPhos::FinalizeDataTransformation(LymphnodalStatus, TableName = "Progress", FeatureName = "LymphnodalStatus"),   # Assign factor labels?
                           MetastasisStatus = dsCCPhos::FinalizeDataTransformation(MetastasisStatus, TableName = "Progress", FeatureName = "MetastasisStatus"))   # Assign factor labels?
                    #--- Update PB ---
                    try(ProgressBar$tick())


# Finalize df_RadiationTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_RadiationTherapy <- df_RadiationTherapy %>%
                            mutate(RadiationTherapyRelationToSurgery = dsCCPhos::FinalizeDataTransformation(RadiationTherapyRelationToSurgery, TableName = "RadiationTherapy", FeatureName = "RadiationTherapyRelationToSurgery"),   # Assign factor labels?
                                   RadiationTherapyIntention = dsCCPhos::FinalizeDataTransformation(RadiationTherapyIntention, TableName = "RadiationTherapy", FeatureName = "RadiationTherapyIntention"))   # Assign factor labels?
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
                  mutate(SurgeryIntention = dsCCPhos::FinalizeDataTransformation(SurgeryIntention, TableName = "Surgery", FeatureName = "SurgeryIntention"),   # Assign factor labels?
                         ResidualAssessmentLocal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentLocal, TableName = "Surgery", FeatureName = "ResidualAssessmentLocal"),
                         ResidualAssessmentTotal = dsCCPhos::FinalizeDataTransformation(ResidualAssessmentTotal, TableName = "Surgery", FeatureName = "ResidualAssessmentTotal"))
                  #--- Update PB ---
                  try(ProgressBar$tick())


# Finalize df_SystemicTherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SystemicTherapy <- df_SystemicTherapy %>%
                          mutate(SystemicTherapyRelationToSurgery = dsCCPhos::FinalizeDataTransformation(SystemicTherapyRelationToSurgery, TableName = "SystemicTherapy", FeatureName = "SystemicTherapyRelationToSurgery"),   # Assign factor labels?
                                 SystemicTherapyIntention = dsCCPhos::FinalizeDataTransformation(SystemicTherapyIntention, TableName = "SystemicTherapy", FeatureName = "SystemicTherapyIntention"))   # Assign factor labels?
                          #--- Update PB ---
                          try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 F)  Track Feature Values after Finalized Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Re-pack data frames into list in order to pass them to map-function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Names have to be exactly the same (and the same order) as in ls_MonitorFeatures_all
#-------------------------------------------------------------------------------
ls_CuratedDataSet <- list(BioSampling = df_BioSampling,
                          Diagnosis = df_Diagnosis,
                          Histology = df_Histology,
                          Metastasis = df_Metastasis,
                          MolecularDiagnostics = df_MolecularDiagnostics,
                          Patient = df_Patient,
                          Progress = df_Progress,
                          RadiationTherapy = df_RadiationTherapy,
                          Staging = df_Staging,
                          Surgery = df_Surgery,
                          SystemicTherapy = df_SystemicTherapy)
                          #--- Update PB ---
                          try(ProgressBar$tick())


# Map raw values to their finalized state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, ls_DataSet and ls_MonitorFeatures_All are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(ls_DataSet)) & all(names(ls_DataSet) == names(ls_MonitorFeatures_All)))
{
    ls_TransformationTracks <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                     ls_DataSet,
                                                     ls_MonitorFeatures_All),
                                           .f = function(TransformationTrack, FinalizedDataFrame, MonitorFeatures)
                                                {
                                                     if (purrr::is_empty(MonitorFeatures) == FALSE)
                                                     {
                                                         FinalizedValues <- FinalizedDataFrame %>%
                                                                                 select(c("TrackID", names(MonitorFeatures))) %>%
                                                                                 rename_with(.fn = ~ str_c(., "__Final"),   # Two underscores for later use in pivot_longer()
                                                                                             .cols = all_of(names(MonitorFeatures)))

                                                         TransformationTrack %>%
                                                             left_join(FinalizedValues,
                                                                       by = join_by(TrackID)) %>%
                                                             distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                                     }
                                                     else { return(data.frame()) }
                                                })
} else {
    stop("Internal error: Object names in ls_TransformationTracks, ls_DataSet and ls_MonitorFeatures_All must be identical and in the same order.")
}


# Get counts of all distinct values in finalized data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_DataSet and ls_MonitorFeatures_All are identical to avoid incorrect mapping
if (all(names(ls_DataSet) == names(ls_MonitorFeatures_All)))
{
    ls_ValueCounts_Final <- map2(.x = ls_DataSet,
                                 .y = ls_MonitorFeatures_All,
                                 .f = function(DataFrame, MonitorFeatures)
                                      {
                                          DataFrame %>%
                                              dsCCPhos::TrackValueCounts(Features = MonitorFeatures,
                                                                         TransformationStage = "Final")
                                      })
} else {
    stop("Internal error: Object names in ls_DataSet and ls_MonitorFeatures_All must be identical and in the same order.")
}


try(ProgressBar$tick())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module 3 G)  Merge monitor objects into coherent summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ls_TransformationTracks_Summaries <- purrr::pmap(.l = list(ls_TransformationTracks,
                                                         ls_MonitorFeatures_All),
                                                 .f = function(TrackData,
                                                               FeatureInfo)
                                                      {
                                                           if (purrr::is_empty(TrackData) == FALSE)
                                                           {
                                                               TrackData %>%
                                                                   mutate(across(everything(), ~ as.character(.x))) %>%      # Turn all columns into character (necessary for pivot_longer())
                                                                   tidyr::pivot_longer(cols = c(everything(), -TrackID),
                                                                                       names_to = c("Feature", "Stage"),
                                                                                       names_sep = "(__)",      # Separate by two underscores
                                                                                       values_to = "Value") %>%
                                                                   distinct(pick(c(everything(), -TrackID)), .keep_all = TRUE) %>%
                                                                   tidyr::pivot_wider(names_from = Stage,
                                                                                      values_from = Value) %>%
                                                                   select(-TrackID) %>%
                                                                   rename(Value = Raw,
                                                                          Value_Transformed = Transformed,
                                                                          Value_Final = Final) %>%
                                                                   rowwise() %>%
                                                                   mutate(IsEligible_Raw = Value %in% FeatureInfo[[Feature]],
                                                                          IsEligible_Transformed = Value_Transformed %in% FeatureInfo[[Feature]]) %>%
                                                                   ungroup() %>%
                                                                   arrange(Feature)
                                                           }
                                                           else { return (data.frame()) }
                                                      })


ls_ValueCounts_Summaries <- purrr::pmap(.l = list(ls_ValueCounts_Raw,
                                                  ls_ValueCounts_Transformed,
                                                  ls_ValueCounts_Final),
                                        .f = function(ValueCounts_Raw,
                                                      ValueCounts_Transformed,
                                                      ValueCounts_Final)
                                             {
                                                  if (nrow(ValueCounts_Raw) > 0)
                                                  {
                                                      ValueCounts_Raw %>%
                                                          dplyr::full_join(ValueCounts_Transformed, by = join_by(Feature, Value, IsValueEligible, TransformationStage, Frequency)) %>%
                                                          dplyr::full_join(ValueCounts_Final, by = join_by(Feature, Value, IsValueEligible, TransformationStage, Frequency)) %>%
                                                          tidyr::pivot_wider(names_from = TransformationStage,
                                                                             values_from = Frequency) %>%
                                                          select(-IsValueEligible) %>%
                                                          group_by(Feature, Value) %>%
                                                              summarize(Count_Raw = sum(Raw, na.rm = TRUE),
                                                                        Count_Transformed = sum(Transformed, na.rm = TRUE),
                                                                        Count_Final = sum(Final, na.rm = TRUE)) %>%
                                                              arrange(Feature)
                                                  }
                                                  else { return(data.frame()) }
                                             })


ls_TransformationSummaries <- purrr::pmap(.l = list(ls_TransformationTracks_Summaries,
                                                    ls_ValueCounts_Summaries),
                                          .f = function(TransformationTracksSummary,
                                                        ValueCountsSummary)
                                               {
                                                    if (nrow(TransformationTracksSummary))
                                                    {
                                                         TransformationTracksSummary %>%
                                                              full_join(ValueCountsSummary, by = c("Feature", "Value"))
                                                    }
                                                    else { return(NULL) }
                                               })


# Delete artificial "TrackID"-column from data frames (not needed anymore)
ls_DataSet <- purrr::modify(.x = ls_DataSet,
                            .f = function(DataFrame)
                                 {
                                      if ("TrackID" %in% names(DataFrame))
                                      {
                                          DataFrame %>%
                                              select(-TrackID)
                                      }
                                      else { return(DataFrame) }
                            })




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
                    left_join(df_Histology, by = join_by(PatientID, DiagnosisID)) %>%
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
                                                    RuleSet = RuleSet_DiagnosisRedundancy.S,
                                                    RuleProfile = RuleProfile_DiagnosisRedundancy.S,
                                                    ValueIfNoRuleMet = FALSE)
# Make list of rule calls to pass them to function
RuleCalls_DiagnosisRedundancy <- list(IsLikelyRedundant = Call_IsLikelyRedundant)


# Set up progress bar
CountProgressItems <- df_Diagnosis %>% filter(PatientCountInitialEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying redundant diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)


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
CountProgressItems <- df_Diagnosis %>% filter(PatientCountDistinctEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying associated diagnosis entries [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)

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
df_Metastasis <- df_Metastasis %>%
                      ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                      relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = MetastasisID)

if (nrow(df_MolecularDiagnostics) > 0)
{
    df_MolecularDiagnostics <- df_MolecularDiagnostics %>%
                                    ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = MolecularDiagnosticsID)
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



# Module 4 D) Reconstruct df_Histology from df_Diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove OriginalDiagnosisID column (not needed anymore)
df_Diagnosis <-  df_Diagnosis %>%
                      select(-OriginalDiagnosisID)

# Reconstruct df_Histology
df_Histology <- df_Diagnosis %>%
                    select(all_of(c("SubDiagnosisID", names(df_Histology)))) %>%
                    relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = HistologyID)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re-pack data frames into list to get compact Curated Data Set (CDS) object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Conversion from tibble to data.frame (if necessary), because dataSHIELD can handle data.frames better
#-------------------------------------------------------------------------------
ls_CuratedDataSet <- list(BioSampling = as.data.frame(df_BioSampling),
                          Diagnosis = as.data.frame(df_Diagnosis),
                          Histology = as.data.frame(df_Histology),
                          Metastasis = as.data.frame(df_Metastasis),
                          MolecularDiagnostics = as.data.frame(df_MolecularDiagnostics),
                          Patient = as.data.frame(df_Patient),
                          Progress = as.data.frame(df_Progress),
                          RadiationTherapy = as.data.frame(df_RadiationTherapy),
                          Staging = as.data.frame(df_Staging),
                          Surgery = as.data.frame(df_Surgery),
                          SystemicTherapy = as.data.frame(df_SystemicTherapy))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define content of CurationReport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_CurationReport <- list(UnlinkedEntries = CountUnlinkedEntries,      # Named vector
                          Transformation = ls_TransformationSummaries,      # List
                          DiagnosisClassification = c(DiagnosisRedundancies = CountDiagnosisRedundancies,      # Named vector
                                                      PatientsWithDiagnosisRedundancies = CountPatientsWithDiagnosisRedundancies,
                                                      DiagnosisAssociations = CountDiagnosisAssociations,
                                                      PatientsWithDiagnosisAssociations = CountPatientsWithDiagnosisAssociations))

Messages$Completed <- TRUE


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RETURN STATEMENT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Return the Curated Data Set (CDS) a Curation Report (defined above) and Messages
return(list(CuratedDataSet = ls_CuratedDataSet,
            CurationReport = ls_CurationReport,
            CurationMessages = Messages))
}

