
#' CurateDataDS
#'
#' Takes Raw Data Set (RDS) and transforms it into Curated Data Set (CDS) while tracing data transformation.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param Settings.S \code{list} - Settings passed to function
#'                   \itemize{\item DataHarmonization_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DataHarmonization}
#'                            \item DataHarmonization_Profile \code{character} - Profile name defining rule set to be used for data harmonization. Profile name must be stated in \code{RawDataHarmonization_RuleSet} - Default: 'Default'
#'                            \item DiagnosisRedundancy_Check \code{logical} - Whether or not to check for redundant diagnosis entries
#'                            \item DiagnosisRedundancy_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DiagnosisRedundancy}
#'                            \item DiagnosisRedundancy_Profile \code{character} - Profile name defining rule set to be used for classification of diagnosis redundancies. Profile name must be stated in \code{DiagnosisRedundancy_RuleSet} - Default: 'Default'
#'                            \item DiagnosisAssociation_Check \code{logical} - Whether or not to classify associated diagnosis entries
#'                            \item DiagnosisAssociation_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DiagnosisAssociation}
#'                            \item DiagnosisAssociation_Profile \code{character} - Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{DiagnosisAssociation_RuleSet} - Default: 'Default'
#'                            \item FeatureObligations_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureObligations}
#'                            \item FeatureObligations_Profile \code{character} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations_RuleSet} - Default: 'Default'
#'                            \item FeatureTracking_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureTracking}
#'                            \item FeatureTracking_Profile \code{character} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking_RuleSet} - Default: 'Default'}
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item CuratedDataSet \code{list}
#'                      \itemize{\item BioSampling
#'                               \item Diagnosis
#'                               \item GeneralCondition
#'                               \item Histology
#'                               \item Metastasis
#'                               \item MolecularDiagnostics
#'                               \item OtherClassification
#'                               \item Patient
#'                               \item Progress
#'                               \item RadiationTherapy
#'                               \item Staging
#'                               \item Surgery
#'                               \item SystemicTherapy
#'                               \item TherapyRecommendation}
#'                  \item CurationReport \code{list}
#'                      \itemize{\item EntryCounts \code{tibble}
#'                                    \itemize{\item Table
#'                                             \item InitialCount
#'                                             \item ExcludedPrimary
#'                                             \item AfterPrimaryExclusion
#'                                             \item ExcludedSecondary
#'                                             \item AfterSecondaryExclusion}
#'                               \item Transformation (list of lists)
#'                                    \itemize{\item Monitors
#'                                             \item EligibilityOverviews
#'                                             \item ValueSetOverviews}
#'                               \item DiagnosisClassification
#'                                    \itemize{\item DiagnosisRedundancies
#'                                             \item PatientsWithDiagnosisRedundancies
#'                                             \item DiagnosisAssociations
#'                                             \item PatientsWithDiagnosisAssociations}}
#'                  \item CurationMessages \code{list}}
#' @export
#' @author Bastian Reiter
CurateDataDS <- function(RawDataSetName.S = "RawDataSet",
                         Settings.S = list(DataHarmonization_RuleSet = dsCCPhos::Meta_DataHarmonization,
                                           DataHarmonization_Profile = "Default",
                                           DiagnosisRedundancy_Check = TRUE,
                                           DiagnosisRedundancy_RuleSet = dsCCPhos::Meta_DiagnosisRedundancy,
                                           DiagnosisRedundancy_Profile = "Default",
                                           DiagnosisAssociation_Check = TRUE,
                                           DiagnosisAssociation_RuleSet = dsCCPhos::Meta_DiagnosisAssociation,
                                           DiagnosisAssociation_Profile = "Default",
                                           FeatureObligations_RuleSet = dsCCPhos::Meta_FeatureObligations,
                                           FeatureObligations_Profile = "Default",
                                           FeatureTracking_RuleSet = dsCCPhos::Meta_FeatureTracking,
                                           FeatureTracking_Profile = "Default"))
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OVERVIEW
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   SETUP
#     - Evaluation and parsing of input
#     - Loading of required package namespaces
#
#   MODULE A)  Harmonization of Raw Data Set meta data and structure
#     1) Transform table names
#     2) Add empty tables in data set if they are missing in raw data
#     3) Rename features
#     4) In tables with missing features, add empty features accordingly
#
#   MODULE B)  Primary entry exclusion
#     1) Remove entries that are not linked to related tables
#     2) Remove duplicate entries
#     3) Remove entries in RDS missing obligatory features (defined in meta data or passed as optional argument)
#
#   MODULE C)  Data Harmonization / Transformation
#     1) Transform feature names
#     2) Definition of features to monitor during value transformation (defined in meta data or passed as an optional argument)
#     3) Value transforming operations
#     4) Compilation of curation report
#
#   MODULE D)  Processing of diagnosis data
#     1) Joining of Tables 'Diagnosis' and 'Histology'
#     2) Classification and removal of redundant diagnosis entries
#     3) Classification and bundling of associated diagnosis entries
#     4) Update DiagnosisIDs in other tables
#
#   MODULE E)  Secondary entry exclusion
#     1) Remove entries in CDS missing obligatory features (defined in meta data or passed as optional argument)
#
#   Return list containing
#     - Curated Data Set (list)
#     - Curation Report (list)
#     - Curation Messages (list)



### For testing purposes
# Settings.S <- list(DataHarmonization_RuleSet = dsCCPhos::Meta_DataHarmonization,
#                    DataHarmonization_Profile = "Default",
#                    DiagnosisRedundancy_Check = FALSE,
#                    DiagnosisRedundancy_RuleSet = dsCCPhos::Meta_DiagnosisRedundancy,
#                    DiagnosisRedundancy_Profile = "Default",
#                    DiagnosisAssociation_Check = FALSE,
#                    DiagnosisAssociation_RuleSet = dsCCPhos::Meta_DiagnosisAssociation,
#                    DiagnosisAssociation_Profile = "Default",
#                    FeatureObligations_RuleSet = dsCCPhos::Meta_FeatureObligations,
#                    FeatureObligations_Profile = "Default",
#                    FeatureTracking_RuleSet = dsCCPhos::Meta_FeatureTracking,
#                    FeatureTracking_Profile = "Default")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(dsCCPhos)
require(lubridate)
require(progress)
require(purrr)
require(rlang)
require(stats)
require(stringr)
require(tidyr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Equip 'Settings.S' with default values in case of missing arguments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename 'Settings.S' argument for better code readability
Settings <- Settings.S

# If list of 'Settings' passed to function is incomplete, complete it with default values
if (is.null(Settings$DataHarmonization_RuleSet)) { Settings$DataHarmonization_RuleSet <- dsCCPhos::Meta_DataHarmonization }
if (is.null(Settings$DataHarmonization_Profile)) { Settings$DataHarmonization_Profile <- "Default" }
if (is.null(Settings$DiagnosisRedundancy_Check)) { Settings$DiagnosisRedundancy_Check <- TRUE }
if (is.null(Settings$DiagnosisRedundancy_RuleSet)) { Settings$DiagnosisRedundancy_RuleSet <- dsCCPhos::Meta_DiagnosisRedundancy }
if (is.null(Settings$DiagnosisRedundancy_Profile)) { Settings$DiagnosisRedundancy_Profile <- "Default" }
if (is.null(Settings$DiagnosisAssociation_Check)) { Settings$DiagnosisAssociation_Check <- TRUE }
if (is.null(Settings$DiagnosisAssociation_RuleSet)) { Settings$DiagnosisAssociation_RuleSet <- dsCCPhos::Meta_DiagnosisAssociation }
if (is.null(Settings$DiagnosisAssociation_Profile)) { Settings$DiagnosisAssociation_Profile <- "Default" }
if (is.null(Settings$FeatureObligations_RuleSet)) { Settings$FeatureObligations_RuleSet <- dsCCPhos::Meta_FeatureObligations }
if (is.null(Settings$FeatureObligations_Profile)) { Settings$FeatureObligations_Profile <- "Default" }
if (is.null(Settings$FeatureTracking_RuleSet)) { Settings$FeatureTracking_RuleSet <- dsCCPhos::Meta_FeatureTracking }
if (is.null(Settings$FeatureTracking_Profile)) { Settings$FeatureTracking_Profile <- "Default" }



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(RawDataSetName.S))
{
    RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())

} else {

    ClientMessage <- "ERROR: 'RawDataSetName.S' must be specified as a character string."
    stop(ClientMessage, call. = FALSE)
}

if (Settings$FeatureObligations_Profile %in% names(Settings$FeatureObligations_RuleSet) == FALSE)
{
    ClientMessage <- "ERROR: Value of settings argument 'FeatureObligations_Profile' must be column name of data.frame passed in settings argument 'FeatureObligations_RuleSet'."
    stop(ClientMessage, call. = FALSE)
}

if (Settings$FeatureTracking_Profile %in% names(Settings$FeatureTracking_RuleSet) == FALSE)
{
    ClientMessage <- "ERROR: Value of settings argument 'FeatureTracking_Profile' must be column name of data.frame passed in settings argument 'FeatureTracking_RuleSet'."
    stop(ClientMessage, call. = FALSE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial statements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Print starting message
cat("\n")
Message <- paste0("Starting Data Curation...")
cli::cat_bullet(Message, bullet = "star")
cat("\n")

# Suppress summarize info messages
options(dplyr.summarise.inform = FALSE)

# Initiate Messaging objects
Messages <- list()
Messages$ExcludedEntries_Primary <- character()
Messages$ExcludedEntries_Secondary <- character()
Messages$DiagnosisRedundancies <- character()
Messages$DiagnosisAssociation <- character()
Messages$CheckCurationCompletion <- "red"
Messages$FinalMessage <- "Curation not completed"


# For semantic reasons, set up 'DataSet' as object that holds all data throughout the function
DataSet <- RawDataSet


# Use tryCatch to catch warnings and errors
# Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
#tryCatch({


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE A)  Harmonization of Raw Data Set meta data and structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Transform table names
#   - Add empty tables in data set if they are missing in raw data
#   - Rename features
#   - In tables with missing features, add empty features accordingly
#-------------------------------------------------------------------------------


# Rename tables (Remove the 'RDS_'-prefix)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(DataSet) <- sapply(names(DataSet),
                         function(TableName) { str_remove(TableName, "RDS_") })


# If tables are missing, create corresponding empty tables for easier management throughout following processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AllTableNames <- dsCCPhos::Meta_Tables$TableName_Curated
MissingTableNames <- AllTableNames[!(AllTableNames %in% names(DataSet))]

# Create empty data frames for missing tables
if (length(MissingTableNames) > 0)
{
    for (i in 1:length(MissingTableNames))
    {
        DataSet[[MissingTableNames[i]]] <- data.frame()
    }
}

# Reestablish original order of tables in DataSet
DataSet <- DataSet[AllTableNames]


# Rename features
#~~~~~~~~~~~~~~~~

# Looping through tables to rename features
DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                        vc_Lookup <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Raw
                        names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Curated

                        if (!is_empty(Table))
                        {
                            # Rename feature names according to look-up vector
                            dplyr::rename(Table, any_of(vc_Lookup))      # Returns a tibble
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

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        # Determine missing features
                        RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Curated
                        PresentFeatureNames <- names(Table)
                        MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

                        # If a table misses features, add empty columns accordingly
                        if (length(MissingFeatures) > 0)
                        {
                            ComplementedDataFrame <- Table %>%
                                                        mutate(!!!set_names(rep(list(NA_character_), length(MissingFeatures)), MissingFeatures))

                            return(ComplementedDataFrame)

                        } else {

                          return(Table)
                        }
                     })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE B)  Primary entry exclusion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   1) Remove entries that are not linked to related tables
#   2) Remove entries in RDS with missing strictly obligatory features (defined in meta data / passed through Settings)
#   3) Remove duplicate entries
#   4) Remove entries that are not consistent with special trans-feature obligation rules (defined in meta data / passed through Settings)
#-------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count table entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count entries in initial data frames
CountEntries_Initial <- DataSet %>%
                            map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))


# By merging 'Patient' and 'Diagnosis', create auxiliary data frame containing all eligible combinations of PatientIDs and DiagnosisIDs
# Do NOT simply delete (pseudo-)duplicate entries (because different DiagnosisIDs of same patient and diagnosis can e.g. be related to different Histologies).
# Filter out any entry that has missings in features marked as obligatory in meta data (thereby also removing 'rogue'/unlinked patient or diagnosis entries because this way every patient needs to have at least one related diagnosis and vice versa)
DataSetRoot <- DataSet$Patient %>%
                    left_join(DataSet$Diagnosis, by = join_by(PatientID)) %>%
                    CleanTable(TableNameLookup = c("Diagnosis", "Patient"),
                               RemoveRedundantEntries = FALSE,
                               FeatureObligations_RuleSet = Settings$FeatureObligations_RuleSet,
                               FeatureObligations_Profile = Settings$FeatureObligations_Profile) %>%
                    select(PatientID, DiagnosisID) %>%
                    distinct()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Loop through whole DataSet to only keep entries that belong to eligible 'root'
# - Remove entries that have missing values in strictly obligatory features (defined in meta data / passed as an argument)
# - Remove duplicate entries
# - Remove entries that are not consistent with special trans-feature obligation rules (defined in meta data / passed as an argument)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
ProgressBar <- progress_bar$new(format = "Primary exclusion: Excluding ineligible table entries... [:bar] :percent in :elapsed  :spin",
                                total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        try(ProgressBar$tick())

                        if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                        {
                            # Join current table with preselection of 'DataSetRoot'
                            if (tablename %in% c("BioSampling", "GeneralCondition", "Patient", "TherapyRecommendation"))
                            {
                                Table <- DataSetRoot %>%
                                            select(PatientID) %>%
                                            distinct() %>%
                                            left_join(Table, by = join_by(PatientID))
                            } else {

                                Table <- DataSetRoot %>%
                                              left_join(Table, by = join_by(PatientID, DiagnosisID))
                            }

                            # Clean current table using auxiliary function dsCCPhos::CleanTable()
                            CleanedTable <- Table %>%
                                                CleanTable(TableNameLookup = tablename,
                                                           RemoveRedundantEntries = TRUE,
                                                           FeatureObligations_RuleSet = Settings$FeatureObligations_RuleSet,
                                                           FeatureObligations_Profile = Settings$FeatureObligations_Profile)
                            return(CleanedTable)

                        } else {

                            return(Table)
                        }
                     })

try(ProgressBar$terminate())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count ineligible entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count entries in data frames after primary exclusion
CountEntries_AfterPrimaryExclusion <- DataSet %>%
                                          map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

# Count excluded entries
CountExcludedEntries_Primary <- CountEntries_Initial - CountEntries_AfterPrimaryExclusion


# Print messages for live monitoring in local tests
for (i in 1:length(CountExcludedEntries_Primary))
{
    Message <- paste0("Primary exclusion: Removed ", CountExcludedEntries_Primary[i], " ineligible entries from '", names(CountExcludedEntries_Primary)[i], "' table.")
    cli::cat_bullet(Message, bullet = "info")

    # Save messages in output object
    Messages$ExcludedEntries_Primary <- c(Messages$ExcludedEntries_Primary,
                                          info = Message)
}
cat("\n")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE C)  Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   1) Definition of features to monitor during Transformation
#   2) Tracking of raw feature values
#   3) Data harmonization (correctional transformation)
#   4) Tracking of harmonized feature values
#   5) Data recoding and formatting
#   6) Tracking of recoded / formatted feature values
#   7) Finalize transformation of data
#        - Removing of ineligible values
#        - Optional conversion to factor
#   8) Tracking of finalized feature values
#   9) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 1)  Definition of tracked features and their sets of eligible values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     - Create meta data on eligible value sets of features to be tracked / monitored during curation process
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data / passed rule set
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

ls_MonitorMetaData <- names(DataSet) %>%
                          map(function(tablename)
                              {
                                  vc_FeaturesToTrack <- Settings$FeatureTracking_RuleSet %>%
                                                            rename(IsTracked = all_of(Settings$FeatureTracking_Profile)) %>%      # Renaming feature based on passed argument
                                                            filter(Table == tablename,
                                                                   IsTracked == TRUE) %>%
                                                            pull(Feature)

                                  ls_EligibleValues <- vc_FeaturesToTrack %>%
                                                            map(function(featurename)
                                                                {
                                                                    Values <- dsCCPhos::Meta_ValueSets %>%
                                                                                  filter(Table == tablename,
                                                                                         Feature == featurename) %>%
                                                                                  select(Value_Raw,
                                                                                         Value_Curated)

                                                                    if (nrow(Values) == 0) { return(NULL) } else { return(Values) }
                                                                }) %>%
                                                            set_names(vc_FeaturesToTrack)
                              }) %>%
                          set_names(names(DataSet))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 2)  Track feature values of raw data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Get unique raw values and their frequencies for monitoring
#   - Copy values of monitored features and mark them with TrackID that has correspondent in actually processed data frames
#-------------------------------------------------------------------------------

# Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(DataSet) == names(ls_MonitorMetaData)))
{
    # Initiate list of data frames containing transformation tracks
    # First step: Store copied raw values of monitored features and mark them with "TrackID" to track them along transformation process
    ls_TransformationTracks <- map2(.x = DataSet,
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

    ls_ValueCounts_Raw <- map2(.x = DataSet,
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
    stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 3)  Data harmonization (correctional transformation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - General rules using regular expressions and functions defined in rule set
#   - Direct value replacement using hash tables defined in rule set
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
ProgressBar <- progress_bar$new(format = "Harmonizing data... [:bar] :percent in :elapsed  :spin",
                                total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                         try(ProgressBar$tick())

                         Table %>%
                              mutate(TrackID = row_number()) %>%      # Enables tracking of transformation (see above)
                              HarmonizeData(TableName = tablename,
                                            RuleSet = Settings$DataHarmonization_RuleSet,
                                            Profile = Settings$DataHarmonization_Profile)
                     })

try(ProgressBar$terminate())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 4)  Track feature values after Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map raw values to their harmonized state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                              DataSet,
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
    stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in harmonized data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Harmonized <- map2(.x = DataSet,
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
    stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 5)  Data recoding and formatting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Recoding data using dsCCPhos::RecodeData() based on specifications in dsCCPhos::Meta_ValueSets
#   - RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#   - Format / Re-type data using dsCCPhos::FormatData() based on specifications in dsCCPhos::Meta_Features
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
ProgressBar <- progress_bar$new(format = "Recoding and formatting data... [:bar] :percent in :elapsed  :spin",
                                total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        try(ProgressBar$tick())

                        if (!(nrow(Table) == 0))
                        {
                            # Recode table data as defined in meta data
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                            FeaturesWithValueSets <- dsCCPhos::Meta_ValueSets %>%
                                                          filter(Table == tablename) %>%
                                                          pull(Feature) %>%
                                                          unique()

                            # ValueSets <- dsCCPhos::Meta_ValueSets %>%
                            #                   filter(Table == tablename)
                            #
                            # Dictionaries <- ValueSets %>%
                            #                     split(ValueSets$Feature) %>%
                            #                     map(\(Values) with(Values, set_names(Value_Curated, Value_Raw)))

                            if (length(FeaturesWithValueSets) > 0)
                            {
                                for (i in 1:length(FeaturesWithValueSets))
                                {
                                    Dictionary <- with(dsCCPhos::Meta_ValueSets %>% filter(Table == tablename, Feature == FeaturesWithValueSets[i]),
                                                       set_names(Value_Curated, Value_Raw))

                                    Table <- Table %>%
                                                  mutate(across(all_of(FeaturesWithValueSets[i]), ~ RecodeData(.x, Dictionary)))
                                }
                            }

                            # Format / Re-type table data as defined in meta data
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                            FeatureTypes <- dsCCPhos::Meta_Features %>%
                                                filter(TableName_Curated == tablename,
                                                       Type != "character") %>%
                                                select(FeatureName_Curated, Type) %>%
                                                rename(Feature = "FeatureName_Curated")

                            if (nrow(FeatureTypes) > 0)
                            {
                                for (i in 1:nrow(FeatureTypes))
                                {
                                    Table <- Table %>%
                                                  mutate(across(all_of(FeatureTypes$Feature[i]), ~ FormatData(.x, FeatureTypes$Type[i])))
                                }
                            }
                        }

                        return(Table)
                     })

try(ProgressBar$terminate())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 6)  Track feature values after Recoding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map raw values to their recoded state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                             DataSet,
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
    stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in recoded data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Recoded <- map2(.x = DataSet,
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
    stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 7)  Finalize transformation of data values using dsCCPhos::FinalizeDataTransformation()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - (Optional / Default) Exclusion of ineligible data (including data that could not be transformed)
#   - (Optional) Conversion to ordered factor
#   - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#   - All predefined information stored in dsCCPhos::Meta_ValueSets
#-------------------------------------------------------------------------------

# Set up progress bar
#-------------------------------------------------------------------------------
ProgressBar <- progress_bar$new(format = "Finalizing data transformation... [:bar] :percent in :elapsed  :spin",
                                total = length(DataSet), clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        try(ProgressBar$tick())

                        if (!(nrow(Table) == 0))
                        {
                            FeaturesWithValueSets <- dsCCPhos::Meta_ValueSets %>%
                                                          filter(Table == tablename) %>%
                                                          pull(Feature) %>%
                                                          unique()

                            if (length(FeaturesWithValueSets) > 0)
                            {
                                for (i in 1:length(FeaturesWithValueSets))
                                {
                                    EligibleValueSet <- dsCCPhos::Meta_ValueSets %>%
                                                            filter(Table == tablename,
                                                                   Feature == FeaturesWithValueSets[i])

                                    Table <- Table %>%
                                                mutate(across(all_of(FeaturesWithValueSets[i]),
                                                              ~ FinalizeDataTransformation(.x, EligibleValueSet)))
                                }
                            }
                        }

                        return(Table)
                     })

try(ProgressBar$terminate())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 8)  Track Feature Values after Finalized Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map raw values to their finalized state to get transformation tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                             DataSet,
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
    stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
}


# Get counts of all distinct values in finalized data sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
if (all(names(DataSet) == names(ls_MonitorMetaData)))
{
    ls_ValueCounts_Final <- map2(.x = DataSet,
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
    stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module C 9)  Merge monitor objects into coherent summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Summarize Transformation Tracks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls_TransformationTracks_Summaries <- pmap(.l = list(ls_TransformationTracks,
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
ls_TransformationMonitors <- pmap(.l = list(ls_TransformationTracks_Summaries,
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
                                                   mutate(Count_Harmonized = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                       TRUE ~ Count_Harmonized),
                                                          Count_Recoded = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                    TRUE ~ Count_Recoded),
                                                          Count_Final = case_when(IsOccurring == FALSE ~ NA_integer_,
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
ls_EligibilityOverviews <- pmap(.l = list(ls_TransformationMonitors,
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
ls_ValueSetOverviews <- ls_TransformationMonitors %>%
                            map(function(MonitorData)
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

                                    } else { return(list()) }
                                })



# Delete artificial "TrackID"-column from data frames (not needed anymore)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DataSet <- DataSet %>%
                map(function(Table)
                    {
                        try(Table %>% select(-TrackID))
                    })


# Print info message
cli::cat_bullet("Data transformation monitors are stored in 'CurationReport$Transformation'", bullet = "info")
cat("\n")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE D)  Process diagnosis data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   1) Joining of tables 'Diagnosis' and 'Histology' to get coherent diagnosis data
#   2) Classification and removal of redundant diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   3) Classification and bundling of associated diagnosis entries
#        - Afterwards update Diagnosis IDs in related tables
#   4) Reconstruct 'Histology' from 'Diagnosis'
#-------------------------------------------------------------------------------


# Module D 1) Join tables 'Diagnosis' and 'Histology'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Create composed ID ('DiagnosisID / HistologyID')
#   - Add auxiliary features for filtering purposes
#-------------------------------------------------------------------------------

DataSet$Diagnosis <- DataSet$Diagnosis %>%
                          left_join(DataSet$Histology, by = join_by(PatientID, DiagnosisID)) %>%
                          mutate(OriginalDiagnosisID = DiagnosisID,
                                 DiagnosisID = paste0(DiagnosisID, "/", HistologyID)) %>%
                          relocate(DiagnosisID, .after = PatientID) %>%
                          group_by(PatientID) %>%
                              mutate(PatientCountInitialEntries = n()) %>%
                          ungroup()



# Module D 2) Classification and removal of redundant diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisRedundancy() to identify and consolidate redundant diagnosis entries
#   - Rules for classification of redundancy are defined in customizable data object delivered with dsCCPhos
#   - Replace redundant DiagnosisIDs in all related tables
#-------------------------------------------------------------------------------

if (Settings$DiagnosisRedundancy_Check == TRUE)
{
#///////////////////////////////////////////////////////////////////////////////

# Compile rule calls from data in Settings$DiagnosisRedundancy_RuleSet using dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pass required information to dsCCPhos::CompileClassificationCall to compile rule calls (dplyr::case_when-Statements)
Call_IsLikelyRedundant <- CompileClassificationCall(TargetFeature = "IsLikelyRedundant",
                                                    RuleSet = Settings$DiagnosisRedundancy_RuleSet,
                                                    RuleProfile = Settings$DiagnosisRedundancy_Profile,
                                                    ValueIfNoRuleMet = FALSE)

# Make list of rule calls to pass them to function
RuleCalls_DiagnosisRedundancy <- list(IsLikelyRedundant = Call_IsLikelyRedundant)


# Set up progress bar
#-------------------------------------------------------------------------------
CountProgressItems <- DataSet$Diagnosis %>% filter(PatientCountInitialEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying redundant diagnosis entries... [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------


# Filter patients with multiple diagnosis entries and apply dsCCPhos::ClassifyDiagnosisRedundancy()
df_Aux_Diagnosis_ClassifiedRedundancies <- DataSet$Diagnosis %>%
                                                filter(PatientCountInitialEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisRedundancy(DiagnosisEntries = .x,
                                                                                               RuleCalls = RuleCalls_DiagnosisRedundancy,
                                                                                               ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble DataSet$Diagnosis after processing of redundant diagnosis entries
DataSet$Diagnosis <- DataSet$Diagnosis %>%
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
CountDiagnosisRedundancies <- sum(DataSet$Diagnosis$CountRedundancies, na.rm = TRUE)
CountPatientsWithDiagnosisRedundancies <- DataSet$Diagnosis %>%
                                              filter(CountRedundancies > 0) %>%
                                              pull(PatientID) %>%
                                              n_distinct()

# Print message for live monitoring in local tests
Message <- paste0("Found ", CountDiagnosisRedundancies, " redundancies related to ", CountPatientsWithDiagnosisRedundancies, " patient IDs.")
cli::cat_bullet(Message, bullet = "info")
cat("\n")

# Save message in output object
Messages$DiagnosisRedundancies <- Message

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update related tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     - After classification of redundant diagnosis entries, replace DiagnosisIDs in related tables
#     - Removal of auxiliary columns
#-------------------------------------------------------------------------------

# Replace IDs (Original DiagnosisID and not newly composed one) of redundant diagnosis entries in related tables
# Get table of affected DiagnosisIDs
df_Aux_Diagnosis_IDMappingRedundancies <- DataSet$Diagnosis %>%
                                              ungroup() %>%
                                              filter(CountRedundancies > 0) %>%
                                              select(PatientID, RedundantOriginalIDs, OriginalDiagnosisID) %>%
                                              unnest(cols = c(RedundantOriginalIDs)) %>%
                                              rename(all_of(c(OldDiagnosisID = "RedundantOriginalIDs",
                                                              NewDiagnosisID = "OriginalDiagnosisID"))) %>%
                                              filter(OldDiagnosisID != NewDiagnosisID) %>%
                                              distinct()

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        if (nrow(Table) > 0 & tablename %in% c("BioSampling", "Diagnosis", "Histology", "Patient", "TherapyRecommendation") == FALSE)
                        {
                            Table %>%
                                ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingRedundancies)

                        } else { return(Table) }
                     })

# Remove columns of redundant IDs (not needed anymore)
DataSet$Diagnosis <- DataSet$Diagnosis %>%
                          select(-c(RedundantIDs,
                                    RedundantOriginalIDs,
                                    CountRedundancies))

#///////////////////////////////////////////////////////////////////////////////
} else {   # In case no diagnosis redundancy check was performed

    DataSet$Diagnosis <- DataSet$Diagnosis %>%
                              mutate(PatientCountDistinctEntries = PatientCountInitialEntries)

    CountDiagnosisRedundancies <- NA
    CountPatientsWithDiagnosisRedundancies <- NA

    # Print warning message if Diagnosis Redundancy Check was skipped
    Message <- "Diagnosis Redundancy Check was skipped!"
    cli::cat_bullet(Message, bullet = "warning")
    cat("\n")

    # Save message in output object
    Messages$DiagnosisRedundancies <- Message
}



# Module D 3) Classify associations between diagnosis entries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - In patients with multiple distinct diagnosis entries:
#     Use dsCCPhos-function ClassifyDiagnosisAssociations() to plausibly distinguish pseudo-different from actually different diagnoses
#   - Rules for classification of associated diagnosis entries are defined in customizable data object delivered with dsCCPhos
#   - Replace DiagnosisIDs in all related tables with ReferenceDiagnosisID
#-------------------------------------------------------------------------------

if (Settings$DiagnosisAssociation_Check == TRUE)
{
#///////////////////////////////////////////////////////////////////////////////

# Compile rule calls (unevaluated dplyr::case_when-Statements) with dsCCPhos::CompileClassificationCall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Call_IsLikelyAssociated <- CompileClassificationCall(TargetFeature = "IsLikelyAssociated",
                                                     RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                     RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                     ValueIfNoRuleMet = FALSE)

Call_InconsistencyCheck <- CompileClassificationCall(TargetFeature = "InconsistencyCheck",
                                                     RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                     RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                     ValueIfNoRuleMet = "No apparent inconsistency")

Call_ImplausibilityCheck <- CompileClassificationCall(TargetFeature = "ImplausibilityCheck",
                                                      RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                      RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                      ValueIfNoRuleMet = "No apparent implausibility")

Call_Relation_ICD10 <- CompileClassificationCall(TargetFeature = "Relation_ICD10",
                                                 RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                 RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                 ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOTopography <- CompileClassificationCall(TargetFeature = "Relation_ICDOTopography",
                                                          RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                          RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                          ValueIfNoRuleMet = NA_character_)

Call_Relation_LocalizationSide <- CompileClassificationCall(TargetFeature = "Relation_LocalizationSide",
                                                            RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                            RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_ICDOMorphology <- CompileClassificationCall(TargetFeature = "Relation_ICDOMorphology",
                                                            RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                            RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                            ValueIfNoRuleMet = NA_character_)

Call_Relation_Grading <- CompileClassificationCall(TargetFeature = "Relation_Grading",
                                                   RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                   RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                   ValueIfNoRuleMet = NA_character_)

Call_IsLikelyProgression <- CompileClassificationCall(TargetFeature = "IsLikelyProgression",
                                                      RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                      RuleProfile = Settings$DiagnosisAssociation_Profile,
                                                      ValueIfNoRuleMet = NA)

Call_IsLikelyRecoding <- CompileClassificationCall(TargetFeature = "IsLikelyRecoding",
                                                   RuleSet = Settings$DiagnosisAssociation_RuleSet,
                                                   RuleProfile = Settings$DiagnosisAssociation_Profile,
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
CountProgressItems <- DataSet$Diagnosis %>% filter(PatientCountDistinctEntries > 1) %>% pull(PatientID) %>% n_distinct()
ProgressBar <- progress_bar$new(format = "Classifying associated diagnosis entries... [:bar] :percent in :elapsed  :spin",
                                total = CountProgressItems, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

# Filter patients with multiple distinct diagnosis entries and apply dsCCPhos::ClassifyDiagnosisAssociations()
df_Aux_Diagnosis_ClassifiedAssociations <- DataSet$Diagnosis %>%
                                                filter(PatientCountDistinctEntries > 1) %>%
                                                group_by(PatientID) %>%
                                                    group_modify(~ ClassifyDiagnosisAssociation(DiagnosisEntries = .x,
                                                                                                RuleCalls = RuleCalls_DiagnosisAssociation,
                                                                                                ProgressBarObject = ProgressBar)) %>%
                                                ungroup()

# Reassemble DataSet$Diagnosis after processing of associated diagnosis entries
DataSet$Diagnosis <- DataSet$Diagnosis %>%
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
CountDiagnosisAssociations <- sum(DataSet$Diagnosis$IsLikelyAssociated, na.rm = TRUE)
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
    DataSet$Diagnosis <- DataSet$Diagnosis %>%
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
df_Aux_Diagnosis_IDMappingAssociations <- DataSet$Diagnosis %>%
                                              ungroup() %>%
                                              select(PatientID, OriginalDiagnosisID, DiagnosisID) %>%
                                              rename(all_of(c(OldDiagnosisID = "OriginalDiagnosisID",
                                                              NewDiagnosisID = "DiagnosisID"))) %>%
                                              distinct()

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        if (nrow(Table) > 0 & tablename %in% c("BioSampling", "Diagnosis", "Histology", "Patient", "TherapyRecommendation") == FALSE)
                        {
                            Table %>%
                                ReplaceDiagnosisIDs(IDMapping = df_Aux_Diagnosis_IDMappingAssociations) %>%
                                relocate(c(PatientID, DiagnosisID))   # Moves features 'PatientID' and 'DiagnosisID' to front of table

                        } else { return(Table) }
                     })



# Module D 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Reconstruct DataSet$Histology from DataSet$Diagnosis
#-------------------------------------------------------------------------------

# Reconstruction of DataSet$Histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (Settings$DiagnosisAssociation_Check == TRUE)
{
    # Reconstruct DataSet$Histology
    DataSet$Histology <- DataSet$Diagnosis %>%
                              select(all_of(c("SubDiagnosisID", names(DataSet$Histology)))) %>%
                              relocate(c(PatientID, DiagnosisID, SubDiagnosisID), .before = HistologyID)
} else {

    DataSet$Histology <- DataSet$Diagnosis %>%
                              select(all_of(names(DataSet$Histology))) %>%
                              mutate(SubDiagnosisID = DiagnosisID, .after = DiagnosisID)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODULE E)  Secondary entry exclusion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Remove ineligible entries that may have been introduced during processing
#   - Same proceedings as in primary table cleaning (Module B))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
ProgressBar <- progress_bar$new(format = "Secondary exclusion: Excluding ineligible table entries... [:bar] :percent in :elapsed  :spin",
                                total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

DataSetRoot <- DataSet$Patient %>%
                    left_join(DataSet$Diagnosis, by = join_by(PatientID)) %>%
                    CleanTable(TableNameLookup = c("Diagnosis", "Patient"),
                               RemoveRedundantEntries = FALSE,
                               FeatureObligations_RuleSet = Settings$FeatureObligations_RuleSet,
                               FeatureObligations_Profile = Settings$FeatureObligations_Profile) %>%
                    select(PatientID, DiagnosisID) %>%
                    distinct()

DataSet <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        try(ProgressBar$tick())

                        if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                        {
                            # Join current table with preselection of 'DataSetRoot'
                            if (tablename %in% c("BioSampling", "GeneralCondition", "Patient", "TherapyRecommendation"))
                            {
                                Table <- DataSetRoot %>%
                                            select(PatientID) %>%
                                            distinct() %>%
                                            left_join(Table, by = join_by(PatientID))
                            } else {

                                Table <- DataSetRoot %>%
                                              left_join(Table, by = join_by(PatientID, DiagnosisID))
                            }

                            # Clean current table using auxiliary function dsCCPhos::CleanTable()
                            CleanedTable <- Table %>%
                                                CleanTable(TableNameLookup = tablename,
                                                           RemoveRedundantEntries = TRUE,
                                                           FeatureObligations_RuleSet = Settings$FeatureObligations_RuleSet,
                                                           FeatureObligations_Profile = Settings$FeatureObligations_Profile)
                            return(CleanedTable)

                        } else {

                            return(Table)
                        }
                     })

try(ProgressBar$terminate())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Count ineligible entries after secondary exclusion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count entries in data frames after secondary exclusion
CountEntries_AfterSecondaryExclusion <- DataSet %>%
                                            imap_int(function(Table, tablename)
                                                     {
                                                        if (tablename == "Diagnosis")
                                                        {
                                                            if (Settings$DiagnosisAssociation_Check == FALSE)
                                                            {
                                                                Table <- Table %>% distinct(OriginalDiagnosisID, .keep_all = TRUE)
                                                            }

                                                            Table <- Table %>% filter(IsReferenceEntry == TRUE)
                                                        }

                                                        return(ifelse(!is.null(nrow(Table)), nrow(Table), 0))
                                                     })

# Count excluded entries
CountExcludedEntries_Secondary <- CountEntries_AfterPrimaryExclusion - CountEntries_AfterSecondaryExclusion


# Print messages for live monitoring in local tests
for (i in 1:length(CountExcludedEntries_Secondary))
{
    Message <- paste0("Secondary exclusion: Removed ", CountExcludedEntries_Secondary[i], " ineligible entries from '", names(CountExcludedEntries_Secondary)[i], "' table.")
    cli::cat_bullet(Message, bullet = "info")

    # Save messages in output object
    Messages$ExcludedEntries_Secondary <- c(Messages$ExcludedEntries_Secondary,
                                            info = Message)
}
cat("\n")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Final modifications
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove OriginalDiagnosisID column from 'Diagnosis' (not needed anymore)
DataSet$Diagnosis <-  DataSet$Diagnosis %>%
                          select(-OriginalDiagnosisID)

# Conversion of Tables from tibble to data.frame, because DataSHIELD can handle data.frames better
DataSet <- DataSet %>%
                map(\(Table) as.data.frame(Table))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define content of CurationReport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CurationReport <- list(EntryCounts = tibble(Table = names(DataSet),
                                            InitialCount = CountEntries_Initial,
                                            ExcludedPrimary = CountExcludedEntries_Primary,
                                            AfterPrimaryExclusion = CountEntries_AfterPrimaryExclusion,
                                            ExcludedSecondary = CountExcludedEntries_Secondary,
                                            AfterSecondaryExclusion = CountEntries_AfterSecondaryExclusion),
                       Transformation = list(Monitors = ls_TransformationMonitors,
                                             EligibilityOverviews = ls_EligibilityOverviews,
                                             ValueSetOverviews = ls_ValueSetOverviews),
                       DiagnosisClassification = c(DiagnosisRedundancies = CountDiagnosisRedundancies,
                                                   PatientsWithDiagnosisRedundancies = CountPatientsWithDiagnosisRedundancies,
                                                   DiagnosisAssociations = CountDiagnosisAssociations,
                                                   PatientsWithDiagnosisAssociations = CountPatientsWithDiagnosisAssociations))

Messages$CheckCurationCompletion <- "green"
Messages$FinalMessage <- "Data Curation performed successfully!"

# Print completion message
Message <- paste0("Data Curation performed successfully!")
cli::cat_bullet(Message, bullet = "tick")
cat("\n")


# },
#
# # In case of occurring warning:
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# warning = function(w)
#           {
#               Messages$CheckCurationCompletion <- "yellow"
#               Messages$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
#           },
#
# # In case of occurring error:
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# error = function(e)
#         {
#             Messages$CheckCurationCompletion <- "red"
#             Messages$FinalMessage <- paste0("An error occured: \n", e)
#         },
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # RETURN STATEMENT
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# finally =
# {
#   # Return the Curated Data Set (CDS) a Curation Report (defined above) and Messages
  return(list(CuratedDataSet = DataSet,
              CurationReport = CurationReport,
              CurationMessages = Messages))
# })

}

