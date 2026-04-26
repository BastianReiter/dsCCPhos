
#' CurateDataDS
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Transforms CCP Raw Data Set (RDS) into Curated Data Set (CDS) while tracing data transformation.
#'
#' Server-side ASSIGN method
#'
#' @param \code{RawDataSetName.S} \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param \code{MetaData.S} \code{list} - A \code{list} of \code{data.frames} describing the structural and semantic characteristics of a hierarchical data model.
#'            \itemize{ \item \code{Meta.Tables}
#'                      \item \code{Meta.Features}
#'                      \item \code{Meta.Relationships}
#'                      \item \code{Meta.Links}
#'                      \item \code{Meta.Values}}
#' @param \code{DataHarmonization.Run.S} \code{logical} - Whether or not to perform data harmonization - Default: \code{TRUE}
#' @param \code{DataHarmonization.Process.S} \code{data.frame} - Default: \code{dsCCPhos::Set.DataHarmonization}
#' @param \code{DataHarmonization.Process.Profile.S} \code{string} - Profile used in \emph{Process} - Default: 'Default'
#' @param \code{DataHarmonization.TransformativeExpressions.S} \code{data.frame} - Default: \code{dsCCPhos::Set.TransformativeExpressions}
#' @param \code{DataHarmonization.TransformativeExpressions.Profile.S} \code{string} - Profile used in \emph{TransformativeExpressions} - Default: 'Default'
#' @param \code{DataHarmonization.Dictionary.S} \code{data.frame} - Default: \code{dsCCPhos::Set.Dictionary}
#' @param \code{DataHarmonization.Dictionary.Profile.S} \code{string} - Profile used in \emph{Dictionary} - Default: 'Default'
#' @param \code{DataHarmonization.FuzzyStringMatching.S} \code{data.frame} - Default: \code{dsCCPhos::Set.FuzzyStringMatching}
#' @param \code{DataHarmonization.FuzzyStringMatching.Profile.S} \code{string} - Profile used in \emph{FuzzyStringMatching} - Default: 'Default'
#' @param \code{DataHarmonization.ExludeIneligibleValues.S} \code{logical} - Default: \code{TRUE}
#' @param \code{FeatureObligations.RuleSet.S} \code{data.frame} - Default: \code{dsCCPhos::Set.FeatureObligations}
#' @param \code{FeatureObligations.RuleSet.Profile.S} \code{string} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations$RuleSet} - Default: 'Default'
#' @param \code{FeatureTracking.RuleSet.S} \code{data.frame} - Default: \code{dsCCPhos::Set.FeatureTracking}
#' @param \code{FeatureTracking.RuleSet.Profile.S} \code{string} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking$RuleSet} - Default: 'Default'
#' @param \code{TableCleaning.Run.S} \code{logical} - Whether or not to perform table cleaning (removal of redundant and ineligible entries) - Default: \code{TRUE}
#' @param \code{TableNormalization.Run.S} \code{logical} - Whether or not to perform table normalization - Default: \code{TRUE}
#' @param \code{TableNormalization.RuleSet.S} \code{data.frame} - Default: \code{dsCCPhos::Proc.TableNormalization}
#' @param \code{TableNormalization.RuleSet.Profile.S} \code{string} - Profile name defining rule set to be used for table normalization. Profile name must be stated in \code{TableNormalization$RuleSet} - Default: 'Default'
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item CuratedDataSet \code{list}
#'                      \itemize{ \item BioSampling
#'                                \item Diagnosis
#'                                \item DiseaseStatus
#'                                \item GeneralCondition
#'                                \item Histology
#'                                \item Metastasis
#'                                \item MolecularDiagnostics
#'                                \item OtherClassification
#'                                \item Patient
#'                                \item RadiationTherapy
#'                                \item Staging
#'                                \item Surgery
#'                                \item SystemicTherapy
#'                                \item TherapyRecommendation}
#'                  \item CurationReport \code{list}
#'                      \itemize{ \item EntryCounts \code{tibble}
#'                                    \itemize{ \item Table
#'                                              \item InitialCount
#'                                              \item ExcludedPrimary
#'                                              \item AfterPrimaryExclusion
#'                                              \item ExcludedSecondary
#'                                              \item AfterSecondaryExclusion
#'                                              \item ExcludedSecondaryRedundancy
#'                                              \item AfterSecondaryRedundancyExclusion}
#'                                \item Transformation (list of lists)
#'                                    \itemize{ \item Monitors
#'                                              \item EligibilityOverviews
#'                                              \item ValueSetOverviews}}
#'                  \item Messages \code{list}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurateDataDS <- function(RawDataSetName.S = "RawDataSet",
                         MetaData.S = list(Tables = dsCCPhos::Meta.Tables,
                                           Relationships = dsCCPhos::Meta.Relationships,
                                           Links = dsCCPhos::Meta.Links,
                                           Features = dsCCPhos::Meta.Features,
                                           Values = dsCCPhos::Meta.Values),
                         DataHarmonization.Run.S = TRUE,
                         DataHarmonization.Process.S = dsCCPhos::Set.DataHarmonization,
                         DataHarmonization.Process.Profile.S = "Default",
                         DataHarmonization.TransformativeExpressions.S = dsCCPhos::Set.TransformativeExpressions,
                         DataHarmonization.TransformativeExpressions.Profile.S = "Default",
                         DataHarmonization.Dictionary.S = dsCCPhos::Set.Dictionary,
                         DataHarmonization.Dictionary.Profile.S = "Default",
                         DataHarmonization.FuzzyStringMatching.S = dsCCPhos::Set.FuzzyStringMatching,
                         DataHarmonization.FuzzyStringMatching.Profile.S = "Default",
                         DataHarmonization.ExcludeIneligibleValues.S = TRUE,
                         FeatureObligations.RuleSet.S = dsCCPhos::Set.FeatureObligations,
                         FeatureObligations.RuleSet.Profile.S = "Default",
                         FeatureTracking.RuleSet.S = dsCCPhos::Set.FeatureTracking,
                         FeatureTracking.RuleSet.Profile.S = "Default",
                         TableCleaning.Run.S = TRUE,
                         TableNormalization.Run.S = TRUE,
                         TableNormalization.RuleSet.S = dsCCPhos::Proc.TableNormalization,
                         TableNormalization.RuleSet.Profile.S = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{

#===============================================================================
# - OVERVIEW -
#===============================================================================
#
#   SETUP
#
#   MODULE A)  Harmonization of Raw Data Set meta data and structure
#     1)  Transform table names
#     2)  Add empty tables in data set if they are missing in raw data
#     3)  Rename features
#     4)  In tables with missing features, add empty features accordingly
#
#   MODULE B)  Primary entry exclusion
#     1)  Remove entries that are not linked to related tables
#     2)  Remove duplicate entries
#     3)  Remove entries in RDS missing obligatory features (defined in meta data or passed as optional argument)
#
#   MODULE C)  Table normalization
#     1)  'Split and expand' where necessary (as determined by arguments / meta data)
#
#   MODULE D)  Data Harmonization / Transformation
#     1) Definition of features to monitor during Transformation
#     2) Tracking of raw feature values
#     3) Data Harmonization with
#        3.1) Transformative Expressions
#        3.2) Dictionary
#        3.3) Fuzzy String Matching
#     4) Tracking of harmonized feature values
#     5) Data recoding and formatting
#     6) Tracking of recoded / formatted feature values
#     7) Finalize transformation of data
#         - Removing of ineligible values
#         - Optional conversion to factor
#     8) Tracking of finalized feature values
#     9) Compilation of monitor objects for reporting
#
#   MODULE E)  Secondary entry exclusion
#     1)  Remove duplicate entries
#     2)  Remove entries in CDS missing obligatory features (defined in meta data or passed as optional argument)
#
#   MODULE F)  Finding and removing secondary redundancies
#     1)  Process table 'Diagnosis' first, since other tables hold primary key 'DiagnosisID'.
#         Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables.
#     2)  Proceed with all other tables (excluding 'Patient')
#
#   RETURN list containing
#     - Curated Data Set (list)
#     - Curation Report (list)
#     - Curation Messages (list)
#
#===============================================================================


  # --- For Testing Purposes ---
  # RawDataSetName.S <- "RawDataSet"
  # MetaData.S <- list(Tables = dsCCPhos::Meta.Tables,
  #                    Relationships = dsCCPhos::Meta.Relationships,
  #                    Links = dsCCPhos::Meta.Links,
  #                    Features = dsCCPhos::Meta.Features,
  #                    Values = dsCCPhos::Meta.Values)
  # DataHarmonization.Run.S <- TRUE
  # DataHarmonization.Process.S <- dsCCPhos::Set.DataHarmonization
  # DataHarmonization.Process.Profile.S <- "Default"
  # DataHarmonization.TransformativeExpressions.S <- dsCCPhos::Set.TransformativeExpressions
  # DataHarmonization.TransformativeExpressions.Profile.S <- "Default"
  # DataHarmonization.Dictionary.S <- dsCCPhos::Set.Dictionary
  # DataHarmonization.Dictionary.Profile.S <- "Default"
  # DataHarmonization.FuzzyStringMatching.S <- dsCCPhos::Set.FuzzyStringMatching
  # DataHarmonization.FuzzyStringMatching.Profile.S <- "Default"
  # DataHarmonization.ExcludeIneligibleValues.S <- TRUE
  # FeatureObligations.RuleSet.S <- dsCCPhos::Set.FeatureObligations
  # FeatureObligations.RuleSet.Profile.S <- "Default"
  # FeatureTracking.RuleSet.S <- dsCCPhos::Set.FeatureTracking
  # FeatureTracking.RuleSet.Profile.S <- "Default"
  # TableCleaning.Run.S <- TRUE
  # TableNormalization.Run.S <- TRUE
  # TableNormalization.RuleSet.S <- dsCCPhos::Proc.TableNormalization
  # TableNormalization.RuleSet.Profile.S <- "Default"


  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.list(MetaData),
              is.data.frame(MetaData$Tables),
              is.data.frame(MetaData$Relationships),
              is.data.frame(MetaData$Links),
              is.data.frame(MetaData$Features),
              is.data.frame(MetaData$Values),
              is.flag(DataHarmonization.Run.S),
              is.data.frame(DataHarmonization.Process.S),
              is.string(DataHarmonization.Process.Profile.S),
              is.data.frame(DataHarmonization.TransformativeExpressions.S),
              is.string(DataHarmonization.TransformativeExpressions.Profile.S),
              is.data.frame(DataHarmonization.Dictionary.S),
              is.string(DataHarmonization.Dictionary.Profile.S),
              is.data.frame(DataHarmonization.FuzzyStringMatching.S),
              is.string(DataHarmonization.FuzzyStringMatching.Profile.S),
              is.flag(DataHarmonization.ExcludeIneligibleValues.S),
              is.data.frame(FeatureObligations.RuleSet.S),
              is.string(FeatureObligations.RuleSet.Profile.S),
              is.data.frame(FeatureTracking.RuleSet.S),
              is.string(FeatureTracking.RuleSet.Profile.S),
              is.flag(TableCleaning.Run.S),
              is.flag(TableNormalization.Run.S),
              is.data.frame(TableNormalization.RuleSet.S),
              is.string(TableNormalization.RuleSet.Profile.S))

  # Special validation rules implemented with base::stopifnot() instead of assertthat::assert_that()
  stopifnot("ERROR: Value of argument 'DataHarmonization.Process.Profile.S' must be a column name of the data.frame passed in argument 'DataHarmonization.Process.S'." = (DataHarmonization.Process.Profile.S %in% names(DataHarmonization.Process.S)))
  stopifnot("ERROR: Value of argument 'DataHarmonization.TransformativeExpressions.Profile.S' must be a column name of the data.frame passed in argument 'DataHarmonization.TransformativeExpressions.S'." = (DataHarmonization.TransformativeExpressions.Profile.S %in% names(DataHarmonization.TransformativeExpressions.S)))
  stopifnot("ERROR: Value of argument 'DataHarmonization.Dictionary.Profile.S' must be a column name of the data.frame passed in argument 'DataHarmonization.Dictionary.S'." = (DataHarmonization.Dictionary.Profile.S %in% names(DataHarmonization.Dictionary.S)))
  stopifnot("ERROR: Value of argument 'DataHarmonization.FuzzyStringMatching.Profile.S' must be a column name of the data.frame passed in argument 'DataHarmonization.FuzzyStringMatching.S'." = (DataHarmonization.FuzzyStringMatching.Profile.S %in% names(DataHarmonization.FuzzyStringMatching.S)))
  stopifnot("ERROR: Value of argument 'FeatureObligations.RuleSet.Profile.S' must be a column name of the data.frame passed in argument 'FeatureObligations.RuleSet.S'." = (FeatureObligations.RuleSet.Profile.S %in% names(FeatureObligations.RuleSet.S)))
  stopifnot("ERROR: Value of argument 'FeatureTracking.RuleSet.Profile.S' must be a column name of the data.frame passed in argument 'FeatureTracking.RuleSet.S'." = (FeatureTracking.RuleSet.Profile.S %in% names(FeatureTracking.RuleSet.S)))
  stopifnot("ERROR: Value of argument 'TableNormalization.RuleSet.Profile.S' must be a column name of the data.frame passed in argument 'TableNormalization.RuleSet.S'." = (TableNormalization.RuleSet.Profile.S %in% names(TableNormalization.RuleSet.S)))


#-------------------------------------------------------------------------------
# - Equip 'Settings' with default values in case of missing arguments -
#-------------------------------------------------------------------------------

  # Bundle settings arguments in a list
  Settings <- list(DataHarmonization = list(Run = DataHarmonization.Run.S,
                                            Process = DataHarmonization.Process.S,
                                            Process.Profile = DataHarmonization.Process.Profile.S,
                                            TransformativeExpressions = DataHarmonization.TransformativeExpressions.S,
                                            TransformativeExpressions.Profile = DataHarmonization.TransformativeExpressions.Profile.S,
                                            Dictionary = DataHarmonization.Dictionary.S,
                                            Dictionary.Profile = DataHarmonization.Dictionary.Profile.S,
                                            FuzzyStringMatching = DataHarmonization.FuzzyStringMatching.S,
                                            FuzzyStringMatching.Profile = DataHarmonization.FuzzyStringMatching.Profile.S,
                                            ExcludeIneligibleValues = DataHarmonization.ExcludeIneligibleValues.S),
                   FeatureObligations = list(RuleSet = FeatureObligations.RuleSet.S,
                                             RuleSet.Profile = FeatureObligations.RuleSet.Profile.S),
                   FeatureTracking = list(RuleSet = FeatureTracking.RuleSet.S,
                                          RuleSet.Profile = FeatureTracking.RuleSet.Profile.S),
                   TableCleaning = list(Run = TableCleaning.Run.S),
                   TableNormalization = list(Run = TableNormalization.Run.S,
                                             RuleSet = TableNormalization.RuleSet.S,
                                             RuleSet.Profile = TableNormalization.RuleSet.Profile.S))


  # If list of 'Settings' passed to function is incomplete, complete it with default values
  # if (is.null(Settings$DataHarmonization$Run)) { Settings$DataHarmonization$Run <- TRUE }
  # if (is.null(Settings$DataHarmonization$Process)) { Settings$DataHarmonization$Process <- dsCCPhos::Set.DataHarmonization }
  # if (is.null(Settings$DataHarmonization$Process.Profile)) { Settings$DataHarmonization$Process.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$TransformativeExpressions)) { Settings$DataHarmonization$TransformativeExpressions <- dsCCPhos::Set.TransformativeExpressions }
  # if (is.null(Settings$DataHarmonization$TransformativeExpressions.Profile)) { Settings$DataHarmonization$TransformativeExpressions.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$Dictionary)) { Settings$DataHarmonization$Dictionary <- dsCCPhos::Set.Dictionary }
  # if (is.null(Settings$DataHarmonization$Dictionary.Profile)) { Settings$DataHarmonization$Dictionary.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$FuzzyStringMatching)) { Settings$DataHarmonization$FuzzyStringMatching <- dsCCPhos::Set.FuzzyStringMatching }
  # if (is.null(Settings$DataHarmonization$FuzzyStringMatching.Profile)) { Settings$DataHarmonization$FuzzyStringMatching.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$ExcludeIneligibleValues)) { Settings$DataHarmonization$ExcludeIneligibleValues <- TRUE }
  # if (is.null(Settings$FeatureObligations$RuleSet)) { Settings$FeatureObligations$RuleSet <- dsCCPhos::Set.FeatureObligations }
  # if (is.null(Settings$FeatureObligations$RuleSet.Profile)) { Settings$FeatureObligations$RuleSet.Profile <- "Default" }
  # if (is.null(Settings$FeatureTracking$RuleSet)) { Settings$FeatureTracking$RuleSet <- dsCCPhos::Set.FeatureTracking }
  # if (is.null(Settings$FeatureTracking$RuleSet.Profile)) { Settings$FeatureTracking$RuleSet.Profile <- "Default" }
  # if (is.null(Settings$TableCleaning$Run)) { Settings$TableCleaning$Run <- TRUE }
  # if (is.null(Settings$TableNormalization$Run)) { Settings$TableNormalization$Run <- TRUE }
  # if (is.null(Settings$TableNormalization$RuleSet)) { Settings$TableNormalization$RuleSet <- dsCCPhos::Proc.TableNormalization }
  # if (is.null(Settings$TableNormalization$RuleSet.Profile)) { Settings$TableNormalization$RuleSet.Profile <- "Default" }



#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())


#-------------------------------------------------------------------------------
# - Initial statements -
#-------------------------------------------------------------------------------

  # Print starting message
  cat("\n")
  cli::cat_bullet("Starting Data Curation...", bullet = "star")
  cat("\n")

  # Suppress summarize info messages
  options(dplyr.summarise.inform = FALSE)

  # Initiate Messaging objects
  Messages <- list()
  Messages$AddedFeatures <- character()
  Messages$RemovedFeatures <- character()
  Messages$ExcludedEntries_Primary <- character()
  Messages$ExcludedEntries_Secondary <- character()
  Messages$ExcludedEntries_SecondaryRedundancy <- character()
  Messages$CheckCurationCompletion <- "red"
  Messages$FinalMessage <- "Curation not completed"

#-------------------------------------------------------------------------------


  # Use tryCatch to catch warnings and errors
  # Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
  #tryCatch({


#===============================================================================
# MODULE A)  Processing of data set meta data
#===============================================================================

  # Renaming meta data object for better code readability
  MetaData <- MetaData.S

  # Extracting all table names
  TableNames <- MetaData$Tables$TableName.Curated

  # Assuming a hierarchical data model
  SeedTableName <- MetaData$Tables %>%
                        filter(Role == "Seed") %>%
                        pull(TableName.Curated)

  stopifnot("ERROR in meta data: There must be exactly one table with role 'Seed'!" = (length(SeedTableName) == 1))


  RootTableNames <- MetaData$Tables %>%
                        filter(Role == "Root") %>%
                        pull(TableName.Curated)

  BranchTableNames <- MetaData$Tables %>%
                          filter(Role == "Branch") %>%
                          pull(TableName.Curated)


  RootFeatureNames <- TableNames %>%
                          map(function(tablename)
                              {
                                  MetaData$Links %>%
                                      filter(ChildTableName == tablename) %>%
                                      pull(ChildFeatureName)
                              }) %>%
                          set_names(TableNames)


#===============================================================================
# MODULE A)  Harmonization of Raw Data Set meta data and structure
#===============================================================================
#   - Add empty tables in data set if they are missing in raw data
#   - Recode feature names according to meta data
#   - Add empty features in case of missing feature names
#   - Remove unknown features
#-------------------------------------------------------------------------------


# If tables are missing, create corresponding empty tables for easier management throughout following processing
#-------------------------------------------------------------------------------

  MissingTableNames <- TableNames[!(TableNames %in% names(DataSet))]

  # Create empty data frames for missing tables
  if (length(MissingTableNames) > 0)
  {
      for (tablename in MissingTableNames)
      {
          DataSet[[tablename]] <- data.frame()
      }
  }

  # Reestablish original order of tables in 'DataSet' list
  DataSet <- DataSet[TableNames]


# Rename features from harmonized raw feature names to curated feature names
#-------------------------------------------------------------------------------

  # Looping through tables to rename features
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                          vc_Lookup <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Raw
                          names(vc_Lookup) <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated

                          if (length(Table) > 0)
                          {
                              # Rename feature names according to look-up vector
                              Table %>% rename(any_of(vc_Lookup))      # Returns a tibble

                          } else {

                              # Create empty data.frame with pre-defined column names
                              df <- data.frame(matrix(nrow = 0,
                                                      ncol = length(names(vc_Lookup)))) %>%
                                        setNames(names(vc_Lookup)) %>%
                                        mutate(across(everything(), ~ as.character(.x)))

                              return(df)
                          }
                       })


# Add empty features in case of missing feature names and remove unknown existing features
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Determine missing and unknown features
                          RequiredFeatureNames <- dplyr::filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated
                          PresentFeatureNames <- names(Table)
                          MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]
                          UnknownFeatures <- names(Table)[!(names(Table) %in% RequiredFeatureNames)]

                          # If a table misses features, add empty columns accordingly
                          if (length(MissingFeatures) > 0)
                          {
                              Table <- Table %>%
                                            mutate(!!!set_names(rep(list(NA_character_), length(MissingFeatures)), MissingFeatures))

                              # Print message
                              Message <- paste0("Table '", tablename, "': Added empty vector for missing features ", paste0("'", MissingFeatures, "'", collapse = ", "), ".")
                              cli::cat_bullet(Message, bullet = "info")
                              cat("\n")

                              # Save message for output
                              Messages$AddedFeatures <- c(Messages$AddedFeatures,
                                                          Info = Message)
                          }

                          # The following effectively removes all unknown features
                          Table <- Table %>%
                                        select(all_of(RequiredFeatureNames))

                          # Print and save messages for removed features
                          if (length(UnknownFeatures) > 0)
                          {
                              Message <- paste0("Table '", tablename, "': Removed unknown features ", paste0("'", UnknownFeatures, "'", collapse = ", "), "!")
                              cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                              cat("\n")

                              Messages$RemovedFeatures <- c(Messages$RemovedFeatures,
                                                            Warning = Message)
                          }

                          return(Table)
                       })



#===============================================================================
# MODULE B)  Primary entry exclusion
#===============================================================================
#   1) Remove entries that are not linked to related tables
#   2) Remove entries in RDS with missing strictly obligatory features (determined in meta data / passed through Settings)
#   3) Remove duplicate entries
#   4) Remove entries that are not consistent with special trans-feature obligation rules (defined in meta data / passed through Settings)
#-------------------------------------------------------------------------------

#===============================================================================
# MONITORING: Count table entries
#===============================================================================

  # Count entries in initial data.frames
  CountEntries_Initial <- DataSet %>%
                              map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))


  # By merging 'Patient' and 'Diagnosis', create auxiliary data frame containing all eligible combinations of PatientIDs and DiagnosisIDs
  # Do NOT simply delete (pseudo-)duplicate entries (because different DiagnosisIDs of same patient and diagnosis can e.g. be related to different Histologies).
  # Filter out any entry that has missings in features marked as obligatory in meta data (thereby also removing 'rogue'/unlinked patient or diagnosis entries because this way every patient needs to have at least one related diagnosis and vice versa)

  DataSetRoot <- DataSet$Patient %>%
                      left_join(DataSet$Diagnosis, by = join_by(PatientID)) %>%
                      {   if (Settings$TableCleaning$Run == TRUE)
                          {
                              dsFreda::CleanTable(Table = .,
                                                  TableNameLookup = c("Diagnosis", "Patient"),
                                                  RootFeatures,
                                                  FeatureObligations = Settings$FeatureObligations,
                                                  RemoveEmptyStrings = TRUE,
                                                  RemoveDuplicateEntries = FALSE)
                          } else {.}
                      } %>%
                      select(PatientID, DiagnosisID) %>%
                      distinct()


#===============================================================================
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

                          if (length(Table) > 0 && nrow(Table) > 0)
                          {
                              # Join current table with preselection of 'DataSetRoot'
                              if (all(c("PatientID", "DiagnosisID") %in% names(Table)))
                              {
                                  Table <- DataSetRoot %>%
                                                left_join(Table, by = join_by(PatientID, DiagnosisID))

                              } else {

                                  Table <- DataSetRoot %>%
                                              select(PatientID) %>%
                                              distinct() %>%
                                              left_join(Table, by = join_by(PatientID))
                              }

                              # Clean current table using auxiliary function dsFreda::CleanTable() (Can be optionally omitted)
                              if (Settings$TableCleaning$Run == TRUE)
                              {
                                  Table <- Table %>%
                                                dsFreda::CleanTable(TableNameLookup = tablename,
                                                                    RemoveEmptyStrings = TRUE,
                                                                    RemoveDuplicateEntries = TRUE,
                                                                    FeatureObligations = Settings$FeatureObligations)
                              }

                              return(Table)

                          } else {

                              return(Table)
                          }
                       })

  try(ProgressBar$terminate())



#===============================================================================
# MONITORING: Count ineligible entries
#===============================================================================

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
                                            Info = Message)
  }
  cat("\n")




#===============================================================================
# MODULE C)  Table Normalization
#===============================================================================
#   - Perform procedures like 'split and expand' where necessary (as determined by settings / meta data)
#-------------------------------------------------------------------------------

  if (Settings$TableNormalization$Run == TRUE)
  {
    #-----------------------------------------------------------------------------
    ProgressBar <- progress_bar$new(format = "Table Normalization... [:bar] :percent in :elapsed  :spin",
                                    total = length(DataSet), clear = FALSE, width = 100)
    #-----------------------------------------------------------------------------

    DataSet <- DataSet %>%
                    imap(function(Table, tablename)
                         {
                            try(ProgressBar$tick())

                            Table <- Table %>%
                                          dsFreda::NormalizeTable(TableName = tablename,
                                                                  RuleSet = Settings$TableNormalization$RuleSet,
                                                                  RuleSet.Profile = Settings$TableNormalization$RuleSet.Profile)

                            return(Table)
                         })

    try(ProgressBar$terminate())
  }




#===============================================================================
# MODULE D)  Data Harmonization / Transformation
#===============================================================================
#   1) Conversion of all features into type character prior to definitive feature-specific formatting
#   2) Definition of features to monitor during Transformation
#   3) Tracking of raw feature values
#   4) Data harmonization (correctional transformation)
#   5) Tracking of harmonized feature values
#   6) Data recoding and formatting
#   7) Tracking of recoded / formatted feature values
#   8) Finalize transformation of data
#        - Removing of ineligible values
#        - Optional conversion to factor
#   9) Tracking of finalized feature values
#   10) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------

#===============================================================================
# Module D 1)  Conversion of all features into type character prior to definitive formatting
#===============================================================================

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          Table <- Table %>%
                                      mutate(across(everything(), ~ as.character(.x)))
                       })


#===============================================================================
# Module D 2)  Definition of tracked features and their sets of eligible values
#===============================================================================
#     - Create meta data on eligible value sets of features to be tracked / monitored during curation process
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data / passed rule set
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

  ls_MonitorMetaData <- names(DataSet) %>%
                            map(function(tablename)
                                {
                                    vc_FeaturesToTrack <- Settings$FeatureTracking$RuleSet %>%
                                                              rename(IsTracked = all_of(Settings$FeatureTracking$RuleSet.Profile)) %>%      # Renaming feature based on passed argument
                                                              filter(Table == tablename,
                                                                     IsTracked == TRUE) %>%
                                                              pull(Feature)

                                    ls_EligibleValues <- vc_FeaturesToTrack %>%
                                                              map(function(featurename)
                                                                  {
                                                                      Values <- MetaData$Values %>%
                                                                                    filter(Table == tablename,
                                                                                           FeatureName.Curated == featurename) %>%
                                                                                    select(Value.Raw,
                                                                                           Value.Curated)

                                                                      if (nrow(Values) == 0) { return(NULL) } else { return(Values) }
                                                                  }) %>%
                                                              set_names(vc_FeaturesToTrack)
                                }) %>%
                            set_names(names(DataSet))



#===============================================================================
# Module D 3)  Track feature values of raw data
#===============================================================================
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
                                             dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                        TransformationStage = "Raw") %>%
                                             select(Feature,
                                                    Value,
                                                    Frequency) %>%
                                             rename(Value.Raw = Value,
                                                    Count.Raw = Frequency)
                                      })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
# Module D 4)  Data Harmonization (correctional transformation)
#===============================================================================
#   Step-wise approach incorporating the following methods (feature-specific selection and order defined by passed settings)
#     - Transformative expressions
#     - Dictionary look-up
#     - Fuzzy String Matching
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Harmonizing data... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          Table <- Table %>% mutate(TrackID = row_number())      # Enables tracking of transformation (see above)

                          if (Settings$DataHarmonization$Run == TRUE)      # The check is placed here and not before the mapping function so that the 'TrackID' is appended regardless (see above)
                          {
                              HarmonizationProcess <- Settings$DataHarmonization$Process %>%
                                                          filter(Profile == Settings$DataHarmonization$Process.Profile,
                                                                 Table == tablename,
                                                                 RunHarmonization == TRUE) %>%
                                                          arrange(HarmonizationOrder)      # Defines the order in which features within a table are being harmonized (this can be relevant in transformative espressions that contain inter-feature dependencies)

                              for (featurename in HarmonizationProcess$Feature)
                              {
                                  Methods <- Settings$DataHarmonization$Process %>%
                                                  filter(Table == tablename,
                                                         Feature == featurename) %>%
                                                  as.list()

                                  EligibleValueSet <- MetaData$Values %>%
                                                          filter(Table == tablename,
                                                                 FeatureName.Curated == featurename) %>%
                                                          pull(Value.Raw)   # Eligible Values BEFORE recoding

                                  TransformativeExpressions <- Settings$DataHarmonization$TransformativeExpressions %>%
                                                                    filter(Table == tablename,
                                                                           Feature == featurename)

                                  Dictionary <- Settings$DataHarmonization$Dictionary %>%
                                                    filter(Table == tablename,
                                                           Feature == featurename) %>%
                                                    pull(var = NewValue,
                                                         name = LookupValue)

                                  FuzzyStringMatching <- Settings$DataHarmonization$FuzzyStringMatching %>%
                                                              filter(Table == tablename,
                                                                     Feature == featurename) %>%
                                                              as.list()

                                  Table[[featurename]] <- dsFreda::HarmonizeFeature(Feature = Table[[featurename]],
                                                                                    FeatureName = featurename,
                                                                                    ContextDataFrame = Table,
                                                                                    Methods = Methods,
                                                                                    EligibleValueSet = EligibleValueSet,
                                                                                    TransformativeExpressions = TransformativeExpressions,
                                                                                    Dictionary = Dictionary,
                                                                                    FuzzyStringMatching = FuzzyStringMatching)
                              }
                          }

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
# Module D 5)  Track feature values after Harmonization
#===============================================================================

# Map raw values to their harmonized state to get transformation tracks
#===============================================================================

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
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Harmonized <- map2(.x = DataSet,
                                        .y = ls_MonitorMetaData,
                                        .f = function(DataFrame, MonitorMetaData)
                                             {
                                                 DataFrame %>%
                                                    dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                              TransformationStage = "Harmonized") %>%
                                                    select(Feature,
                                                           Value,
                                                           Frequency) %>%
                                                    rename(Value.Harmonized = Value,
                                                           Count.Harmonized = Frequency)
                                             })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
# Module D 6)  Data recoding and formatting
#===============================================================================
#   - Recoding data using dsFreda::RecodeData() based on specifications in MetaData$Values
#   - RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#   - Format / Re-type data using dsFreda::FormatData() based on specifications in MetaData$Features
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Recoding and formatting data... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (length(Table) > 0 && nrow(Table) > 0)
                          {
                              # Recode table data as defined in meta data
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                              FeaturesWithValueSets <- MetaData$Values %>%
                                                            filter(Table == tablename) %>%
                                                            pull(FeatureName.Curated) %>%
                                                            unique()

                              if (length(FeaturesWithValueSets) > 0)
                              {
                                  RecodingDictionaries <- MetaData$Values %>%
                                                              filter(Table == tablename) %>%
                                                              split(.$FeatureName.Curated) %>%      # 'split' is a base function and needs '.$' to address 'FeatureName.Curated'
                                                              map(\(Values) with(Values, set_names(Value.Curated, Value.Raw)))

                                  for (featurename in FeaturesWithValueSets)
                                  {
                                      Table[[featurename]] <- dsFreda::RecodeData(TargetVector = Table[[featurename]],
                                                                                  Dictionary = RecodingDictionaries[[featurename]])
                                  }
                              }

                              # Format / Re-type table data as defined in meta data
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                              FeatureTypes <- MetaData$Features %>%
                                                  filter(TableName.Curated == tablename,
                                                         Type != "character") %>%
                                                  select(FeatureName.Curated, Type) %>%
                                                  rename(Feature = "FeatureName.Curated")

                              if (nrow(FeatureTypes) > 0)
                              {
                                  for (i in 1:nrow(FeatureTypes))
                                  {
                                      Table <- Table %>%
                                                    mutate(across(all_of(FeatureTypes$Feature[i]),
                                                                  ~ dsFreda::FormatData(.x, FeatureTypes$Type[i])))
                                  }
                              }
                          }

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
# Module D 7)  Track feature values after Recoding
#===============================================================================

# Map raw values to their recoded state to get transformation tracks
#===============================================================================

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
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Recoded <- map2(.x = DataSet,
                                     .y = ls_MonitorMetaData,
                                     .f = function(DataFrame, MonitorMetaData)
                                          {
                                              DataFrame %>%
                                                 dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                            TransformationStage = "Recoded") %>%
                                                 select(Feature,
                                                        Value,
                                                        Frequency) %>%
                                                 rename(Value.Recoded = Value,
                                                        Count.Recoded = Frequency)
                                          })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
# Module D 8)  Finalize transformation of data values using dsFreda::FinalizeDataTransformation()
#===============================================================================
#   - (Optional / Default) Exclusion of ineligible data (including data that could not be transformed)
#   - (Optional) Conversion to ordered factor
#   - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#   - All predefined information stored in MetaData$Values
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Finalizing data transformation... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (length(Table) > 0 && nrow(Table) > 0)
                          {
                              # Get features that are supposed to be harmonized from settings
                              HarmonizationProcess <- Settings$DataHarmonization$Process %>%
                                                          filter(Profile == Settings$DataHarmonization$Process.Profile,
                                                                 Table == tablename,
                                                                 RunHarmonization == TRUE)

                              for (featurename in HarmonizationProcess$Feature)
                              {
                                  # Get eligible value set for current feature as data.frame including data on factoring
                                  EligibleValueSet <- MetaData$Values %>%
                                                          filter(Table == tablename,
                                                                 FeatureName.Curated == featurename)

                                  if (nrow(EligibleValueSet) > 0)
                                  {
                                      Table[[featurename]] <- dsFreda::FinalizeDataTransformation(TargetVector = Table[[featurename]],
                                                                                                  EligibleValueSet = EligibleValueSet,
                                                                                                  ExcludeIneligibleValues = Settings$DataHarmonization$ExcludeIneligibleValues)
                                  }
                              }
                          }

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
# Module D 9)  Track Feature Values after Finalized Transformation
#===============================================================================

# Map raw values to their finalized state to get transformation tracks
#===============================================================================

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
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Final <- map2(.x = DataSet,
                                   .y = ls_MonitorMetaData,
                                   .f = function(DataFrame, MonitorMetaData)
                                        {
                                            DataFrame %>%
                                                dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                           TransformationStage = "Final") %>%
                                                select(Feature,
                                                       Value,
                                                       Frequency) %>%
                                                rename(Value.Final = Value,
                                                       Count.Final = Frequency)
                                        })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
# Module D 10)  Merge monitor objects into coherent summaries
#===============================================================================

# Summarize Transformation Tracks
#===============================================================================
  ls_TransformationTracks_Summaries <- pmap(.l = list(ls_TransformationTracks,
                                                      ls_MonitorMetaData),
                                            .f = function(TrackData,
                                                          MonitorMetaData)
                                                 {
                                                     if (length(TrackData) > 0 && nrow(TrackData) > 0)
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
                                                                         rename(Value.Raw = Raw,
                                                                                Value.Harmonized = Harmonized,
                                                                                Value.Recoded = Recoded,
                                                                                Value.Final = Final) %>%
                                                                         rowwise() %>%
                                                                         mutate(IsOccurring = TRUE,
                                                                                IsEligible.Raw = ifelse(is.na(Value.Raw) | is.null(MonitorMetaData[[Feature]]),      # If value is NA or if there is no set of eligible values, set variable NA...
                                                                                                        NA,
                                                                                                        Value.Raw %in% MonitorMetaData[[Feature]]$Value.Raw),      # ... else check if specific row value is in set of eligible values
                                                                                IsEligible.Harmonized = ifelse(is.na(Value.Harmonized) | is.null(MonitorMetaData[[Feature]]),
                                                                                                               NA,
                                                                                                               Value.Harmonized %in% MonitorMetaData[[Feature]]$Value.Raw),
                                                                                IsEligible.Recoded = ifelse(is.na(Value.Recoded) | is.null(MonitorMetaData[[Feature]]),
                                                                                                            NA,
                                                                                                            Value.Recoded %in% MonitorMetaData[[Feature]]$Value.Curated),
                                                                                IsEligible.Final = ifelse(is.na(Value.Final),
                                                                                                          NA,
                                                                                                          TRUE)) %>%
                                                                         ungroup()

                                                         # Add set of all eligible values regardless of occurrence to summary
                                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                         for (i in 1:length(MonitorMetaData))   # Loop through all monitored features of a table
                                                         {
                                                             AllEligibleValues <- tibble(Feature = names(MonitorMetaData)[i],
                                                                                         Value.Raw = MonitorMetaData[[i]]$Value.Raw,
                                                                                         IsOccurring = FALSE,
                                                                                         IsEligible.Raw = TRUE)

                                                             Summary <- bind_rows(Summary,
                                                                                  AllEligibleValues)
                                                         }

                                                         # Filter out eligible values marked as not occurring if they actually occur
                                                         # Result: All eligible values are included in summary, regardless of occurrence
                                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                         Summary <- Summary %>%
                                                                        group_by(Feature, Value.Raw) %>%
                                                                            arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                                            slice_head() %>%
                                                                        ungroup() %>%
                                                                        arrange(Feature,
                                                                                desc(IsOccurring),
                                                                                desc(IsEligible.Raw),
                                                                                desc(IsEligible.Harmonized),
                                                                                Value.Raw)

                                                         return(Summary)

                                                     } else { return (data.frame()) }
                                                 })


# Create detailed transformation monitors
#===============================================================================
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
                                                     left_join(ValueCountsRaw, by = c("Feature", "Value.Raw")) %>%
                                                     left_join(ValueCountsHarmonized, by = c("Feature", "Value.Harmonized")) %>%
                                                     left_join(ValueCountsRecoded, by = c("Feature", "Value.Recoded")) %>%
                                                     left_join(ValueCountsFinal, by = c("Feature", "Value.Final")) %>%
                                                     mutate(Count.Harmonized = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                         TRUE ~ Count.Harmonized),
                                                            Count.Recoded = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                      TRUE ~ Count.Recoded),
                                                            Count.Final = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                    TRUE ~ Count.Final)) %>%
                                                     arrange(Feature,
                                                             desc(IsOccurring),
                                                             desc(IsEligible.Raw),
                                                             desc(IsEligible.Harmonized),
                                                             Value.Raw)
                                             }
                                             else { return(NULL) }
                                         })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  ls_EligibilityOverviews <- pmap(.l = list(ls_TransformationMonitors,
                                            ls_MonitorMetaData),
                                  .f = function(MonitorData,
                                                MonitorMetaData)
                                       {
                                          if (length(MonitorData) > 0 && nrow(MonitorData) > 0)
                                          {
                                              # Filter out features that are not meant to be monitored, e.g. do not have applicable eligibility criteria
                                              MonitorData <- MonitorData %>%
                                                                  filter(Feature %in% names(MonitorMetaData))

                                              SummaryRaw <- MonitorData %>%
                                                                group_by(Feature, IsEligible.Raw) %>%
                                                                    summarize(Raw = sum(Count.Raw, na.rm = TRUE)) %>%
                                                                    rename(Eligibility = IsEligible.Raw)

                                              SummaryHarmonized <- MonitorData %>%
                                                                        distinct(pick(Feature, Value.Harmonized), .keep_all = TRUE) %>%
                                                                        group_by(Feature, IsEligible.Harmonized) %>%
                                                                            summarize(Harmonized = sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                            rename(Eligibility = IsEligible.Harmonized)

                                              SummaryRecoded <- MonitorData %>%
                                                                    distinct(pick(Feature, Value.Recoded), .keep_all = TRUE) %>%
                                                                    group_by(Feature, IsEligible.Recoded) %>%
                                                                        summarize(Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                        rename(Eligibility = IsEligible.Recoded)

                                              SummaryFinal <- MonitorData %>%
                                                                  distinct(pick(Feature, Value.Final), .keep_all = TRUE) %>%
                                                                  group_by(Feature, IsEligible.Final) %>%
                                                                      summarize(Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                      rename(Eligibility = IsEligible.Final)

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

                                          } else { return(data.frame()) }
                                      })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  ls_ValueSetOverviews <- ls_TransformationMonitors %>%
                              map(function(MonitorData)
                                  {
                                      if (length(MonitorData) > 0 && nrow(MonitorData) > 0)
                                      {
                                          ValueSets <- list()

                                          ValueSets$Raw <- MonitorData %>%
                                                                select(Feature, Value.Raw, IsOccurring, IsEligible.Raw, Count.Raw) %>%
                                                                group_by(Feature) %>%
                                                                    mutate(Proportion_Raw = Count.Raw / sum(Count.Raw, na.rm = TRUE)) %>%
                                                                ungroup()

                                          ValueSets$Harmonized <- MonitorData %>%
                                                                      group_by(Feature, Value.Harmonized, IsEligible.Harmonized) %>%
                                                                          summarize(Count.Harmonized = sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                      ungroup() %>%
                                                                      distinct(Feature, Value.Harmonized, .keep_all = TRUE) %>%
                                                                      group_by(Feature) %>%
                                                                          mutate(Proportion_Harmonized = Count.Harmonized / sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                      ungroup()

                                          ValueSets$Recoded <- MonitorData %>%
                                                                    group_by(Feature, Value.Recoded, IsEligible.Recoded) %>%
                                                                        summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                    ungroup() %>%
                                                                    distinct(Feature, Value.Recoded, .keep_all = TRUE) %>%
                                                                    group_by(Feature) %>%
                                                                        mutate(Proportion_Recoded = Count.Recoded / sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                    ungroup()

                                          ValueSets$Final <- MonitorData %>%
                                                                  group_by(Feature, Value.Final, IsEligible.Final) %>%
                                                                      summarize(Count.Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                  ungroup() %>%
                                                                  distinct(Feature, Value.Final, .keep_all = TRUE) %>%
                                                                  group_by(Feature) %>%
                                                                      mutate(Proportion_Final = Count.Final / sum(Count.Final, na.rm = TRUE)) %>%
                                                                  ungroup()

                                          return(ValueSets)

                                      } else { return(list()) }
                                  })



# Delete artificial "TrackID"-column from data frames (not needed anymore)
#===============================================================================

  DataSet <- DataSet %>%
                  map(function(Table)
                      {
                          try(Table %>% select(-TrackID))
                      })

  # Print info message
  cli::cat_bullet("Data transformation monitors are stored in 'CurationReport$Transformation'", bullet = "info")
  cat("\n")




#===============================================================================
# MODULE E)  Secondary entry exclusion
#===============================================================================
#   - Remove ineligible entries that may have been introduced during processing
#   - Same proceedings as in primary table cleaning (Module B))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Secondary exclusion: Excluding ineligible table entries... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSetRoot <- DataSet$Patient %>%
                      left_join(DataSet$Diagnosis, by = join_by(PatientID)) %>%
                      {   if (Settings$TableCleaning$Run == TRUE)
                          {
                              dsFreda::CleanTable(Table = .,
                                                  TableNameLookup = c("Diagnosis", "Patient"),
                                                  RemoveDuplicateEntries = FALSE,
                                                  FeatureObligations = Settings$FeatureObligations)
                          } else {.}
                      } %>%
                      select(PatientID, DiagnosisID) %>%
                      distinct()

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                          {
                              # Join current table with preselection of 'DataSetRoot'
                              if (all(c("PatientID", "DiagnosisID") %in% names(Table)))
                              {
                                  Table <- DataSetRoot %>%
                                                left_join(Table, by = join_by(PatientID, DiagnosisID))

                              } else {

                                  Table <- DataSetRoot %>%
                                              select(PatientID) %>%
                                              distinct() %>%
                                              left_join(Table, by = join_by(PatientID))
                              }

                              # Clean current table using auxiliary function dsFreda::CleanTable()
                              if (Settings$TableCleaning$Run == TRUE)
                              {
                                  Table <- Table %>%
                                                dsFreda::CleanTable(TableNameLookup = tablename,
                                                                    RemoveDuplicateEntries = TRUE,
                                                                    FeatureObligations = Settings$FeatureObligations)
                              }

                              return(Table)

                          } else {

                              return(Table)
                          }
                       })

  try(ProgressBar$terminate())


#===============================================================================
# MONITORING: Count ineligible entries after secondary exclusion
#===============================================================================

  # Count entries in data frames after secondary exclusion
  CountEntries_AfterSecondaryExclusion <- DataSet %>%
                                              map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

  # Count excluded entries
  CountExcludedEntries_Secondary <- CountEntries_AfterPrimaryExclusion - CountEntries_AfterSecondaryExclusion


  # Print messages for live monitoring in local tests
  for (i in 1:length(CountExcludedEntries_Secondary))
  {
      Message <- paste0("Secondary exclusion: Removed ", CountExcludedEntries_Secondary[i], " ineligible entries from '", names(CountExcludedEntries_Secondary)[i], "' table.")
      cli::cat_bullet(Message, bullet = "info")

      # Save messages in output object
      Messages$ExcludedEntries_Secondary <- c(Messages$ExcludedEntries_Secondary,
                                              Info = Message)
  }
  cat("\n")




#===============================================================================
# MODULE F)  Find and remove secondary redundancies (table entries that can be considered redundant when they provide no additional informational value compared to a previous entry)
#===============================================================================
# 1)  Process table 'Diagnosis' first, since other tables hold primary key 'DiagnosisID'.
#     Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables.
# 2)  Proceed with all other tables (excluding 'Patient')
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress_bar$new(format = "Removing redundant table entries... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet) - 1, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

  try(ProgressBar$tick())

  # Using dsFreda::FindRedundantEntries(), mark redundant entries in table 'Diagnosis' for further processing
  Aux_DiagnosisRedundancies <- DataSet$Diagnosis %>%
                                    dsFreda::FindRedundantEntries(PrimaryKeyFeature = "DiagnosisID",
                                                                  DiscriminatoryFeatures = MetaData$Features %>%
                                                                                               filter(TableName.Curated == "Diagnosis", IsDiscriminatory == TRUE) %>%
                                                                                               pull(FeatureName.Curated),
                                                                  EssentialFeatures = MetaData$Features %>%
                                                                                           filter(TableName.Curated == "Diagnosis", IsEssential == TRUE) %>%
                                                                                           pull(FeatureName.Curated),
                                                                  RemoveRedundantEntries = FALSE)

  # Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables
  # Create a mapping structure to know which IDs to replace
  Aux_RedundanciesIDMapping <- Aux_DiagnosisRedundancies %>%
                                    filter(IsRedundant == TRUE) %>%
                                    select(PatientID, DiagnosisID, ReferenceID)

  # Afterwards, remove redundant entries from 'Diagnosis'
  DataSet$Diagnosis <- Aux_DiagnosisRedundancies %>%
                            filter(IsRedundant == FALSE) %>%
                            select(-IsRedundant,
                                   -ReferenceID)

  # Find and remove redundant entries in other tables (except 'Patient')
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          if (nrow(Table) > 0 & !(tablename %in% c("Patient", "Diagnosis")))
                          {
                              try(ProgressBar$tick())

                              # For all tables that use 'DiagnosisID', make sure the previously removed DiagnosisIDs are replaced by respective reference DiagnosisIDs...
                              if (all(c("PatientID", "DiagnosisID") %in% names(Table)))
                              {
                                  Table <- Table %>%
                                                left_join(Aux_RedundanciesIDMapping, by = join_by(PatientID,
                                                                                                  DiagnosisID)) %>%
                                                mutate(DiagnosisID = ifelse(!is.na(ReferenceID),
                                                                            ReferenceID,
                                                                            DiagnosisID)) %>%
                                                select(-ReferenceID)
                              }

                              # ... then proceed with secondary redundancy removal
                              Table <- Table %>%
                                            dsFreda::FindRedundantEntries(PrimaryKeyFeature = MetaData$Features %>%
                                                                                                   filter(TableName.Curated == tablename, IsPrimaryKey == TRUE) %>%
                                                                                                   pull(FeatureName.Curated),
                                                                          DiscriminatoryFeatures = MetaData$Features %>%
                                                                                                       filter(TableName.Curated == tablename, IsDiscriminatory == TRUE) %>%
                                                                                                       pull(FeatureName.Curated),
                                                                          EssentialFeatures = MetaData$Features %>%
                                                                                                   filter(TableName.Curated == tablename, IsEssential == TRUE) %>%
                                                                                                   pull(FeatureName.Curated),
                                                                          RemoveRedundantEntries = TRUE)

                          } else { return(Table) }
                       })

  try(ProgressBar$terminate())


#===============================================================================
# MONITORING: Count secondary redundancies
#===============================================================================

  # Count entries in data frames after secondary redundancy removal
  CountEntries_AfterSecondaryRedundancyExclusion <- DataSet %>%
                                                        map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

  # Count excluded entries
  CountExcludedEntries_SecondaryRedundancy <- CountEntries_AfterSecondaryExclusion - CountEntries_AfterSecondaryRedundancyExclusion


  # Print messages for live monitoring in local tests
  for (i in 1:length(CountExcludedEntries_SecondaryRedundancy))
  {
      Message <- paste0("Secondary redundancy: Removed ", CountExcludedEntries_SecondaryRedundancy[i], " redundant entries from '", names(CountExcludedEntries_SecondaryRedundancy)[i], "' table.")
      cli::cat_bullet(Message, bullet = "info")

      # Save messages in output object
      Messages$ExcludedEntries_SecondaryRedundancy <- c(Messages$ExcludedEntries_SecondaryRedundancy,
                                                        Info = Message)
  }
  cat("\n")




#===============================================================================
# Final modifications
#===============================================================================

  # Conversion of Tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  DataSet <- DataSet %>%
                  map(\(Table) as.data.frame(Table))




#===============================================================================
# Compile content of 'CurationReport'
#===============================================================================

  CurationReport <- list(EntryCounts = data.frame(Table = names(DataSet),
                                                  InitialCount = CountEntries_Initial,
                                                  ExcludedPrimary = CountExcludedEntries_Primary,
                                                  AfterPrimaryExclusion = CountEntries_AfterPrimaryExclusion,
                                                  ExcludedSecondary = CountExcludedEntries_Secondary,
                                                  AfterSecondaryExclusion = CountEntries_AfterSecondaryExclusion,
                                                  ExcludedSecondaryRedundancy = CountExcludedEntries_SecondaryRedundancy,
                                                  AfterSecondaryRedundancyExclusion = CountEntries_AfterSecondaryRedundancyExclusion),
                         Transformation = list(Monitors = ls_TransformationMonitors,
                                               EligibilityOverviews = ls_EligibilityOverviews,
                                               ValueSetOverviews = ls_ValueSetOverviews))

  Messages$CheckCurationCompletion <- "green"
  Messages$FinalMessage <- "Data Curation performed successfully!"

  # Print completion message
  Message <- paste0("Data Curation performed successfully!")
  cli::cat_bullet(Message, bullet = "tick")
  cat("\n")


  # },
  #
  # # In case of occurring warning:
  # #===============================================================================
  # warning = function(w)
  #           {
  #               Messages$CheckCurationCompletion <- "yellow"
  #               Messages$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
  #           },
  #
  # # In case of occurring error:
  # #===============================================================================
  # error = function(e)
  #         {
  #             Messages$CheckCurationCompletion <- "red"
  #             Messages$FinalMessage <- paste0("An error occured: \n", e)
  #         },
  #
  # #===============================================================================
  # # RETURN STATEMENT
  # #===============================================================================
  # finally =
  # {
  #   # Return the Curated Data Set (CDS) a Curation Report (defined above) and Messages
    return(list(CuratedDataSet = DataSet,
                CurationReport = CurationReport,
                Messages = Messages))
  # })

}

