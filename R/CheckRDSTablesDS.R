
#' CheckRDSTablesDS
#'
#' Checks existence and completeness of RawDataSet (RDS) tables and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#'
#' @return A list containing information about existence and completeness of RDS tables
#' @export
#'
#' @author Bastian Reiter
CheckRDSTablesDS <- function(RawDataSetName.S = "RawDataSet")
{
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
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(purrr)
require(stringr)
require(tidyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check existence and completeness of tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create template of info about all required RDS tables ("assume as empty/missing")
TableCheckTemplate <- dsCCPhos::Meta_Tables$TableName_Curated %>%
                          map(function(tablename)
                              {
                                  RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Raw
                                  MissingFeatures <- RequiredFeatureNames

                                  FeatureCheck <- tibble(Feature = RequiredFeatureNames,
                                                         Exists = FALSE,
                                                         Type = NA,
                                                         NonMissingValueRate = NA)

                                  return(list(TableExists = FALSE,
                                              TableComplete = FALSE,
                                              FeatureCheck = FeatureCheck,
                                              MissingFeatures = MissingFeatures,
                                              RowCount = NA))
                              }) %>%
                          set_names(paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated))


# Go through available data frames in RawDataSet and check for feature completeness as well as feature types, row counts and rates of non-missing values
TableCheckExisting <- RawDataSet %>%
                          imap(function(Table, tablename)
                               {
                                  if (!(is_empty(Table) | length(Table) == 0 | nrow(Table) == 0))
                                  {
                                      RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw
                                      PresentFeatureNames <- names(Table)
                                      MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]
                                      TableComplete <- (length(MissingFeatures) == 0)

                                      # Get table row count
                                      RowCount <- nrow(Table)

                                      # Get summarizing data.frame that contains info about existence of table features
                                      FeatureExistence <- tibble(Feature = RequiredFeatureNames) %>%
                                                               mutate(Exists = case_when(Feature %in% PresentFeatureNames ~ TRUE,
                                                                                         .default = FALSE))

                                      # Get types/classes of table features
                                      FeatureTypes <- Table %>%
                                                          summarize(across(everything(), ~ typeof(.x))) %>%
                                                          pivot_longer(cols = everything(),
                                                                       names_to = "Feature",
                                                                       values_to = "Type")

                                      # Get rate of non-missing values per table feature
                                      NonMissingValueRates <- Table %>%
                                                                  summarize(across(everything(), ~ sum(!(is.na(.x) | .x == "")) / n())) %>%
                                                                  pivot_longer(cols = everything(),
                                                                               names_to = "Feature",
                                                                               values_to = "NonMissingValueRate")

                                      # Consolidate feature meta data in one data.frame
                                      FeatureCheck <- FeatureExistence %>%
                                                          left_join(FeatureTypes, by = join_by(Feature)) %>%
                                                          left_join(NonMissingValueRates, by = join_by(Feature))

                                      return(list(TableExists = TRUE,
                                                  TableComplete = TableComplete,
                                                  FeatureCheck = FeatureCheck,
                                                  MissingFeatures = MissingFeatures,
                                                  RowCount = RowCount))
                                  }
                                  else { return(NULL) }
                               })


# Replace info in TableCheckTemplate with info about existing tables to get coherent summary
TableCheck <- TableCheckTemplate %>%
                  imap(function(Table, tablename)
                       {
                          if (tablename %in% names(TableCheckExisting)) { return(TableCheckExisting[[tablename]]) }
                          else { return(Table) }
                       })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(TableCheck)

}
