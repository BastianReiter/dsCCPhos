
#' CheckRDSTablesDS
#'
#' Checks existence and completeness of RawDataSet (RDS) tables and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param RawDataSetName.S String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check existence and completeness of tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create template of info about all required RDS tables ("assume as empty/missing")
TableCheckTemplate <- dsCCPhos::Meta_Tables$TableName_Curated %>%
                          map(function(tablename)
                              {
                                  RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Raw

                                  FeatureExistence <- tibble(FeatureName = RequiredFeatureNames) %>%
                                                          mutate(Exists = FALSE)

                                  MissingFeatures <- RequiredFeatureNames

                                  return(list(TableExists = FALSE,
                                              TableComplete = FALSE,
                                              FeatureExistence = FeatureExistence,
                                              MissingFeatures = MissingFeatures))
                              }) %>%
                          set_names(paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated))


# Go through available data frames in RawDataSet and check for feature completeness
TableCheckExisting <- RawDataSet %>%
                          imap(function(Table, name)
                               {
                                  if (!is_empty(Table))
                                  {
                                      RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(name, "RDS_"))$FeatureName_Raw
                                      PresentFeatureNames <- names(Table)

                                      FeatureExistence <- tibble(FeatureName = RequiredFeatureNames) %>%
                                                                 mutate(Exists = case_when(FeatureName %in% PresentFeatureNames ~ TRUE,
                                                                                           TRUE ~ FALSE))

                                      MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

                                      TableComplete <- (length(MissingFeatures) == 0)

                                      #FeatureTypes <-

                                      return(list(TableExists = TRUE,
                                                  TableComplete = TableComplete,
                                                  FeatureExistence = FeatureExistence,
                                                  MissingFeatures = MissingFeatures))
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
