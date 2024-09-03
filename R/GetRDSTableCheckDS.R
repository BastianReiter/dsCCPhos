
#' GetRDSTableCheck
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
#' @examples
#' @author Bastian Reiter
GetRDSTableCheckDS <- function(RawDataSetName.S = "RawDataSet")
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check existence and completeness of tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Go through available data frames in RawDataSet and check for completeness
TableCheckPresent <- RawDataSet %>%
                          imap(function(dataframe, name)
                               {
                                  TableExists <- (!is_empty(dataframe))

                                  RequiredFeatureNames <- dplyr::filter(dsCCPhos::Meta_FeatureNames, TableName_Curated == name)$FeatureName_Raw
                                  PresentFeatureNames <- names(dataframe)

                                  MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

                                  TableComplete <- (length(MissingFeatures) == 0)

                                  return(list(TableExists = TableExists,
                                              TableComplete = TableComplete,
                                              MissingFeatures = MissingFeatures))
                               })

# Get all required table names from meta data
RequiredTableNames <- paste0("RDS_", dsCCPhos::Meta_TableNames$TableName_Curated)

# Compile summarizing list of table check
TableCheck <- RequiredTableNames %>%
                  map(function(tablename)
                      {
                          if (tablename %in% names(TableCheckPresent)) { return(TableCheckPresent[[tablename]]) }
                          else { return(list(TableExists = FALSE)) }
                      }) %>%
                  set_names(RequiredTableNames)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(TableCheck)

}
