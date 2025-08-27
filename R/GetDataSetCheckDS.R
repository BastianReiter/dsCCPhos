
#' GetDataSetCheckDS
#'
#' Checks out a given data set (list of data.frames) and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames.S \code{character vector} - Names of tables that are expected/required to be in the data set - Default: Names of elements in list evaluated from \code{DataSetName.S}
#' @param RequiredFeatureNames.S \code{list} of \code{character vectors} - Features that are expected/required in each table of the data set - Default: Names of features in respective table
#' @param AssumeCCPDataSet.S \code{logical} - Whether or not the data set to be checked out is one of the main data sets used in CCPhos - Default: FALSE
#'
#' @return A \code{list} containing meta data about tables in a data set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheckDS <- function(DataSetName.S,
                              RequiredTableNames.S = NULL,
                              RequiredFeatureNames.S = NULL,
                              AssumeCCPDataSet.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate and parse input before proceeding
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.character(DataSetName.S))
  {
      DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())
  }
  else
  {
      ClientMessage <- "ERROR: 'DataSetName.S' must be specified as a character string"
      stop(ClientMessage, call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Start of function proceedings -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # --- For Testing Purposes ---
  # DataSetName.S <- "RawDataSet"
  # DataSet <- RawDataSet
  # RequiredTableNames.S <- NULL
  # RequiredFeatureNames.S <- NULL
  # AssumeCCPDataSet.S <- TRUE
  # RequiredTableNames.S = paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated)
  # RequiredFeatureNames.S = RequiredTableNames.S %>%
  #                             map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
  #                             set_names(RequiredTableNames.S)

  require(dplyr)
  require(purrr)
  require(stringr)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check existence and completeness of tables
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # If argument 'RequiredTableNames.S' is not passed, assign present table names in 'DataSet' per default
  if (is.null(RequiredTableNames.S)) { RequiredTableNames.S <- names(DataSet) }

  # If no feature names list is passed, assign an empty list
  if (is.null(RequiredFeatureNames.S)) { RequiredFeatureNames.S <- list() }


  if (AssumeCCPDataSet.S == TRUE)
  {
      if (DataSetName.S == "RawDataSet")
      {
          RequiredTableNames.S <- paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated)
          RequiredFeatureNames.S <- RequiredTableNames.S %>%
                                        map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
                                        set_names(RequiredTableNames.S)
      }
      if (DataSetName.S == "CuratedDataSet")
      {
          RequiredTableNames.S <- dsCCPhos::Meta_Tables$TableName_Curated
          RequiredFeatureNames.S <- RequiredTableNames.S %>%
                                        map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Curated) %>%
                                        set_names(RequiredTableNames.S)
      }
  }


  # Create Table check templates for all required Data Set tables ("assume as empty/missing")
  DataSetCheckTemplate <- RequiredTableNames.S %>%
                              map(function(tablename)
                                  {
                                      CheckTable(Table = NULL,
                                                 RequiredFeatureNames = RequiredFeatureNames.S[[tablename]])

                                  }) %>%
                              set_names(RequiredTableNames.S)


  # Go through actually existing tables in 'DataSet' and check for feature completeness as well as feature types, row counts and rates of non-missing values
  DataSetCheckExisting <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      if (length(Table) > 0 && !is.null(Table) && !is_empty(Table) && nrow(Table) > 0)
                                      {
                                          CheckTable(Table = Table,
                                                     RequiredFeatureNames = RequiredFeatureNames.S[[tablename]])
                                      }
                                      else { return(NULL) }
                                   }) %>%
                              compact()      # This removes all NULL elements from list


  # Replace info in TableCheckTemplate with info about existing tables to get coherent summary
  DataSetCheck <- DataSetCheckTemplate %>%
                      imap(function(Table, tablename)
                           {
                              if (tablename %in% names(DataSetCheckExisting)) { return(DataSetCheckExisting[[tablename]]) }
                              else { return(Table) }
                           })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(DataSetCheck)
}
