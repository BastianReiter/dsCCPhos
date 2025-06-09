
#' GetRDSValidationReportDS
#'
#' Performs validation operations on Raw Data Set (RDS) and returns a list containing informative data frames.
#'
#' Server-side AGGREGATE method
#'
#' @param RawDataSetName.S String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#'
#' @return A list containing validation reports
#' @export
#'
#' @author Bastian Reiter
GetRDSValidationReportDS <- function(RawDataSetName.S = "RawDataSet")
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

### For testing purposes
# RawDataSetName.S <- "RawDataSet"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load package namespaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(purrr)
require(stringr)
require(validate)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename features in tables to make sure R object naming rules are respected (this renaming only extends to the scope of this function)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RawDataSet <- RawDataSet %>%
                  imap(function(dataframe, name)
                       {
                          # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                          name <- str_remove(name, "RDS_")   # Remove "RDS_" prefix from table names
                          vc_Lookup <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == name)$FeatureName_Raw
                          names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == name)$FeatureName_Curated

                          if (!is_empty(dataframe))
                          {
                              # Rename feature names according to look-up vector
                              dplyr::rename(dataframe, any_of(vc_Lookup))      # Returns a tibble
                          }
                          else { return(dataframe) }
                       })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define validation rules in data frames according to syntax demanded by package "validate"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ValidationRules <- names(RawDataSet) %>%
                      map(function(tablename)
                           {
                              # Get table feature names from meta data
                              tablename <- str_remove(tablename, "RDS_")   # Remove "RDS_" prefix from table names
                              # Raw feature names
                              vc_FeatureNames <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Raw
                              # Corresponding curated feature names (used for ensured compatibility with R naming rules)
                              names(vc_FeatureNames) <- dplyr::filter(dsCCPhos::Meta_Features, TableName_Curated == tablename)$FeatureName_Curated

                              # Validation rules: data type
                              Rules_DataType <- names(vc_FeatureNames) %>%
                                                    map(\(curatedfeaturename) data.frame(name = paste0("DataTypeCharacter...", tablename, "...", vc_FeatureNames[curatedfeaturename]),
                                                                                         description = "",
                                                                                         rule = paste0("is.character(", curatedfeaturename, ")"))) %>%
                                                    list_rbind()

                              # Validation rules: missing data
                              Rules_Missings <- names(vc_FeatureNames) %>%
                                                    map(\(curatedfeaturename) data.frame(name = paste0("NonMissingValues...", tablename, "...", vc_FeatureNames[curatedfeaturename]),
                                                                                         description = "",
                                                                                         rule = paste0("!is.na(", curatedfeaturename, ")"))) %>%
                                                    list_rbind()


                              # Return consolidated data frame of validation rules
                              return(rbind(Rules_DataType,
                                           Rules_Missings))

                           }) %>% set_names(names(RawDataSet))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Functionality of package 'validate': Confront each data frame with a 'validator' based on the corresponding validation rule set
# - Obtain summary (data frame) of the resulting validation process
#-------------------------------------------------------------------------------

ValidationReports <- pmap(.l = list(RawDataSet,
                                    ValidationRules),
                          .f = \(dataframe, ruleset) confront(dataframe,
                                                              validator(.data = ruleset)))

# ValidationSummaries <- ValidationReports %>%
#                             map(\(report) summary(report))
#
#
# ValidationReportTables <- ValidationSummaries %>%
#                               map(\(summary) as.data.frame(summary, check.names = FALSE))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return list of validation report data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return(ValidationReports)

}
