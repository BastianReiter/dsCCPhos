
#' GetDataSetOverviewDS
#'
#' Assesses feature types, row counts and data completeness on a given data set (e.g. RDS, CDS, ADS)
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (usually a list) on server
#'
#' @return A list of data.frames containing info about data set tables
#' @export
#'
#' @author Bastian Reiter
GetDataSetOverviewDS <- function(DataSetName.S)
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

### For testing purposes
# DataSet <- RawDataSet

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load package namespaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(purrr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Go tables in data set and get feature types, row counts and data completeness
Overview <- DataSet %>%
                imap(function(Table, tablename)
                     {
                        if (!(is_empty(Table) | length(Table) == 0 | nrow(Table) == 0))
                        {
                            # Get named vector containing types/classes of table features
                            FeatureTypes <- sapply(Table, typeof)

                            RowCount <- nrow(Table)

                            # Get rate of non-missing values per table feature
                            RateNonMissingValues <- Table %>%
                                                        summarize(across(everything(), ~ sum(!(is.na(.x) | .x == "")) / n()))

                            return(list(FeatureTypes = FeatureTypes,
                                        RowCount = RowCount,
                                        RateNonMissingValues = RateNonMissingValues))
                        }
                        else { return(NULL) }
                     })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return list of overview data.frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return(Overview)

}
