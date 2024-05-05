
#' GetFeatureInfoDS
#'
#' Obtain data about feature type and sample size.
#'
#' @param TableName.S \code{string} | Name of the data frame that contains the feature
#' @param FeatureName.S \code{string} | Name of feature
#'
#' @return A \code{tibble}
#' @export
#'
#' @examples
GetFeatureInfoDS <- function(TableName.S,
                             FeatureName.S)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check, evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName.S) & is.character(FeatureName.S))
{
    Table <- eval(parse(text = TableName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "Error: 'TableName.S' and 'FeatureName.S' must be specified as character strings."
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(tibble)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function proceedings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For function testing purposes
# TableName.S <- ADS$Patients
# FeatureName.S <- "TNM_T"


# Evaluate feature in question
Feature <- Table[[FeatureName.S]]

# Tibble containing useful meta data
MetaData <- tibble(DataType = class(Feature),
                   N_Total = length(Feature),
                   N_Valid = sum(!is.na(Feature)),
                   ValidProportion = N_Valid / N_Total,
                   N_Missing = sum(is.na(Feature)),
                   MissingProportion = N_Missing / N_Total,
                   CountUniqueValues = length(unique(Feature[!is.na(Feature)])))      # Count only non-NA unique values

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(MetaData)
}
