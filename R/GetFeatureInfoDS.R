
#' GetFeatureInfoDS
#'
#' Obtain data about feature type and sample size.
#'
#' @param TableName.S \code{string} - Name of the data frame that contains the feature
#' @param FeatureName.S \code{string} - Name of feature
#'
#' @return A \code{list}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetFeatureInfoDS <- function(TableName.S,
                             FeatureName.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# --- For Testing Purposes ---
# TableName.S <- ADS$Patients
# FeatureName.S <- "TNM_T"


# Evaluate feature in question / Get vector
Feature <- Table[[FeatureName.S]]

# Tibble containing useful properties
Properties <- tibble(DataType = class(Feature),
                     N.Total = length(Feature),
                     N.Valid = sum(!is.na(Feature)),
                     ValidProportion = N.Valid / N.Total,
                     N.Missing = sum(is.na(Feature)),
                     MissingProportion = N.Missing / N.Total,
                     CountUniqueValues = length(unique(Feature[!is.na(Feature)])))      # Count only non-NA unique values


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(Properties)
}
