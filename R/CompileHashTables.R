
#' CompileHashTables
#'
#' Auxiliary function within \code{\link{CurateDataDS}} or \code{\link{HarmonizeData}} respectively
#'
#' Based on a predefined rule set data frame, compile hash tables for direct value transformation
#'
#' @param TableName \code{string} - Name of table that contains features to be transformed
#' @param RuleSet \code{data.frame} - Contains predefined set of transformation rules
#' @param RuleSet.Profile \code{string} - Profile name stated in 'RuleSet'
#'
#' @return \code{list} of named vectors (serving as hash tables)
#' @export
#'
#' @author Bastian Reiter
CompileHashTables <- function(TableName,
                              RuleSet,
                              RuleSet.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(purrr)
    require(stringr)

    #--- For testing purposes ---
    # TableName <- "Staging"
    # RuleSet <- dsCCPhos::RuleSet_RawDataTransformation
    # RuleProfile <- "Default"


    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleSet.Profile
                                  & Table == TableName
                                  & Operation == "HashTable") %>%
                          arrange(EvaluationOrder)

    if (nrow(RelevantRules) == 0)
    {
        return(NULL)
    }
    else
    {
        # Get list of hash tables (named vectors) corresponding to occurring features
        HashTables <- RelevantRules %>%
                          split(RelevantRules$Feature) %>%
                          map(\(df) setNames(df$NewValue,
                                             nm = df$LookupValue))

        return(HashTables)
    }
}
