
#' CompileHarmonizationRules
#'
#' Auxiliary function within \code{\link{CurateDataDS}} or \code{\link{HarmonizeData}} respectively
#'
#' Based on a predefined rule set data frame, compile argument expressions for dplyr::mutate()
#'
#' @param TableName \code{string} - Name of table that contains features to be transformed
#' @param RuleSet \code{data.frame} - Contains predefined set of harmonization rules
#' @param RuleSet.Profile \code{string} - Profile name stated in rule set data frame
#'
#' @return \code{string} containing arguments for \code{dplyr::mutate()}
#' @export
#'
#' @author Bastian Reiter
CompileHarmonizationRules <- function(TableName,
                                      RuleSet,
                                      RuleSet.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(purrr)
    require(stringr)

    # For testing purposes
    # TableName <- "Staging"
    # RuleSet <- dsCCPhos::RuleSet_RawDataHarmonization
    # RuleSet.Profile <- "Default"


    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleSet.Profile
                                  & Table == TableName
                                  & Operation != "HashTable") %>%
                          arrange(EvaluationOrder)

    if (nrow(RelevantRules) == 0)
    {
        return(NA)
    }
    else
    {
        vc_Rules <- NULL

        for (i in 1:nrow(RelevantRules))
        {
            Rule <- NA
            Feature <- RelevantRules$Feature[i]
            Operation <- RelevantRules$Operation[i]

            if (!is.na(Feature) & !is.na(Operation))
            {
                Operation <- str_replace(Operation, ".X", Feature)
                Operation <- str_replace(Operation, ".REP", paste0("c('", RelevantRules$LookupValue[i], "' = '", RelevantRules$NewValue[i], "')"))
                Rule <- paste0(Feature,
                               " = ",
                               Operation)
            }

            vc_Rules <- c(vc_Rules, Rule)
        }

        # Sort out Rules that are NA and consolidate in a single string
        Rules <- paste0(vc_Rules[!is.na(vc_Rules)], collapse = ", ")
        if (Rules == "") { Rules <- NA }

        return(Rules)
    }
}
