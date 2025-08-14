
#' NormalizeTable
#'
#' Carries out table harmonization procedures (like 'splitting/expanding') based on a given rule set.
#'
#' Uses \code{tidyr} functionality to transform table based on normalization rules defined in a given rule set (Default: dsCCPhos::Meta_TableHarmonization)
#'
#' @param DataFrame \code{data.frame} containing data to be transformed
#' @param TableName \code{string} - Name of the table (to enable mapping to normalization rules)
#' @param RuleSet \code{data.frame} - Contains predefined set of normalization rules
#' @param RuleSet.Profile \code{string} - Profile name stated in 'RuleSet'
#'
#' @return The transformed input \code{data.frame}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NormalizeTable <- function(DataFrame,
                           TableName,
                           RuleSet,
                           RuleSet.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)
    require(tidyr)

    # --- For testing purposes ---
    # DataFrame <- DataSet$SystemicTherapy
    # TableName <- "SystemicTherapy"
    # RuleSet <- dsCCPhos::Meta_TableNormalization
    # RuleSet.Profile <- "Default"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Temporary security measure: Define permitted functions
    PermittedFunctions <- c("separate_longer")

    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleSet.Profile
                                  & Table == TableName) %>%
                          arrange(EvaluationOrder)

    if (nrow(RelevantRules) == 0)
    {
        return(DataFrame)

    } else {

        vc_Operations <- NULL

        for (i in 1:nrow(RelevantRules))
        {
            FeatureName <- RelevantRules$Feature[i]
            Operation <- RelevantRules$Operation[i]

            # Check if 'Operation' string is available and if it belongs to the set of permitted functions
            if (!is.na(Operation) & any(str_starts(Operation, PermittedFunctions)))
            {
                Operation <- str_replace(Operation, ".Table", ".")

                if (!is.na(FeatureName)) { Operation <- str_replace(Operation, ".Feature", FeatureName) }

                vc_Operations <- c(vc_Operations, Operation)
            }
        }

        Output <- DataFrame

        if (length(vc_Operations) > 0 && !is.null(vc_Operations))
        {
            for (i in 1:length(vc_Operations))
            {
                Output <- Output %>%
                              eval(expr = parse(text = vc_Operations[i]))
            }
        }

        return(Output)
    }
}
