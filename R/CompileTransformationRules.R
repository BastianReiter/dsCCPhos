
#' CompileTransformationRules
#'
#' Auxiliary function within \code{\link{CurateDataDS}} or \code{\link{TransformData}} respectively
#'
#' Based on a predefined rule set data frame, compile argument expressions for dplyr::mutate()
#'
#' @param TargetTable String | Name of table that contains features to be transformed
#' @param RuleSet Data frame | Contains predefined set of transformation rules
#' @param RuleProfile String | Profile name stated in rule set data frame
#'
#' @return String containing arguments for dplyr::mutate()
#' @export
#'
#' @examples
#' @author Bastian Reiter
CompileTransformationRules <- function(TargetTable,
                                       RuleSet,
                                       RuleProfile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    # For testing purposes
    # TargetTable <- "Histology"
    # RuleSet <- dsCCPhos::RuleSet_RawDataTransformation
    # RuleProfile <- "Default"


    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleProfile & Table == TargetTable) %>%
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
                Operation <- str_replace(Operation, ".REP", paste0("c('", RelevantRules$REP_OriginalValue[i], "' = '", RelevantRules$REP_NewValue[i], "')"))
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
