
#' GetClassificationCall
#'
#' Auxiliary function within \code{\link{ClassifyDiagnosisAssociations}}
#'
#' Based on a predefined rule set data frame, construct an R expression and return it as a string
#'
#' @param TargetFeature String | Name of feature that is being engineered
#' @param PredictorFeatures Vector | Character vector containing the names of all determining / predictor features
#' @param RuleSet Data frame | Contains predefined set of rules
#' @param ValueIfNoRuleMet String | Value (as string) of TargetFeature if no rules are fulfilled | Default = "NA"
#'
#' @return String containing an unevaluated R expression
#' @export
#'
#' @examples
#' @author Bastian Reiter
GetClassificationCall <- function(TargetFeature,
                                  PredictorFeatures,
                                  RuleSet,
                                  ValueIfNoRuleMet = "NA")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    # For testing purposes
    # FeatureName <- "IsLikelyAssociated"
    # PredictorFeatures <- PredictorFeatures_DiagnosisAssociation
    # RuleSet <- Meta_DiagnosisAssociation
    # ValueIfNoRuleMet <- "FALSE"

    RelevantRules <- RuleSet %>%
                          filter(Feature == TargetFeature) %>%
                          arrange(EvaluationOrder)

    vc_Rules <- character()

    for (i in 1:nrow(RelevantRules))
    {

        String <- character()

        vc_Blocks <- character()

        for (j in 1:length(PredictorFeatures))
        {
            RefPointer <- paste0("Reference$", PredictorFeatures[j])      # Ex. 'Reference$ICD10Group' (value of Reference diagnosis)
            CandPointer <- paste0(PredictorFeatures[j], "[row_number()]")      # Ex. 'ICD10Group[row_number()]' (value of Candidate diagnosis)
            ExpressionRef <- RelevantRules[i, paste0(PredictorFeatures[j], "_Ref")]      # Ex. '%in% c(...)'
            ExpressionCand <- RelevantRules[i, paste0(PredictorFeatures[j], "_Cand")]      # Ex. =='Digestive organs'
            ConnectingOperator <- RelevantRules[i, paste0(PredictorFeatures[j], "_Conn")]       # Ex. '&' or '=='

            vc_Blockparts <- NULL

            if (!is.na(ConnectingOperator))
            {
                vc_Blockparts <- c("(",
                                   RefPointer, " ",
                                   ExpressionRef, " ",
                                   ConnectingOperator, " ",
                                   CandPointer, " ",
                                   ExpressionRef,
                                   ")")
            }

            Block <- paste0(vc_Blockparts[!is.na(vc_Blockparts)], collapse = "")
            vc_Blocks <- c(vc_Blocks, Block)
        }

        Rule <- paste0(vc_Blocks, collapse = "")
        Rule <- paste0(Rule, " ~ ", RelevantRules$Value[i], ", " , collapse = "")

        vc_Rules <- c(vc_Rules, Rule)
    }

    Rules <- paste0(vc_Rules, collapse = "")

    CallString <- paste0("case_when(",
                         Rules,
                         "TRUE ~ ", ValueIfNoRuleMet,
                         ")",
                         collapse = "")

    return(CallString)
}
