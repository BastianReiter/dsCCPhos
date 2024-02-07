
#' CompileClassificationCall
#'
#' Auxiliary function within \code{\link{ClassifyDiagnosisAssociations}}
#'
#' Based on a predefined rule set data frame, compile a dplyr::case_when() expression and return it as a string
#'
#' @param TargetFeature String | Name of feature that is being engineered
#' @param PredictorFeatures Vector | Character vector containing the names of all determining / predictor features
#' @param RuleSet Data frame | Contains predefined set of rules
#' @param RuleProfile String | Profile name stated in rule set data frame
#' @param ValueIfNoRuleMet String | Value (as string) of TargetFeature if no rules are fulfilled | Default = "NA"
#'
#' @return String containing an unevaluated dplyr::case_when() expression
#' @export
#'
#' @examples
#' @author Bastian Reiter
CompileClassificationCall <- function(TargetFeature,
                                      PredictorFeatures,
                                      RuleSet,
                                      RuleProfile,
                                      ValueIfNoRuleMet = NA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    # For testing purposes
    # TargetFeature <- "Relation_ICD10"
    # PredictorFeatures <- PredictorFeatures_DiagnosisAssociation
    # RuleSet <- RuleSet_DiagnosisAssociation
    # ValueIfNoRuleMet <- NA_character_


    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleProfile & Feature == TargetFeature) %>%
                          arrange(EvaluationOrder)

    if (is.null(ValueIfNoRuleMet))
    {
        stop("ERROR: 'ValueIfNoRuleMet' can not be NULL. NA would be a valid alternative.")
    }

    # Process ValueIfNoRuleMet to turn it into valid string
    ValueIfNoRuleMet <- case_when(is.character(ValueIfNoRuleMet) ~ paste0("'", ValueIfNoRuleMet, "'"),
                                  is.na(ValueIfNoRuleMet) ~ "NA",
                                  TRUE ~ as.character(ValueIfNoRuleMet))

    vc_Rules <- NULL

    for (i in 1:nrow(RelevantRules))
    {
        vc_Blocks <- NULL

        for (j in 1:length(PredictorFeatures))
        {
            vc_Blockparts <- NULL

            RefPointer <- paste0("Reference$", PredictorFeatures[j])      # Ex. 'Reference$ICD10Group' (value of Reference diagnosis)
            RefFunction <- RelevantRules[i, paste0(PredictorFeatures[j], "_Ref_f")]      # Ex. is.na(x)
            RefExpression <- RelevantRules[i, paste0(PredictorFeatures[j], "_Ref_expr")]      # Ex. '%in% c(...)'
            ConnectingOperator <- RelevantRules[i, paste0(PredictorFeatures[j], "_Conn")]       # Ex. '&' or '=='
            CandPointer <- paste0(PredictorFeatures[j], "[row_number()]")      # Ex. 'ICD10Group[row_number()]' (value of Candidate diagnosis)
            CandFunction <- RelevantRules[i, paste0(PredictorFeatures[j], "_Cand_f")]      # Ex. is.na(x)
            CandExpression <- RelevantRules[i, paste0(PredictorFeatures[j], "_Cand_expr")]       # Ex. =='Digestive organs'

            # Check if any rule entries exist in Block of current PredictorFeature
            if ((is.na(RefFunction)
                  & is.na(RefExpression)
                  & is.na(ConnectingOperator)
                  & is.na(CandFunction)
                  & is.na(CandExpression)) == FALSE)
            {
                # If there is no ConnectingOperator and neither a function nor an expression for RefPointer is defined, make RefPointer empty string
                if (is.na(ConnectingOperator) & is.na(RefFunction) & is.na(RefExpression))
                {
                    RefPointer <- ""
                }
                # If there is no ConnectingOperator and neither a function nor an expression for CandPointer is defined, make CandPointer empty string
                if (is.na(ConnectingOperator) & is.na(CandFunction) & is.na(CandExpression))
                {
                    CandPointer <- ""
                }

                # If a function on RefPointer is defined, compile accordingly
                if (!is.na(RefFunction))
                {
                    RefPointer <- paste0("(", RefPointer, ")")
                    RefPointer <- str_replace(RefFunction, "(x)", RefPointer)
                }
                # If a function on CandPointer is defined, compile accordingly
                if (!is.na(RefFunction))
                {
                    CandPointer <- paste0("(", CandPointer, ")")
                    CandPointer <- str_replace(CandFunction, "(x)", CandPointer)
                }

                # Concatenate all parts in a vector
                vc_Blockparts <- c("(",
                                   RefPointer, " ",
                                   RefExpression, " ",
                                   ConnectingOperator, " ",
                                   CandPointer, " ",
                                   CandExpression,
                                   ")")
            }

            # Sort out Block parts that are NA and consolidate in a single string
            Block <- paste0(vc_Blockparts[!is.na(vc_Blockparts)], collapse = "")

            # Add Block string to other Blocks
            vc_Blocks <- c(vc_Blocks, Block)
        }

        vc_Blocks <- vc_Blocks[vc_Blocks != ""]      # Remove all empty Blocks from vc_Blocks
        BlockConnector <- paste0(" ", RelevantRules$BlockConnector[i], " ")      # Get operator symbol connecting Blocks for current rule chain (usually '&' or '|')
        Rule <- paste0(vc_Blocks, collapse = BlockConnector)      # If there are multiple Blocks, connect them with BlockConnector operator
        Rule <- paste0(Rule, " ~ ", RelevantRules$Value[i], ", " , collapse = "")      # Map Rule Blocks with resulting value for target feature

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
