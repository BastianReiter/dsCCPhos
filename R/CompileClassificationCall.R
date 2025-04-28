
#' CompileClassificationCall
#'
#' Auxiliary function to compile calls used in \code{\link{ClassifyDiagnosisAssociation}}
#'
#' Based on a predefined rule set data frame, compile a dplyr::case_when() expression and return it as a string
#'
#' @param TargetFeature \code{string} - Name of feature that is being engineered
#' @param RuleSet \code{data.frame} - Contains predefined set of rules
#' @param RuleProfile \code{string} - Profile name stated in rule set data frame
#' @param ValueIfNoRuleMet \code{string} - Value (as string) of TargetFeature if no rules are fulfilled | Default = "NA"
#'
#' @return \code{string} containing an unevaluated dplyr::case_when() expression
#' @export
#'
#' @author Bastian Reiter
CompileClassificationCall <- function(TargetFeature,
                                      RuleSet,
                                      RuleProfile,
                                      ValueIfNoRuleMet = NA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    # For testing purposes
    # TargetFeature <- "IsLikelyRedundant"
    # RuleSet <- RuleSet_DiagnosisRedundancy
    # RuleProfile <- RuleProfile_DiagnosisRedundancy.S
    # ValueIfNoRuleMet <- FALSE


    # Filter relevant rules from given rule set
    RelevantRules <- RuleSet %>%
                          filter(Profile == RuleProfile,
                                 Feature == TargetFeature) %>%
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
        # Clean Condition string of '\r\n' string induced by line breaks in excel
        Condition <- str_replace_all(RelevantRules$Condition[i], "\r\n", " ")

        # Using regular expressions, compile compatible code for 'Reference' ...
        Condition <- str_replace_all(string = Condition,
                                     pattern = "<#Ref:(.*?)#>",
                                     replacement = paste0("Reference$", "\\1"))      # Ex. '<#Ref:ICD10Code#>' --> 'Reference$ICD10Code'

        # ... and for 'Candidate' values
        Condition <- str_replace_all(string = Condition,
                                     pattern = "<#Cand:(.*?)#>",
                                     replacement = paste0("\\1", "[row_number()]"))      # Ex. '<#Cand:ICD10Code#>' --> 'ICD10Code[row_number()]'

        # Map Conditions with resulting value for target feature
        Rule <- paste0(Condition, " ~ ", RelevantRules$Value[i], ", " , collapse = "")

        # Add current rule to set of rules in a vector
        vc_Rules <- c(vc_Rules, Rule)
    }

    # Concatenate rules from rule vector
    Rules <- paste0(vc_Rules, collapse = "")

    # Compile complete 'case_when()' call
    CallString <- paste0("case_when(",
                         Rules,
                         "TRUE ~ ", ValueIfNoRuleMet,
                         ")",
                         collapse = "")

    return(CallString)
}
