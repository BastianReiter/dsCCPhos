
#' CompileTransFeatureRules
#'
#' Auxiliary function to compile unevaluated R expression from trans-feature obligation rules stated as pseudo-code
#'
#' Based on a predefined rule set data frame, compile a dplyr::case_when() expression and return it as a string
#'
#' @param PseudoCodeRules \code{character vector} containing pseudo-code rules about feature obligations
#'
#' @return \code{character vector} containing unevaluated expressions to be used in dplyr::mutate() calls
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CompileTransFeatureRules <- function(PseudoCodeRules)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    # For testing purposes
    # PseudoCodeRules <- "MIN-1-FROM(GlobalStatus,PrimarySiteStatus,LymphnodalStatus,MetastasisStatus)"


    vc_Rules <- NULL

    for (i in 1:length(PseudoCodeRules))
    {
        # Clean Condition string of '\r\n' string induced by line breaks in excel
        PseudoCode <- str_replace_all(PseudoCodeRules[i], "\r\n", " ")

        if (str_starts(PseudoCode, "MIN-"))
        {
            NumberOfNonMissingFeatures <- str_split(PseudoCode, "-FROM")[[1]][1] %>%
                                              str_extract(pattern = "\\d+") %>%
                                              as.numeric()

            TargetFeatures <- str_extract(PseudoCode, "(?<=FROM\\()[^)]+") %>%
                                  str_split_1(., pattern = ",")

            Rule <- paste0("rowSums(!is.na(across(c(", paste0(TargetFeatures, collapse = ", "), ")))) >= ", NumberOfNonMissingFeatures)
        }

        # Add current rule to set of rules in a vector
        vc_Rules <- c(vc_Rules, Rule)
    }

    return(vc_Rules)
}
