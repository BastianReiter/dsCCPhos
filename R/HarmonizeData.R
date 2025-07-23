
#' HarmonizeData
#'
#' Harmonizes data based on a given rule set. First, general harmonizing transformations are performed using regular expression based rules. Second, for direct replacement of values hash tables are being compiled and applied.
#'
#' Uses dplyr::mutate() to transform data based on harmonization rules defined in a given rule set (Default: dsCCPhos::RuleSet_RawDataHarmonization)
#'
#' @param DataFrame \code{data.frame} containing data to be transformed
#' @param TableName \code{string} - Name of the table (to enable mapping to harmonization rules)
#' @param Methods \code{data.frame}
#' @param RuleSet \code{data.frame} - Contains predefined set of harmonization rules
#' @param RuleSet.Profile \code{string} - Profile name stated in 'RuleSet'
#'
#' @return The input \code{data.frame} with transformed data values
#' @export
#'
#' @author Bastian Reiter
HarmonizeData <- function(DataFrame,
                          TableName,
                          Methods = NULL,
                          RuleSet,
                          RuleSet.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)

    # For testing purposes
    # DataFrame <- df_Staging
    # TableName <- "Staging"
    # RuleSet <- dsCCPhos::RuleSet_RawDataHarmonization
    # RuleSet.Profile <- "Default"

    # Compile general harmonization rules with dsCCPhos::CompileHarmonizationRules
    HarmonizationRules <- CompileHarmonizationRules(TableName,
                                                    RuleSet,
                                                    RuleSet.Profile)

    # Compile hash tables for direct value replacement
    HashTables <- CompileHashTables(TableName,
                                    RuleSet,
                                    RuleSet.Profile)

    if (is.na(HarmonizationRules) & is.null(HashTables))
    {
        return(DataFrame)
    }
    else
    {
        df_Output <- DataFrame

        if (!is.na(HarmonizationRules))
        {
            df_Output <- df_Output %>%
                            eval(expr = parse(text = paste0("mutate(., ", HarmonizationRules, ")")))
        }

        if (!is.null(HashTables))
        {
            for (i in 1:length(HashTables))
            {
                Feature <- names(HashTables)[i]
                HashTable <- HashTables[[Feature]]
                FeatureData <- df_Output[[Feature]]

                FeatureDataTransformed <- ifelse(is.na(HashTable[FeatureData]),
                                                 FeatureData,
                                                 HashTable[FeatureData])

                df_Output[[Feature]] <- FeatureDataTransformed
            }
        }

        return(df_Output)
    }
}
