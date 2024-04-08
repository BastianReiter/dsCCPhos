
#' HarmonizeData
#'
#' Harmonizes data based on a given rule set. First, general harmonizing transformations are performed using regular expression based rules. Second, for direct replacement of values hash tables are being compiled and applied.
#'
#' Uses dplyr::mutate() to transform data based on harmonization rules defined in a given rule set (Default: dsCCPhos::RuleSet_RawDataHarmonization)
#'
#' @param DataFrame data frame containing data to be transformed
#' @param TableName String | Name of the table (to enable mapping to harmonization rules)
#' @param RuleSet Data frame | Contains predefined set of harmonization rules
#' @param RuleProfile String | Profile name stated in rule set data frame
#'
#' @return The input data frame with transformed data values
#' @export
#'
#' @examples
#' @author Bastian Reiter
HarmonizeData <- function(DataFrame,
                          TableName,
                          RuleSet,
                          RuleProfile)
{
    require(dplyr)
    require(dsCCPhos)

    # For testing purposes
    # DataFrame <- df_Staging
    # TableName <- "Staging"
    # RuleSet <- dsCCPhos::RuleSet_RawDataHarmonization
    # RuleProfile <- "Default"

    # Compile general harmonization rules with dsCCPhos::CompileHarmonizationRules
    HarmonizationRules <- CompileHarmonizationRules(TableName,
                                                    RuleSet,
                                                    RuleProfile)

    # Compile hash tables for direct value replacement
    HashTables <- CompileHashTables(TableName,
                                    RuleSet,
                                    RuleProfile)

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
