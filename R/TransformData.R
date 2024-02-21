
#' TransformData
#'
#' Transforms data based on a given rule set. First, general transformations are performed using regular expression based rules. Second, for direct replacement of values hash tables are being compiled and applied.
#'
#' Uses dplyr::mutate() to transform data based on transformation rules defined in a given rule set (Default: dsCCPhos::RuleSet_RawDataTransformation)
#'
#' @param DataFrame data frame containing data to be transformed
#' @param TableName String | Name of the table (to enable mapping to transformation rules)
#' @param RuleSet Data frame | Contains predefined set of transformation rules
#' @param RuleProfile String | Profile name stated in rule set data frame
#'
#' @return The input data frame with transformed data values
#' @export
#'
#' @examples
#' @author Bastian Reiter
TransformData <- function(DataFrame,
                          TableName,
                          RuleSet,
                          RuleProfile)
{
    require(dplyr)
    require(dsCCPhos)

    # For testing purposes
    # DataFrame <- df_Staging
    # TableName <- "Staging"
    # RuleSet <- dsCCPhos::RuleSet_RawDataTransformation
    # RuleProfile <- "Default"

    # Compile general transformation rules with dsCCPhos::CompileTransformationRules
    TransformationRules <- CompileTransformationRules(TableName,
                                                      RuleSet,
                                                      RuleProfile)

    # Compile hash tables for direct value transformation
    HashTables <- CompileHashTables(TableName,
                                    RuleSet,
                                    RuleProfile)

    if (is.na(TransformationRules) & is.null(HashTables))
    {
        return(DataFrame)
    }
    else
    {
        df_Output <- DataFrame

        if (!is.na(TransformationRules))
        {
            df_Output <- df_Output %>%
                            eval(expr = parse(text = paste0("mutate(., ", TransformationRules, ")")))
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
