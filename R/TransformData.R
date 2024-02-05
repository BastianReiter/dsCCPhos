
#' TransformData
#'
#' Uses dplyr::mutate() to transform data based on transformation rules defined in dsCCPhos::RuleSet_RawDataTransformation
#'
#' @param TargetVector Vector that recoding is performed on
#' @param Dictionary A named character vector. Vector names are look-up values, vector values are substitute values.
#'
#' @return A vector
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
    # TableName = "BioSampling"
    # RuleSet = dsCCPhos::RuleSet_RawDataTransformation
    # RuleProfile = "Default"

    # Compile rules with dsCCPhos::CompileTransformationRules
    TransformationRules <- CompileTransformationRules(TargetTable = TableName,
                                                      RuleSet,
                                                      RuleProfile)

    if (is.na(TransformationRules))
    {
        return(DataFrame)
    }
    else
    {
        df_Output <- DataFrame %>%
                        eval(expr = parse(text = paste0("mutate(., ", TransformationRules, ")")))

        return(df_Output)
    }
}
