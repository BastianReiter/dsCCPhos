
#' FinalizeDataTransformation
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Ineligible data (including data that could not be transformed) is optionally turned into NA.
#' Optional factor conversion establishes level order where appropriate.
#' Value eligibility and factor information is defined in dsCCPhos::Meta_ValueSets.
#'
#' @param TargetVector Vector | Feature to be finalized
#' @param TableName String | For look-up purposes: Name of parenting table within Curated Data Set (CDS)
#' @param FeatureName String | For look-up purposes: Name of the feature to be finalized
#' @param ExcludeIneligibleValues Logical | Whether to set all ineligible values NA | Default: TRUE
#' @param ConvertToFactor Logical | Whether to convert vector to factor | Default: FALSE
#' @param AssignFactorLabels Logical | Whether to assign factor labels during factor conversion | Default: FALSE
#'
#' @return A vector (or factor)
#' @export
#'
#' @author Bastian Reiter
FinalizeDataTransformation <- function(TargetVector,
                                       TableName,
                                       FeatureName,
                                       ExcludeIneligibleValues = TRUE,
                                       ConvertToFactor = FALSE,
                                       AssignFactorLabels = FALSE)
{
    require(dplyr)

    # For testing purposes
    # TargetVector <- df_Progress$GlobalStatus
    # TableName = "Progress"
    # FeatureName = "GlobalStatus"
    # ExcludeIneligibleValues = TRUE
    # ConvertToFactor = FALSE
    # AssignFactorLabels = FALSE


    RelevantValueSet <- dsCCPhos::Meta_ValueSets %>%
                            filter(Table == TableName & Feature == FeatureName) %>%
                            arrange(FactorRank)

    vc_EligibleValues <- RelevantValueSet$Value_Curated
    vc_EligibleValueLabels <- RelevantValueSet$Label_Curated

    if (AssignFactorLabels == TRUE) { ConvertToFactor <- TRUE }

    if (ExcludeIneligibleValues == TRUE)
    {
        vc_IsEligible <- TargetVector %in% vc_EligibleValues
        vc_IsEligible[vc_IsEligible == FALSE] <- NA      # Replace all "FALSE" entries with NA

        TargetVector <- TargetVector[vc_IsEligible]
    }

    if (ConvertToFactor == TRUE)
    {
        TargetVector <- factor(TargetVector,
                               levels = vc_EligibleValues,
                               labels = ifelse(AssignFactorLabels == TRUE,
                                               vc_EligibleValueLabels,
                                               vc_EligibleValues))
    }

    return(TargetVector)
}
