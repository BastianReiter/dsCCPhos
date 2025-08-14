
#' FinalizeDataTransformation
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Ineligible data (including data that could not be transformed) is turned into NA per default.
#' Optional factor conversion establishes level order where appropriate.
#' Value eligibility and factor information is defined in dsCCPhos::Meta_ValueSets.
#'
#' @param TargetVector \code{vector} - Feature to be finalized
#' @param EligibleValueSet \code{data.frame} - Containing set of eligible values
#' @param ExcludeIneligibleValues \code{logical} - Whether to set all ineligible values NA | Default: TRUE
#' @param ConvertToFactor \code{logical} - Whether to convert vector to factor | Default: FALSE
#' @param AssignFactorLabels \code{logical} - Whether to assign factor labels during factor conversion | Default: FALSE
#'
#' @return A \code{vector} (or factor)
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FinalizeDataTransformation <- function(TargetVector,
                                       EligibleValueSet,
                                       ExcludeIneligibleValues = TRUE,
                                       ConvertToFactor = FALSE,
                                       AssignFactorLabels = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)

  # --- For Testing Purposes ---
  # TargetVector <- DataSet$RadiationTherapy$ApplicationType
  # EligibleValueSet
  # ExcludeIneligibleValues = TRUE
  # ConvertToFactor = FALSE
  # AssignFactorLabels = FALSE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  EligibleValueSet <- EligibleValueSet %>%
                          arrange(FactorRank)

  vc_EligibleValues <- EligibleValueSet$Value_Curated
  vc_EligibleValueLabels <- EligibleValueSet$Label_Curated

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
