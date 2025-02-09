
#' AugmentEventData
#'
#' Auxiliary function within \code{\link{AugmentDataDS}}
#'
#' Enhance event data entries with informative features implementing feature engineering rules defined in meta data
#'
#' @param EventData \code{data.frame} - Usually ADS$Events
#' @param
#' @param ProgressBarObject \code{progress::progress_bar} object - Optionally pass progress bar object to display progress
#'
#' @return \code{data.frame} EventData enhanced with new features
#' @export
#'
#' @author Bastian Reiter
AugmentEventData <- function(EventData,
                             RuleCalls,
                             ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)
    require(tidyr)


    # For function testing purposes
    # DiagnosisEntries <- df_Diagnosis %>% filter(PatientID == "Pat_10010")
    # RuleCalls <- RuleCalls_DiagnosisAssociation
    # RulesProfile = "Default"
    # print(DiagnosisEntries$DiagnosisID[1])


    # Update progress bar object, if assigned in function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(ProgressBarObject)) { try(ProgressBarObject$tick()) }





    return(as.data.frame(Output))
}
