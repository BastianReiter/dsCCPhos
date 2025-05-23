
#' SummarizeEventData
#'
#' Auxiliary function within \code{\link{AugmentDataDS()}}
#'
#' Summarize data stored in \code{ADS$Events} and create meaningful features for \code{ADS$Diagnosis} and \code{ADS$Patient}
#'
#' @param EventData \code{data.frame} that contains entries from \code{ADS$Events}
#' @param ProgressBarObject Optionally pass object of type \code{progress::progress_bar} to display progress
#'
#' @return \code{data.frame} containing summarized information
#' @export
#'
#' @author Bastian Reiter
SummarizeEventData <- function(EventData,
                               ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)


    ### For function testing purposes
    # EventData <- df_ADS_Events %>%
    #                     filter(PatientID == "Pat_2063") %>%
    #                     unnest(cols = c(EventDetails), keep_empty = TRUE)


    # Update progress bar object, if assigned in function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(ProgressBarObject)) { try(ProgressBarObject$tick()) }


    Output <- EventData %>%
                  summarize(PatientAgeAtDiagnosis = first(EventPatientAge[EventSubclass == "InitialDiagnosis"]),      # !!! TEMPORARY? !!! If multiple "InitialDiagnosis" entries occur (Which shouldn't be), select only the first
                            #---------------------------------------------------
                            TimeDiagnosisToDeath = ifelse("Deceased" %in% EventData$EventSubclass,
                                                          EventDaysSinceDiagnosis[EventSubclass == "Deceased"],
                                                          NA_integer_),
                            TimeFollowUp = case_when(!is.na(TimeDiagnosisToDeath) ~ TimeDiagnosisToDeath,
                                                     !all(is.na(EventDaysSinceDiagnosis)) ~ max(EventDaysSinceDiagnosis, na.rm = TRUE),
                                                     TRUE ~ NA_integer_),
                            IsDocumentedDeceased = case_when(!is.na(TimeDiagnosisToDeath) ~ TRUE,
                                                             TRUE ~ FALSE))
                            #---------------------------------------------------
                            #HadChemotherapy = )


    return(as.data.frame(Output))
}
