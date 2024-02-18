
#' SummarizeEventData
#'
#' Auxiliary function within \code{\link{AugmentDataDS}}
#'
#' Summarize data stored in ADS_Events and create meaningful features for ADS_Diagnosis and ADS_Patient
#'
#' @param EventEntries Data frame that contains entries from unnested ADS_Events
#' @param ProgressBarObject Optionally pass object of type progress::progress_bar to display progress
#'
#' @return Data frame containing summarized information
#' @export
#'
#' @examples
#' @author Bastian Reiter
SummarizeEventData <- function(EventEntries,
                               ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)


    # For function testing purposes
    # EventEntries <- df_ADS_Events %>%
    #                     filter(PatientID == "Pat_105431") %>%
    #                     unnest(cols = c(EventDetails), keep_empty = TRUE)



    # Update progress bar object, if assigned in function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(ProgressBarObject)) { ProgressBarObject$tick() }



    Output <- EventEntries %>%
                  summarize(PatientAgeAtDiagnosis = first(EventPatientAge[EventSubclass == "Initial diagnosis"]),      # !!! TEMPORARY? !!! If multiple "Initial diagnoses" occur (Which shouldn't be), select only the first
                            #---------------------------------------------------
                            TimeDiagnosisToDeath = ifelse("Death" %in% EventEntries$EventSubclass,
                                                          EventDaysSinceDiagnosis[EventSubclass == "Death"],
                                                          NA))


    return(as.data.frame(Output))
}
