
#' GetSurvModelDS
#'
#' Return Kaplan-Meier curve
#'
#' @param TableName.S \code{string} | Name of the data frame that holds time and event features
#' @param TimeFeature.S \code{string} | Name of time feature
#' @param EventFeature.S \code{string} | Name of event feature
#' @param Covariates.S \code{character vector}
#'
#' @return A \code{survival curve object}
#' @export
GetSurvModelDS <- function(TableName.S,
                           TimeFeature.S,
                           EventFeature.S,
                           Covariates.S = NULL)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check, evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName.S)
      & is.character(TimeFeature.S)
      & is.character(EventFeature.S))
{
    Table <- eval(parse(text = TableName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "Error: 'TableName.S', 'TimeFeature.S' and 'EventFeature.S' must be specified as character strings."
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(survival)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function proceedings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For function testing purposes
# Table <- ADS$Patients
# FeatureName.S <- "TNM_T"
# GroupingFeatureName.S <- "LastVitalStatus"


Data <- Table %>%
            select(PatientID,
                   UICCStage,
                   PatientAgeAtDiagnosis,
                   TimeFollowUp,
                   IsDocumentedDeceased)

SurvObject <- Surv(time = Data$TimeFollowUp,
                   event = Data$IsDocumentedDeceased,
                   type = "right")

Curve <- survfit(SurvObject ~ 1, data = Data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(Curve)
}


