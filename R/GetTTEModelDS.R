
#' GetTTEModelDS
#'
#' Return Time-to-Event model implemented in the \code{survival} package
#'
#' @param TableName.S \code{string} - Name of the data frame that holds time and event features
#' @param TimeFeature.S \code{string} - Name of time feature
#' @param EventFeature.S \code{string} - Name of event feature
#' @param ModelType.S \code{string} - Function name of different TTE models implemented in \code{survival} package:
#'                                    \itemize{\item 'survfit'
#'                                             \item 'survdiff'
#'                                             \item 'coxph'}
#' @param CovariateA.S \code{string} - Name of optional Covariate A
#' @param CovariateB.S \code{string} - Name of optional Covariate B
#' @param CovariateC.S \code{string} - Name of optional Covariate C
#' @param MinFollowUpTime.S \code{integer} - Optional minimum of observed follow up time
#'
#' @return A Time-to-Event model object
#' @export
GetTTEModelDS <- function(TableName.S,
                          TimeFeature.S,
                          EventFeature.S,
                          ModelType.S = "survfit",
                          CovariateA.S = NULL,
                          CovariateB.S = NULL,
                          CovariateC.S = NULL,
                          MinFollowUpTime.S = 1)
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
# TimeFeature.S <- "TimeFollowUp"
# EventFeature.S <- "IsDocumentedDeceased"
# CovariateA.S <- "UICCStage"
# CovariateB.S <- "PatientAgeAtDiagnosis"
# CovariateC.S <- NULL
# MinFollowUpTime <- 10


# Initiate Messaging object
Messages <- list()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparing data used for model fit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Construct data used for model fit
Data <- Table %>%
            select({{ TimeFeature.S }},
                   {{ EventFeature.S }},
                   {{ CovariateA.S }},
                   {{ CovariateB.S }},
                   {{ CovariateC.S }}) %>%
            rename(Time = {{ TimeFeature.S }},
                   Event = {{ EventFeature.S }},
                   CovariateA = {{ CovariateA.S }},
                   CovariateB = {{ CovariateB.S }},
                   CovariateC = {{ CovariateC.S }})

# How many rows are in the 'raw' data
AvailableRows <- nrow(Data)

# Filtering data
Data <- Data %>%
            filter(!is.na(Time) & !is.na(Event) & Time > 0) %>%      # Filter out invalid data
            filter(Time >= MinFollowUpTime.S)      # Optionally filter for a minimum of observed follow up time

# How many rows remain after filtering
EligibleRows <- nrow(Data)

# How many rows were dropped
Messages$DroppedRows <- AvailableRows - EligibleRows


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating Surv object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using survival::Surv() to create Surv object
SurvObject <- with(Data, Surv(time = Time,
                              event = Event,
                              type = "right"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model Fit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ModelFormulaString <- "SurvObject ~ 1"

if ("CovariateA" %in% names(Data)) { ModelFormulaString <- "SurvObject ~ CovariateA" }
if (all(c("CovariateA", "CovariateB") %in% names(Data))) { ModelFormulaString <- "SurvObject ~ CovariateA + CovariateB" }
if (all(c("CovariateA", "CovariateB", "CovariateC") %in% names(Data))) { ModelFormulaString <- "SurvObject ~ CovariateA + CovariateB + CovariateC" }

Model <- NULL
if (ModelType.S == "survfit") { Model <- survfit(formula(ModelFormulaString), data = Data) }
if (ModelType.S == "survdiff") { Model <- survdiff(formula(ModelFormulaString), data = Data) }
if (ModelType.S == "coxph") { Model <- coxph(formula(ModelFormulaString), data = Data) }


# library(ggsurvfit)
#
# Plot <- Model %>%
#             ggsurvfit() +
#             xlim(0, 5 * 365) +
#             labs(x = "Days",
#                  y = "Overall survival probability") +
#             add_confidence_interval()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(Model)
}


