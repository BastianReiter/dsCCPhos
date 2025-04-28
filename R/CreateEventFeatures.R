
#' CreateEventFeatures
#'
#' Auxiliary function within \code{\link{AugmentDataDS}}
#'
#' Enhance event data entries with informative features implementing feature engineering rules defined in meta data
#'
#' @param EventData \code{data.frame} - Usually ADS$Events
#' @param RuleSet \code{data.frame} - Set of rules for feature engineering - Default: \code{\link{dsCCPhos::Meta_EventFeatures}}
#' @param Profile \code{character} - Profile name defining rule set to be used for event feature engineering. Profile name must be stated in \code{RuleSet} - Default: 'Default'
#' @param ProgressBarObject \code{progress::progress_bar} object - Optionally pass progress bar object to display progress
#'
#' @return \code{data.frame} EventData enhanced with new features
#' @export
#'
#' @author Bastian Reiter
CreateEventFeatures <- function(EventData,
                                RuleSet,
                                Profile,
                                ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    ### For function testing purposes
    # EventData <- ADS$Events %>% unnest(cols = c(EventDetails), keep_empty = TRUE)
    # RuleSet <- dsCCPhos::Meta_EventFeatures
    # RuleProfile = "Default"


    # Update progress bar object, if assigned in function call
    if (!is.null(ProgressBarObject)) { try(ProgressBarObject$tick()) }


    NewFeatures <- RuleSet %>%
                        filter(Profile == Profile) %>%
                        pull(Feature) %>%
                        unique()

    if (length(NewFeatures) > 0)
    {
        for (i in 1:length(NewFeatures))
        {
            CurrentFeature <- NewFeatures[i]

            CurrentFeatureRules <- RuleSet %>%
                                        filter(Profile == Profile,
                                               Feature == CurrentFeature) %>%
                                        arrange(EvaluationOrder)

            vc_Rules <- NULL

            for (j in 1:nrow(CurrentFeatureRules))
            {
                Condition <- CurrentFeatureRules$Condition[j]

                if (!(is.na(Condition) | Condition == ""))
                {
                    # Clean Condition string of '\r\n' string induced by line breaks in excel
                    Condition <- str_replace_all(CurrentFeatureRules$Condition[j], "\r\n", " ")

                    # Map Conditions with resulting value for target feature
                    Rule <- paste0(Condition, " ~ ", CurrentFeatureRules$Value[j], ", " , collapse = "")

                    # Add current rule to set of rules in a vector
                    vc_Rules <- c(vc_Rules, Rule)
                }
            }

            # Concatenate rules from rule vector
            Rules <- paste0(vc_Rules, collapse = "")

            # Compile complete 'case_when()' call
            CallString <- paste0("case_when(",
                                 Rules,
                                 "TRUE ~ NA)",
                                 collapse = "")

            # Create new feature
            EventData <- EventData %>%
                              mutate(!!CurrentFeature := eval(parse(text = CallString)))
        }
    }

    return(EventData)
}
