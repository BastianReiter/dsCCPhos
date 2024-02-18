
#' TrackFeatureValues
#'
#' Track occurrence of a feature's unique values, their eligibility and frequency
#'
#' @param DataFrame A data frame
#' @param Features A list of vectors. Name of vector object gives name of feature, optional vector values give eligible values.
#' @param CurationStage String representing curation stage (usually "Raw" / "After Transformation" / "After Exclusion")
#'
#' @return A tibble
#' @export
#'
#' @examples
#' @author Bastian Reiter
TrackFeatureValues <- function(DataFrame,
                               Features,
                               CurationStage = NULL)
{
    require(dplyr)

    # For testing purposes
    # DataFrame <- df_CDS_Diagnosis
    # Features <- ls_MonitorFeatures_All$Diagnosis
    # CurationStage <- "Final"

    df_Output <- tibble(Feature = character(),
                        Value = character(),
                        IsValueEligible = logical(),
                        CurationStage = character(),
                        Frequency = numeric())

    for (feature in names(Features))
    {
        vc_ContingencyTable <- table(DataFrame[[feature]], useNA = "always")      # table() returns a contingency table in the form of a named vector. Vector element names are the occurring values, vector element values are the corresponding absolute frequencies.

        vc_EligibleValues <- Features[[feature]]      # Get eligible values from optional vector values

        df_FeatureRows <- tibble::tibble(Feature = feature,
                                         Value = names(vc_ContingencyTable),      # Get all distinct values from contingency table
                                         IsValueEligible = NA,
                                         CurationStage = CurationStage,
                                         Frequency = as.numeric(vc_ContingencyTable)) %>%      # Get absolute frequencies from contingency table
                                      arrange(Value)

        if (!is.null(vc_EligibleValues))      # If vector of eligible values is passed (so not null),
        {
            df_FeatureRows <- df_FeatureRows %>%
                                  mutate(IsValueEligible = Value %in% vc_EligibleValues) %>%      # ... see which values are in it
                                  arrange(IsValueEligible, Value)
        }

        df_Output <- bind_rows(df_Output, df_FeatureRows)
    }

    return(df_Output)
}
