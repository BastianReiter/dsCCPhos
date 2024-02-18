
#' GetSampleStatisticsDS
#'
#' Calculate common sample statistics for a feature of a data frame
#'
#' @param TableName String | Name of the Data frame that contains the metric feature
#' @param MetricFeatureName String | Name of metric feature
#' @param GroupingFeatureName String | Name of optional grouping feature
#' @param na.rm Logical | Whether NA values should be removed before calculating statistics
#'
#' @return A tibble containing the following statistics: sample size (N), minimum, maximum, Q1, Median, Q3, MAD, mean, standard deviation
#' @export
#'
#' @examples
GetSampleStatisticsDS <- function(TableName,
                                  MetricFeatureName,
                                  GroupingFeatureName = NULL,
                                  na.rm = FALSE)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check, evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName)
      & is.character(MetricFeatureName)
      & (is.null(GroupingFeatureName) | (!is.null(GroupingFeatureName) & is.character(GroupingFeatureName))))
{
    Table <- eval(parse(text = TableName), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'TableName', 'MetricFeatureName' and 'GroupingFeatureName' (optionally) must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(rlang)
require(stats)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function proceedings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For function testing purposes
# Table <- AugmentationOutput$Patients
# MetricFeatureName <- "PatientAgeAtDiagnosis"
# GroupingFeatureName <- "LastVitalStatus"
# na.rm = TRUE


df_Output <- Table %>%
                  summarize(N = n(),
                            q5 = quantile(.data[[MetricFeatureName]], probs = 0.05, na.rm = na.rm),
                            Q1 = quantile(.data[[MetricFeatureName]], probs = 0.25, na.rm = na.rm),
                            Median = median(.data[[MetricFeatureName]], na.rm = na.rm),
                            Q3 = quantile(.data[[MetricFeatureName]], probs = 0.75, na.rm = na.rm),
                            q95 = quantile(.data[[MetricFeatureName]], probs = 0.95, na.rm = na.rm),
                            MAD = mad(.data[[MetricFeatureName]], na.rm = na.rm),
                            Mean = mean(.data[[MetricFeatureName]], na.rm = na.rm),
                            SD = sd(.data[[MetricFeatureName]], na.rm = na.rm),
                            SEM = SD / sqrt(N))


if (!is.null(GroupingFeatureName))      # If GroupingFeatureName is not empty...
{
    # Get group-specific output
    df_Groupwise <- Table %>%
                        group_by(., .data[[GroupingFeatureName]]) %>%
                        summarize(N = n(),
                                  q5 = quantile(.data[[MetricFeatureName]], probs = 0.05, na.rm = na.rm),
                                  Q1 = quantile(.data[[MetricFeatureName]], probs = 0.25, na.rm = na.rm),
                                  Median = median(.data[[MetricFeatureName]], na.rm = na.rm),
                                  Q3 = quantile(.data[[MetricFeatureName]], probs = 0.75, na.rm = na.rm),
                                  q95 = quantile(.data[[MetricFeatureName]], probs = 0.95, na.rm = na.rm),
                                  MAD = mad(.data[[MetricFeatureName]], na.rm = na.rm),
                                  Mean = mean(.data[[MetricFeatureName]], na.rm = na.rm),
                                  SD = sd(.data[[MetricFeatureName]], na.rm = na.rm),
                                  SEM = SD / sqrt(N))

    # Create Extra column for later rbinding
    df_Output <- df_Output %>%
                      mutate(dummy = "All", .before = 1)

    # Harmonize column names for rbinding
    colnames(df_Output) <- colnames(df_Groupwise)

    df_Output <- rbind(df_Output,
                       df_Groupwise)
}

return(df_Output)
}


