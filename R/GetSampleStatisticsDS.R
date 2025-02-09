
#' GetSampleStatisticsDS
#'
#' Calculate common sample statistics for a feature of a data frame.
#'
#' @param TableName.S \code{string} | Name of the Data frame that contains the feature
#' @param MetricFeatureName.S \code{string} | Name of feature
#' @param GroupingFeatureName.S \code{string} | Name of optional grouping feature
#' @param RemoveMissings.S \code{logical} | With numeric features; Whether NA values should be removed before calculating statistics
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#' @author Bastian Reiter
GetSampleStatisticsDS <- function(TableName.S,
                                  MetricFeatureName.S,
                                  GroupingFeatureName.S = NULL,
                                  RemoveMissings.S = TRUE)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check, evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName.S)
      & is.character(MetricFeatureName.S)
      & (is.null(GroupingFeatureName.S) | (!is.null(GroupingFeatureName.S) & is.character(GroupingFeatureName.S))))
{
    Table <- eval(parse(text = TableName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "Error: 'TableName.S', 'MetricFeatureName.S' and (optionally) 'GroupingFeatureName.S' must be specified as a character string"
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
# Table <- ADS$Patients
# MetricFeatureName.S <- "TNM_T"
# GroupingFeatureName.S <- "LastVitalStatus"
# RemoveMissings.S = TRUE


# Evaluate feature in question
Feature <- Table[[MetricFeatureName.S]]

# Stop if Feature is not of class 'numeric'
if (!(class(Feature) %in% c("double", "integer", "numeric"))) { stop(paste0("The specified feature '", MetricFeatureName.S, "' is of class '", class(Feature), "' and therefore not suitable."), call. = FALSE) }


# Get count of valid (non-missing) values in Feature
N_Valid <- sum(!is.na(Feature))

if (N_Valid > 0)
{
    # Calculate parametric and non-parametric sample statistics
    Statistics <- as_tibble(Table) %>%
                      summarize(N = N_Valid,
                                q5 = quantile(.data[[MetricFeatureName.S]], probs = 0.05, na.rm = RemoveMissings.S),
                                Q1 = quantile(.data[[MetricFeatureName.S]], probs = 0.25, na.rm = RemoveMissings.S),
                                Median = median(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                Q3 = quantile(.data[[MetricFeatureName.S]], probs = 0.75, na.rm = RemoveMissings.S),
                                q95 = quantile(.data[[MetricFeatureName.S]], probs = 0.95, na.rm = RemoveMissings.S),
                                MAD = mad(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                Mean = mean(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                SD = sd(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                SEM = SD / sqrt(N))

    # If GroupingFeatureName.S is passed...
    if (!is.null(GroupingFeatureName.S))
    {
        # Get group-specific output
        df_Groupwise <- as_tibble(Table) %>%
                            group_by(., .data[[GroupingFeatureName.S]]) %>%
                            summarize(q5 = quantile(.data[[MetricFeatureName.S]], probs = 0.05, na.rm = RemoveMissings.S),
                                      Q1 = quantile(.data[[MetricFeatureName.S]], probs = 0.25, na.rm = RemoveMissings.S),
                                      Median = median(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      Q3 = quantile(.data[[MetricFeatureName.S]], probs = 0.75, na.rm = RemoveMissings.S),
                                      q95 = quantile(.data[[MetricFeatureName.S]], probs = 0.95, na.rm = RemoveMissings.S),
                                      MAD = mad(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      Mean = mean(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      SD = sd(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      SEM = SD / sqrt(N))

        # Create Extra column for later rbinding
        Statistics <- Statistics %>%
                          mutate(dummy = "All", .before = 1)

        # Harmonize column names for rbinding
        colnames(Statistics) <- colnames(df_Groupwise)

        Statistics <- rbind(Statistics,
                            df_Groupwise)
    }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(Statistics)
}


