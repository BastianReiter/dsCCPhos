
#' GetSampleStatisticsDS
#'
#' Calculate common sample statistics for a feature of a data frame. The function recognizes the feature's data type and adjusts the output accordingly.
#'
#' @param TableName.S \code{string} | Name of the Data frame that contains the feature
#' @param FeatureName.S \code{string} | Name of feature
#' @param GroupingFeatureName.S \code{string} | Name of optional grouping feature
#' @param MaxNumberCategories.S \code{integer} | With categorical features: Maximum number of categories analyzed individually before frequencies are cumulated in 'Other' category.
#' @param RemoveMissingsInNumeric.S \code{logical} | With numeric features; Whether NA values should be removed before calculating statistics
#'
#' @return A list containing:
#'         \itemize{\item MetaData (\code{tibble})
#'                  \item Statistics (\code{tibble})
#'                      \itemize{\item For numeric features: Parametric and non-parametric sample statistics
#'                               \item For character features: Absolute and relative frequencies}}
#' @export
#'
#' @examples
GetSampleStatisticsDS <- function(TableName.S,
                                  FeatureName.S,
                                  GroupingFeatureName.S = NULL,
                                  MaxNumberCategories.S = NULL,
                                  RemoveMissingsInNumeric.S = TRUE)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check, evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName.S)
      & is.character(FeatureName.S)
      & (is.null(GroupingFeatureName.S) | (!is.null(GroupingFeatureName.S) & is.character(GroupingFeatureName.S))))
{
    Table <- eval(parse(text = TableName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "Error: 'TableName.S', 'FeatureName.S' and (optionally) 'GroupingFeatureName.S' must be specified as a character string"
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
# FeatureName.S <- "TNM_T"
# GroupingFeatureName.S <- "LastVitalStatus"
# MaxNumberCategories.S <- 5
# RemoveMissingsInNumeric.S = TRUE


# Evaluate feature in question
Feature <- Table[[FeatureName.S]]


# Tibble containing useful meta data about sample size (regardless of feature type)
MetaData <- tibble(DataType = class(Feature),
                   N_Total = length(Feature),
                   N_Valid = sum(!is.na(Feature)),
                   ValidProportion = N_Valid / N_Total,
                   N_Missing = sum(is.na(Feature)),
                   MissingProportion = N_Missing / N_Total)

# Initiate Output object
Output <- list(MetaData = MetaData,
               Statistics = NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NUMERIC feature
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (class(Feature) == "numeric" & (MetaData$N_Valid > 0))
{
    MetaData <- MetaData %>%
                    mutate(ScaleType = "metric", .after = DataType)

    # Calculate parametric and non-parametric sample statistics
    Statistics <- as_tibble(Table) %>%
                      summarize(N = MetaData$N_Valid,
                                q5 = quantile(.data[[FeatureName.S]], probs = 0.05, na.rm = RemoveMissingsInNumeric.S),
                                Q1 = quantile(.data[[FeatureName.S]], probs = 0.25, na.rm = RemoveMissingsInNumeric.S),
                                Median = median(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                Q3 = quantile(.data[[FeatureName.S]], probs = 0.75, na.rm = RemoveMissingsInNumeric.S),
                                q95 = quantile(.data[[FeatureName.S]], probs = 0.95, na.rm = RemoveMissingsInNumeric.S),
                                MAD = mad(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                Mean = mean(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                SD = sd(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                SEM = SD / sqrt(N))

    # If GroupingFeatureName.S is passed...
    if (!is.null(GroupingFeatureName.S))
    {
        # Get group-specific output
        df_Groupwise <- as_tibble(Table) %>%
                            group_by(., .data[[GroupingFeatureName.S]]) %>%
                            summarize(q5 = quantile(.data[[FeatureName.S]], probs = 0.05, na.rm = RemoveMissingsInNumeric.S),
                                      Q1 = quantile(.data[[FeatureName.S]], probs = 0.25, na.rm = RemoveMissingsInNumeric.S),
                                      Median = median(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                      Q3 = quantile(.data[[FeatureName.S]], probs = 0.75, na.rm = RemoveMissingsInNumeric.S),
                                      q95 = quantile(.data[[FeatureName.S]], probs = 0.95, na.rm = RemoveMissingsInNumeric.S),
                                      MAD = mad(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                      Mean = mean(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                      SD = sd(.data[[FeatureName.S]], na.rm = RemoveMissingsInNumeric.S),
                                      SEM = SD / sqrt(N))

        # Create Extra column for later rbinding
        Statistics <- Statistics %>%
                          mutate(dummy = "All", .before = 1)

        # Harmonize column names for rbinding
        colnames(Statistics) <- colnames(df_Groupwise)

        Statistics <- rbind(Statistics,
                            df_Groupwise)
    }

    # Create Output object
    Output <- list(MetaData = MetaData,
                   Statistics = Statistics)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CATEGORICAL / CHARACTER feature
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (class(Feature) == "character" & (MetaData$N_Valid > 0))
{
    # Add unique value count to meta data
    MetaData <- MetaData %>%
                    mutate(ScaleType = "nominal", .after = DataType) %>%
                    mutate(CountUniqueValues = length(unique(Feature[!is.na(Feature)])))      # Count only non-NA unique values

    # Tibble containing absolute frequencies
    Frequencies <- as_tibble(table(Feature, useNA = "no")) %>%
                        rename(c(Frequency = "n",
                                 Value = "Feature")) %>%
                        arrange(desc(Frequency))

    # If the number of unique values exceeds 'MaxNumberCategories.S', cumulate less frequent categories under 'Other' category
    if (!is.null(MaxNumberCategories.S))
    {
      if (nrow(Frequencies) > MaxNumberCategories.S)
      {
           FrequenciesKeep <- Frequencies %>%
                                  slice_head(n = MaxNumberCategories.S)

           FrequenciesCumulate <- Frequencies %>%
                                      slice_tail(n = nrow(Frequencies) - MaxNumberCategories.S)

           Frequencies <- bind_rows(FrequenciesKeep,
                                    tibble(Value = "Other",
                                           Frequency = sum(FrequenciesCumulate$Frequency)))
      }
    }

    # Calculate relative frequencies / proportions
    Frequencies <- Frequencies %>%
                        mutate(Proportion = Frequency / MetaData$N_Valid)

    # Create Output object
    Output <- list(MetaData = MetaData,
                   Statistics = Frequencies)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOGICAL feature
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (class(Feature) == "logical" & (MetaData$N_Valid > 0))
{
    MetaData <- MetaData %>%
                    mutate(ScaleType = "binary", .after = DataType)

    # Tibble containing absolute frequencies
    Frequencies <- as_tibble(table(Feature, useNA = "no")) %>%
                        rename(c(Frequency = "n",
                                 Value = "Feature")) %>%
                        mutate(Proportion = Frequency / MetaData$N_Valid)

    # Create Output object
    Output <- list(MetaData = MetaData,
                   Statistics = Frequencies)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(Output)
}


