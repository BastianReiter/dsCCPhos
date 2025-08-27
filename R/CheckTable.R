
#' CheckTable
#'
#' Checks out a data.frame and returns an informative list object.
#'
#' @param Table \code{data.frame} or \code{tibble}
#' @param RequiredFeatureNames \code{character vector} - Optional names of required features - Default: \code{names(Table)}
#'
#' @return A \code{list} containing informative meta data about a \code{data.frame}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CheckTable <- function(Table,
                       RequiredFeatureNames = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(tidyr)

  # --- For Testing Purposes ---
  # Table <- CDS_Diagnosis
  # RequiredFeatureNames <- NULL

  # If no 'RequiredFeatureNames' are passed, assign feature names of Table per default
  if (is.null(RequiredFeatureNames)) { RequiredFeatureNames <- names(Table) }

  # Initiate output object
  TableCheck <- NULL

  # Create template only if one of the conditions below is met
  if (length(Table) == 0 || is.null(Table))
  {
      TableCheck <- list(TableExists = FALSE,
                         TableComplete = FALSE,
                         FeatureCheckOverview = tibble(Feature = RequiredFeatureNames,
                                                       Exists = FALSE,
                                                       Type = NA,
                                                       NonMissingValueCount = NA,
                                                       NonMissingValueRate = NA),
                         MissingFeatures = RequiredFeatureNames,
                         RowCount = NA)

  } else {

      # Identify missing features
      PresentFeatureNames <- names(Table)
      MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

      TableComplete <- (length(MissingFeatures) == 0)

      # Get table row count
      RowCount <- nrow(Table)

      # Get summarizing data.frame that contains info about existence of table features
      FeatureExistence <- tibble(Feature = RequiredFeatureNames) %>%
                               mutate(Exists = case_when(Feature %in% PresentFeatureNames ~ TRUE,
                                                         .default = FALSE))

      # Get types/classes of table features
      FeatureTypes <- Table %>%
                          summarize(across(everything(), ~ typeof(.x))) %>%
                          pivot_longer(cols = everything(),
                                       names_to = "Feature",
                                       values_to = "Type")

      # Get absolute count of non-missing values per table feature
      NonMissingValueCounts <- Table %>%
                                  summarize(across(everything(), ~ sum(!(is.na(.x) | (is.character(.x) & .x == ""))))) %>%
                                  pivot_longer(cols = everything(),
                                               names_to = "Feature",
                                               values_to = "NonMissingValueCount")

      # Get rate of non-missing values per table feature
      NonMissingValueRates <- Table %>%
                                  summarize(across(everything(), ~ sum(!(is.na(.x) | (is.character(.x) & .x == ""))) / n())) %>%
                                  pivot_longer(cols = everything(),
                                               names_to = "Feature",
                                               values_to = "NonMissingValueRate")

      # Consolidate feature meta data in one data.frame
      FeatureCheckOverview <- FeatureExistence %>%
                                  left_join(FeatureTypes, by = join_by(Feature)) %>%
                                  left_join(NonMissingValueCounts, by = join_by(Feature)) %>%
                                  left_join(NonMissingValueRates, by = join_by(Feature))

      # List for return statement
      TableCheck <- list(TableExists = TRUE,
                         TableComplete = TableComplete,
                         FeatureCheckOverview = FeatureCheckOverview,
                         MissingFeatures = MissingFeatures,
                         RowCount = RowCount)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TableCheck)
}
