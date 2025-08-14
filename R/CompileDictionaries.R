
#' CompileDictionaries
#'
#' Auxiliary function within \code{\link{CurateDataDS}} or \code{\link{HarmonizeData}} respectively
#'
#' Based on a given meta data.frame, compile dictionaries (named vectors) for direct value replacement
#'
#' @param Dictionary \code{data.frame} - Contains look-up and replacement values
#' @param Dictionary.Profile \code{string} - Profile selected in \emph{Dictionary}
#' @param FeatureNames \code{character vector} - Names of features to be transformed
#'
#' @return \code{list} of named vectors (serving as hash tables)
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CompileDictionaries <- function(Dictionary,
                                Dictionary.Profile,
                                FeatureNames)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)

  # --- For Testing Purposes ---
  # Dictionary <- dsCCPhos::Meta_Dictionaries
  # Dictionary.Profile <- "Default"
  # FeatureNames <- c("TNM_T", "TNM_N")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # If 'FeatureNames' is empty or NULL, no compilation of dictionaries is necessary
  if (length(FeatureNames) == 0 || is.null(FeatureNames))
  {
      return(NULL)
  }

  # Filter relevant dictionary data from given data.frame
  RelevantDictionaryData <- Dictionary %>%
                                filter(Profile == Dictionary.Profile,
                                       Feature %in% FeatureNames)

  if (nrow(RelevantDictionaryData) == 0)
  {
      return(NULL)

  } else {

      # Get list of dictionaries (named vectors) corresponding to selected features
      Dictionaries <- RelevantDictionaryData %>%
                          split(.$Feature) %>%
                          map(\(df) setNames(df$NewValue,
                                             nm = df$LookupValue))

      return(Dictionaries)
  }
}
