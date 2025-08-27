
#' HarmonizeData
#'
#' This function addresses harmonizing data transformation for features of a \code{data.frame} based on passed settings / meta data.
#' Harmonization is attempted in a step-wise approach, incorporating various methods in a defined order:
#' \enumerate{\item Transformative expressions (e.g. functions like \code{str_to_upper()})
#'            \item Dictionary look-up
#'            \item Fuzzy String Matching}
#'
#' @param DataFrame \code{data.frame} containing data to be transformed
#' @param EligibleValueSets \code{list} of character vectors containing sets of eligible values for corresponding feature
#' @param Methods \code{data.frame} - Contains data on the selection of methods to use for each feature
#' @param TransformativeExpressions \code{data.frame} - Contains set of expressions like functions used to transform data values
#' @param TransformativeExpressions.Profile \code{string} - Profile selected in \emph{TransformativeExpressions}
#' @param Dictionary \code{data.frame} - Contains dictionary data used to look up and replace data values
#' @param Dictionary.Profile \code{string} - Profile selected in \emph{Dictionary}
#' @param FuzzyStringMatching \code{data.frame} - Contains settings for Fuzzy String Matching
#' @param FuzzyStringMatching.Profile \code{string} - Profile selected in \emph{FuzzyStringMatching}
#'
#' @return The input \code{data.frame} with transformed data values
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HarmonizeData <- function(DataFrame,
                          EligibleValueSets,
                          Methods,
                          TransformativeExpressions,
                          TransformativeExpressions.Profile,
                          Dictionary,
                          Dictionary.Profile,
                          FuzzyStringMatching,
                          FuzzyStringMatching.Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(dsCCPhos)

  # --- For Testing Purposes ---
  # DataFrame <- DataSet$SystemicTherapy
  # tablename <- "SystemicTherapy"
  # EligibleValueSets <- dsCCPhos::Meta_Values %>%
  #                           filter(Table == tablename) %>%
  #                           select(Feature, Value_Raw) %>%
  #                           split(.$Feature) %>%      # 'split' is a base function and needs '.$' to address 'Feature'
  #                           map(\(Set) Set$Value_Raw)
  # Methods <- Settings$DataHarmonization$Methods %>% filter(Table == tablename)
  # TransformativeExpressions = Settings$DataHarmonization$TransformativeExpressions %>% filter(Table == tablename)
  # Dictionary <- Settings$DataHarmonization$Dictionary %>% filter(Table == tablename)
  # FuzzyStringMatching <- Settings$DataHarmonization$FuzzyStringMatching %>% filter(Table == tablename)
  # TransformativeExpressions.Profile <- "Default"
  # Dictionary.Profile <- "Default"
  # FuzzyStringMatching.Profile <- "Default"

#-------------------------------------------------------------------------------

  # Get names of features where different harmonization methods are supposed to be performed
  Features.TransformativeExpressions <- Methods %>% filter(TransformativeExpressions == TRUE) %>% pull(Feature)
  Features.Dictionary <- Methods %>% filter(Dictionary == TRUE) %>% pull(Feature)
  Features.FuzzyStringMatching <- Methods %>% filter(FuzzyStringMatching == TRUE) %>% pull(Feature)
  # Features.Classification <- Methods %>% filter(Classification == TRUE) %>% pull(Feature)

  # Compile mutating assignments with dsCCPhos::CompileTransformativeAssignments()
  TransformativeAssignments <- CompileTransformativeAssignments(TransformativeExpressions,
                                                                TransformativeExpressions.Profile,
                                                                Features.TransformativeExpressions)

  # Compile dictionaries for direct value replacement
  Dictionaries <- CompileDictionaries(Dictionary,
                                      Dictionary.Profile,
                                      Features.Dictionary)


  if (is.na(TransformativeAssignments) & is.null(Dictionaries) & length(Features.FuzzyStringMatching) == 0)
  {
      return(DataFrame)

  } else {

      if (!is.na(TransformativeAssignments))
      {
          DataFrame <- DataFrame %>%
                          eval(expr = parse(text = paste0("mutate(., ", TransformativeAssignments, ")")))
      }

      if (!is.null(Dictionaries))
      {
          for (i in 1:length(Dictionaries))
          {
              Feature <- names(Dictionaries)[i]
              Dictionary <- Dictionaries[[Feature]]
              FeatureData <- DataFrame[[Feature]]

              FeatureDataTransformed <- ifelse(is.na(Dictionary[FeatureData]),
                                               FeatureData,
                                               Dictionary[FeatureData])

              DataFrame[[Feature]] <- FeatureDataTransformed
          }
      }

      if (length(Features.FuzzyStringMatching) > 0)
      {
          for (i in 1:length(Features.FuzzyStringMatching))
          {
              CurrentFeature <- Features.FuzzyStringMatching[i]
              EligibleValueSet <- EligibleValueSets[[CurrentFeature]]
              FSMSettings <- filter(FuzzyStringMatching, Feature == CurrentFeature)

              if (!is.null(EligibleValueSet))
              {
                  DataFrame <- DataFrame %>%
                                    mutate(across(all_of(CurrentFeature),
                                                  ~ GetFuzzyStringMatches(Vector = .x,
                                                                          EligibleStrings = EligibleValueSet,
                                                                          PreferredMethod = FSMSettings$PreferredMethod,
                                                                          FindBestMethod = FSMSettings$FindBestMethod,
                                                                          Tolerance = FSMSettings$Tolerance,
                                                                          Preprocessing.FlattenCase = FSMSettings$Preprocessing.FlattenCase,
                                                                          Preprocessing.RemoveAllWhiteSpace = FSMSettings$Preprocessing.RemoveAllWhiteSpace,
                                                                          Preprocessing.SquishWhiteSpace = FSMSettings$Preprocessing.SquishWhiteSpace,
                                                                          StringdistArguments = list(useBytes = as.logical(FSMSettings$Stringdist.useBytes),
                                                                                                     weight = c(d = as.numeric(FSMSettings$Stringdist.weight.d),
                                                                                                                i = as.numeric(FSMSettings$Stringdist.weight.i),
                                                                                                                s = as.numeric(FSMSettings$Stringdist.weight.s),
                                                                                                                t = as.numeric(FSMSettings$Stringdist.weight.t)),
                                                                                                     q = as.numeric(FSMSettings$Stringdist.q),
                                                                                                     p = as.numeric(FSMSettings$Stringdist.p),
                                                                                                     bt = as.numeric(FSMSettings$Stringdist.bt)))))
              }
          }
      }

      return(DataFrame)
  }
}
