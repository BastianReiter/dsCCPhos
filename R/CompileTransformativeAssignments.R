
#' CompileTransformativeAssignments
#'
#' Auxiliary function within \code{\link{CurateDataDS}} or \code{\link{HarmonizeData}} respectively
#'
#' Based on a given meta data.frame, compile assignment expressions for dplyr::mutate()
#'
#' @param TransformativeExpressions \code{data.frame} - Contains expressions that transform data values
#' @param TransformativeExpressions.Profile \code{string} - Profile selected in \emph{TransformativeExpressions}
#' @param FeatureNames \code{character vector} - Names of features to be transformed
#'
#' @return \code{string} containing assignments for \code{dplyr::mutate()}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CompileTransformativeAssignments <- function(TransformativeExpressions,
                                             TransformativeExpressions.Profile,
                                             FeatureNames)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)

  # --- For testing purposes ---
  # TransformativeExpressions <- dsCCPhos::Meta_TransformativeExpressions
  # TransformativeExpressions.Profile <- "Default"
  # FeatureNames <- c("TNM_N", "TNM_M")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # If 'FeatureNames' is empty or NULL, no compilation of arguments for dplyr::mutate() is necessary
  if (length(FeatureNames) == 0 || is.null(FeatureNames))
  {
      return(NA)
  }

  # Filter relevant expressions from given data.frame
  RelevantExpressions <- TransformativeExpressions %>%
                              filter(Profile == TransformativeExpressions.Profile,
                                     Feature %in% FeatureNames) %>%
                              arrange(EvaluationOrder)

  if (nrow(RelevantExpressions) == 0)
  {
      return(NA)

  } else {

      vc_Assignments <- NULL

      for (i in 1:nrow(RelevantExpressions))
      {
          Assignment <- NA
          Feature <- RelevantExpressions$Feature[i]
          Expression <- RelevantExpressions$Expression[i]

          if (!is.na(Feature) & !is.na(Expression))
          {
              Expression <- str_replace(Expression, ".X", Feature)
              Assignment <- paste0(Feature, " = ", Expression)
          }

          vc_Assignments <- c(vc_Assignments, Assignment)
      }

      # Sort out Assignments that are NA and consolidate all Assignments in a single string
      Assignments <- paste0(vc_Assignments[!is.na(vc_Assignments)], collapse = ", ")
      if (Assignments == "") { Assignments <- NA }

      return(Assignments)
  }
}
