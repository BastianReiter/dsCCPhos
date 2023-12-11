
#' AttritionTracker_Update
#'
#' Adds entry to preformed Attrition Tracker
#'
#' @param AttritionTracker A preformed tibble
#' @param NewSampleSize Integer | Sample size at point of call in processing
#' @param InclusionComment String | Optional comment on reasons for inclusion
#' @param ExclusionComment String | Optional comment on reasons for exclusion
#'
#' @return A tibble (Attrition Tracker)
#' @export
#'
#' @examples
AttritionTracker_Update <- function(AttritionTracker,
                                    NewSampleSize,
                                    InclusionComment = "",
                                    ExclusionComment = "")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    AttritionTracker %>%
        tibble::add_row(SampleSize = NewSampleSize,
                        InclusionComment = InclusionComment,
                        ExclusionComment = ExclusionComment)
}
