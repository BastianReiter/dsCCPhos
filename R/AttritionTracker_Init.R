
#' AttritionTracker_Init
#'
#' Initialize Attrition Tracker (auxiliary table to track sample sizes around data excluding operations)
#'
#' @return A tibble
#' @export
#'
#' @examples
AttritionTracker_Init <- function()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    AttritionTracker <- tibble::tibble(SampleSize = numeric(),
                                       InclusionComment = character(),
                                       ExclusionComment = character(),
                                       AttritionCount = numeric())
}
