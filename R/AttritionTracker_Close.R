
#' AttritionTracker_Close
#'
#' @param AttritionTracker A preformed tibble containing attrition information
#'
#' @return A tibble with finalized Attrition Tracker
#' @export
#'
#' @examples
AttritionTracker_Close <- function(AttritionTracker)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    AttritionTracker %>%
        mutate(AttritionCount = SampleSize - lead(SampleSize)) %>%
        tibble::rowid_to_column() %>%
        rename(Step = rowid)
}
