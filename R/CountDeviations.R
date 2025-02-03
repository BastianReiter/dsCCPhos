
#' CountDeviations
#'
#' Take two rows from a data frame and count deviating values
#'
#' @param ReferenceEntry A data frame with one row
#' @param CandidateEntry A data frame with one row
#' @param ComparedFeatures Vector
#'
#' @return Integer number
#' @export
#'
#' @author Bastian Reiter
CountDeviations <- function(CandidateEntry,
                            ReferenceEntry,
                            ComparedFeatures = colnames(ReferenceEntry))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)

    # tryCatch

    CandidateEntry <- CandidateEntry %>%
                          select(all_of(ComparedFeatures))

    ReferenceEntry <- ReferenceEntry %>%
                          select(all_of(ComparedFeatures))

    Count <- 0

    for (i in 1:length(ComparedFeatures))
    {
        CandValue <- CandidateEntry[1, ComparedFeatures[i]]
        RefValue <- ReferenceEntry[1, ComparedFeatures[i]]
        if ((!is.na(CandValue) & is.na(RefValue)) | (is.na(CandValue) & !is.na(RefValue))) { Count <- Count + 1 }
        else if (is.na(CandValue) & is.na(RefValue)) { Count <- Count}
        else if (CandValue != RefValue) { Count <- Count + 1}
        else {}
    }

    return(Count)
}
