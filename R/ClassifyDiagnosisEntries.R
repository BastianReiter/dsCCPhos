
#' ClassifyDiagnosisEntries
#'
#' In patients with multiple diagnosis entries: Distinguish pseudo-different from actually different diagnoses.
#'
#' @param DataFrame Data frame (partially curated) that contains multiple diagnosis entries
#' @param GroupingFeature Grouping feature
#'
#' @return Data frame with classified diagnoses
#' @export
#'
#' @examples
ClassifyDiagnosisEntries <- function(DataFrame,
                                     GroupingFeature)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    df_Input <- DataFrame

    # Groupwise output
    # if (quo_is_null(enquo(GroupingFeature)) == FALSE)      # If GroupingFeature is not empty...
    # {
        # Select patients with multiple diagnosis entries
        # ... that differ in at least one of these criteria:
        #       - Diagnosis ID
        #       - ICD-10 code
        #       - ICD-O Topography coder
        #       - Localization (Side)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        df_Output <- df_Input %>%
                          arrange(desc(InitialDiagnosisDate)) %>%
                          distinct(DiagnosisID,
                                   ICD10Code,
                                   ICDOTopographyCode,
                                   LocalizationSide) %>%
                          mutate(CountDifferentCombinations = n())      # Get Counts of different combinations of formal DiagnosisID, ICD10Code, ICDOTopographyCode and LocalizationSide.
    #}

    return(df_Output)
}
