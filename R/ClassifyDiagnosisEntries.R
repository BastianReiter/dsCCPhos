
#' ClassifyDiagnosisEntries
#'
#' In patients with multiple diagnosis entries: Distinguish pseudo-different from actually different diagnoses.
#'
#' @param DataFrame Data frame (partially curated) that contains multiple diagnosis entries
#' @param Method String |
#'
#' @return Data frame with classified diagnoses
#' @export
#'
#' @examples
ClassifyDiagnosisEntries <- function(DataFrame,
                                     Method = "Auto",
                                     Rules)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    df_Diagnoses <- DataFrame

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

    CountDiagnoses <- nrow(df_Diagnoses)

    df_Diagnoses <- df_Diagnoses %>%
                          arrange(InitialDiagnosisDate) %>%
                          mutate(ICD10CodeShort = str_sub(ICD10Code, end = 3),
                                 ICDOTopographyCodeShort = str_sub(ICDOTopographyCode, end = 3),
                                 IsFirstDiagnosis = case_when(row_number() == 1 ~ TRUE,
                                                              TRUE ~ FALSE)) %>%
                          left_join(dsCCPhos::Meta_CancerGrouping, by = join_by(ICD10CodeShort))





    Reference <- df_Diagnoses %>%
                      filter(IsFirstDiagnosis == TRUE)

    Candidates <- df_Diagnoses %>%
                      filter(IsFirstDiagnosis == FALSE)


    for (i in 1:nrow(Candidates))
    {
        Candidates <- Candidates %>%
                          mutate(IsSameICD10Code = ifelse(ICD10Code[row_number()] == Reference$ICD10Code, TRUE, FALSE),
                                 IsSameICD10CodeShort = ifelse(ICD10CodeShort[row_number()] == Reference$ICD10CodeShort, TRUE, FALSE),
                                 IsSameICDOTopographyCode = ifelse(ICDOTopographyCode[row_number()] == Reference$ICDOTopographyCode, TRUE, FALSE),
                                 IsSameICDOTopographyCodeShort = ifelse(ICDOTopographyCodeShort[row_number()] == Reference$ICDOTopographyCodeShort, TRUE, FALSE),
                                 IsSameLocalizationSide = ifelse(LocalizationSide[row_number()] == Reference$LocalizationSide, TRUE, FALSE),
                                 IsLikelyAssociated = case_when(IsSameICD10CodeShort ~ TRUE,
                                                                IsSameICDOTopographyCode ~ TRUE,
                                                                TRUE ~ FALSE))

        Associates <- Candidates %>%
                          filter(IsLikelyAssociated == TRUE) %>%
                          mutate(ReferenceDiagnosisID = ifelse(IsLikelyAssociated,
                                                               Reference$DiagnosisID,
                                                               NA),
                                 AssociationEntity = case_when(IsSameICD10Code ~ "Same entity",
                                                               IsSameICD10CodeShort ~ "Same organ"),
                                 AssociationTopography = case_when(IsSameICDOTopographyCode ~ "Same topography",
                                                                   IsSameICDOTopographyCodeShort ~ "Slight change in topography",
                                                                   TRUE ~ "Different topography"),
                                 AssociationSide = case_when(IsSameLocalizationSide ~ "No change",
                                                             !IsSameLocalizationSide ~ LocalizationSide))


        Candidates <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE)


    }




    df_Output <- df_Diagnoses


    return(df_Output)
}
