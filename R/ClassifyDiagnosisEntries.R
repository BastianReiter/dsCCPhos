
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
ClassifyDiagnosisEntries <- function(DiagnosisEntries,
                                     Method = "Auto",
                                     Rules)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    #DiagnosisEntries <- df_Aux_Diagnosis_MultipleEntries %>% filter(PatientID == "Pat_2607")

    # Load meta data about cancer topography groups as proposed by ICD-10
    df_ICD10TopographyGroup <- dsCCPhos::Meta_CancerGrouping %>%
                                    select(ICD10CodeShort,
                                           CancerTopographyGroup_ICD10)

    DiagnosisEntries <- DiagnosisEntries %>%
                            arrange(InitialDiagnosisDate) %>%
                            mutate(ICD10CodeShort = str_sub(ICD10Code, end = 3),
                                   ICDOTopographyCodeShort = str_sub(ICDOTopographyCode, end = 3)) %>%
                            left_join(df_ICD10TopographyGroup, by = join_by(ICD10CodeShort))

    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      first()

    # First set of candidates that are compared to reference diagnosis
    Candidates <- DiagnosisEntries %>%
                      filter(row_number() > 1)

    # Initiation of Output data frame
    Output <- NULL

    if (nrow(Candidates) == 0)      # In case the input data frame consists of one single diagnosis entry the loop below will not be executed, therefore the Output is simply the one diagnosis entry
    {
        Output <- Reference
    }

    # Loop through all remaining "unassociated" diagnosis, as long as there are still candidates for potential association
    while (nrow(Candidates) > 0)
    {
        Candidates <- Candidates %>%
                          mutate(IsSameICD10Code = ifelse(ICD10Code[row_number()] == Reference$ICD10Code, TRUE, FALSE),
                                 IsSameICD10CodeShort = ifelse(ICD10CodeShort[row_number()] == Reference$ICD10CodeShort, TRUE, FALSE),
                                 IsSameICDOTopographyCode = ifelse(ICDOTopographyCode[row_number()] == Reference$ICDOTopographyCode, TRUE, FALSE),
                                 IsSameICDOTopographyCodeShort = ifelse(ICDOTopographyCodeShort[row_number()] == Reference$ICDOTopographyCodeShort, TRUE, FALSE),
                                 IsSameICD10TopographyGroup = ifelse(CancerTopographyGroup_ICD10[row_number()] == Reference$CancerTopographyGroup_ICD10, TRUE, FALSE),
                                 IsSameLocalizationSide = ifelse(LocalizationSide[row_number()] == Reference$LocalizationSide, TRUE, FALSE),
                                 IsLikelyAssociated = case_when(IsSameICD10CodeShort ~ TRUE,
                                                                IsSameICDOTopographyCodeShort ~ TRUE,
                                                                IsSameICD10TopographyGroup ~ TRUE,
                                                                TRUE ~ FALSE))

        Associates <- Candidates %>%
                          filter(IsLikelyAssociated == TRUE) %>%
                          mutate(ReferenceDiagnosisID = Reference$DiagnosisID,
                                 AssociationEntity = case_when(IsSameICD10Code ~ "Same entity",
                                                               IsSameICD10CodeShort ~ "Same organ",
                                                               TRUE ~ "Different organ"),
                                 AssociationTopography = case_when(IsSameICDOTopographyCode ~ "Same topography",
                                                                   IsSameICDOTopographyCodeShort ~ "Slight change in topography",
                                                                   IsSameICD10TopographyGroup ~ "Same topography group",
                                                                   TRUE ~ "Different topography"),
                                 AssociationSide = case_when(IsSameLocalizationSide ~ "Same side",
                                                             !IsSameLocalizationSide ~ "Different side"))

        Output <- bind_rows(Output,
                            Reference,
                            Associates)

        # New reference diagnosis
        Reference <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE) %>%
                          first()

        # New candidates for associated diagnoses
        Candidates <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE & row_number() > 1)
    }


    return(as.data.frame(Output))
}
