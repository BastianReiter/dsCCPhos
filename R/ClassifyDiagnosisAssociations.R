
#' ClassifyDiagnosisAssociations
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' In patients with multiple diagnosis entries: Identify and classify associations between diagnosis entries.
#'
#' @param DiagnosisEntries Data frame (partially curated) that contains multiple diagnosis entries
#' @param Method String | Determines the method being used for classification
#'               \itemize{
#'                    \item "Rule-based" (Default)
#'                    \item "ML-Model" }
#' @param ClassificationRules
#'
#' @return Data frame enhanced with classified diagnosis associations
#' @export
#'
#' @examples
#' @author Bastian Reiter
ClassifyDiagnosisAssociations <- function(DiagnosisEntries,
                                          Method = "Rule-based",
                                          RulesProfile = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)
    require(stringr)
    require(tidyr)

    # For function testing purposes
    DiagnosisEntries <- df_CDS_Diagnosis %>% filter(PatientID == "Pat_8386")
    RulesProfile = "Default"

    # Load meta data
    #~~~~~~~~~~~~~~~
    # a) Cancer diagnosis groups as proposed by ICD-10
    Meta_ICD10Group <- dsCCPhos::Meta_CancerGrouping %>%
                            select(ICD10CodeShort,
                                   CancerTopographyGroup_ICD10) %>%
                            rename(ICD10Group = CancerTopographyGroup_ICD10)

    # b) Rule set for classification of associated diagnosis entries
    RuleSet_DiagnosisAssociation <- dsCCPhos::Meta_DiagnosisAssociation %>%
                                        filter(Profile == RulesProfile)

    # c) Vector of names of features that determine classification of diagnosis association
    PredictorFeatures_DiagnosisAssociation = c("ICD10Code",
                                               "ICD10CodeShort",
                                               "ICD10Group",
                                               "ICDOTopographyCode",
                                               "ICDOTopographyCodeShort",
                                               "LocalizationSide",
                                               "ICDOMorphologyCode",
                                               "ICDOMorphologyCodeShort",
                                               "Grading")


    # Enhance diagnosis entries by auxiliary variables
    DiagnosisEntries <- DiagnosisEntries %>%
                            arrange(InitialDiagnosisDate) %>%
                            mutate(ICD10CodeShort = str_sub(ICD10Code, end = 3),
                                   ICDOTopographyCodeShort = str_sub(ICDOTopographyCode, end = 3),
                                   ICDOMorphologyCodeShort = str_sub(ICDOMorphologyCode, end = 4)) %>%
                            left_join(Meta_ICD10Group, by = join_by(ICD10CodeShort))


    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      slice_min(tibble(InitialDiagnosisDate, HistologyDate)) %>%      # Select oldest diagnosis (or diagnoses)
                      arrange(desc(ICD10Code)) %>%      # If there are more than one diagnoses with the same date, prefer the one that starts with "D" over one that starts with "C" (to account for line in progression)
                      first() %>%
                      mutate(ReferenceDiagnosisID = DiagnosisID)

    # First set of candidates that are compared to reference diagnosis
    Candidates <- DiagnosisEntries %>%
                      filter(DiagnosisID != Reference$DiagnosisID)

    # Initiation of Output data frame
    Output <- NULL

    if (nrow(Candidates) == 0)      # In case the input data frame consists of one single diagnosis entry the loop below will not be executed, therefore the Output is simply the one diagnosis entry
    {
        Output <- Reference
    }

    # Loop through all remaining "unassociated" diagnoses, as long as there are still candidates for potential association
    while (nrow(Candidates) > 0)
    {
        # Enhance candidate entries by auxiliary variables
        Candidates <- Candidates %>%
                          mutate(IsLikelyAssociated = eval(parse(text = GetClassificationCall(TargetFeature = "IsLikelyAssociated",
                                                                                              PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                              RuleSet = RuleSet_DiagnosisAssociation,
                                                                                              ValueIfNoRuleMet = "FALSE"))))


        # Apply rules on identification of associated diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsLikelyAssociated'.
        Candidates <- Candidates %>%
                          left_join(Rules_DiagnosisAssociation, by = join_by(starts_with("Relation"))) %>%
                          mutate(IsLikelyAssociated = ifelse(is.na(IsLikelyAssociated), FALSE, IsLikelyAssociated))      # Replace NA values with 'FALSE'


        Associations <- Candidates %>%
                            filter(IsLikelyAssociated == TRUE) %>%
                            mutate(ReferenceDiagnosisID = Reference$DiagnosisID)


        # Add both Reference diagnosis entry and associated diagnosis entries to Output data frame
        Output <- bind_rows(Output,
                            Reference,
                            Associations)

        # Assign new reference diagnosis
        Reference <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE) %>%
                          slice_max(InitialDiagnosisDate) %>%
                          arrange(ICD10Code) %>%
                          first() %>%
                          mutate(ReferenceDiagnosisID = DiagnosisID)

        # Assign new candidates for associated diagnoses
        Candidates <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE) %>%
                          filter(DiagnosisID != Reference$DiagnosisID)
    }


    # Process Output data frame
    Output <- Output %>%
                  select(-ICD10CodeShort,
                         -ICDOTopographyCodeShort,
                         -CancerTopographyGroup_ICD10) %>%
                  nest(Association = c(starts_with(c("Association",
                                                     "IsSame")))) %>%
                  mutate(Association = ifelse(is.na(IsLikelyAssociated),
                                              list(),
                                              Association)) %>%
                  relocate(ReferenceDiagnosisID, .before = DiagnosisID)

    return(as.data.frame(Output))
}
