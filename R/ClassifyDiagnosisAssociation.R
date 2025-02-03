
#' ClassifyDiagnosisAssociation
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' In patients with multiple diagnosis entries: Identify and classify associations between diagnosis entries.
#'
#' @param DiagnosisEntries \code{data.frame} containing multiple diagnosis entries
#' @param RuleCalls \code{list} - Unevaluated \code{dplyr::case_when}-Statements compiled from rules defined in \code{RuleSet_DiagnosisAssociation}
#' @param ProgressBarObject \code{progress::progress_bar} object - Optionally pass progress bar object to display progress
#'
#' @return \code{data.frame} enhanced with classified diagnosis associations
#' @export
#'
#' @author Bastian Reiter
ClassifyDiagnosisAssociation <- function(DiagnosisEntries,
                                         RuleCalls,
                                         ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)
    require(stringr)
    require(tidyr)


    # For function testing purposes
    # DiagnosisEntries <- df_Diagnosis %>% filter(PatientID == "Pat_10010")
    # RuleCalls <- RuleCalls_DiagnosisAssociation
    # RulesProfile = "Default"
    # print(DiagnosisEntries$DiagnosisID[1])


    # Update progress bar object, if assigned in function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(ProgressBarObject)) { ProgressBarObject$tick() }


    # Load meta data: Cancer diagnosis groups as proposed by ICD-10
    Meta_ICD10Group <- dsCCPhos::Meta_CancerGrouping %>%
                            select(ICD10CodeShort,
                                   CancerTopographyGroup_ICD10) %>%
                            rename(ICD10Group = CancerTopographyGroup_ICD10)


    # Enhance diagnosis entries by auxiliary variables
    DiagnosisEntries <- DiagnosisEntries %>%
                            arrange(DiagnosisDate, HistologyDate) %>%
                            mutate(ICD10CodeShort = case_when(str_starts(ICD10Code, "C") ~ str_sub(ICD10Code, end = 3),
                                                              str_starts(ICD10Code, "D") ~ str_sub(ICD10Code, end = 5),
                                                              TRUE ~ str_sub(ICD10Code, end = 3)),
                                   ICDOTopographyCodeShort = str_sub(ICDOTopographyCode, end = 3),
                                   ICDOMorphologyCodeShort = str_sub(ICDOMorphologyCode, end = 4)) %>%
                            left_join(Meta_ICD10Group, by = join_by(ICD10CodeShort))


    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      slice_min(DiagnosisDate) %>%     # Select oldest diagnosis (or diagnoses)
                      arrange(desc(ICD10Code)) %>%      # If there are more than one diagnoses with the same date, prefer the one that starts with "D" over one that starts with "C" (to account for line in progression)
                      slice_head() %>%
                      mutate(ReferenceDiagnosisID = DiagnosisID,
                             IsLikelyAssociated = FALSE)

    # First set of candidates that are compared to reference diagnosis
    Candidates <- DiagnosisEntries %>%
                      filter(DiagnosisID != Reference$DiagnosisID)

    # Initiation of Output data frame
    Output <- NULL

    # Loop through all remaining "unassociated" diagnoses, as long as there are still candidates for potential association
    while (nrow(Candidates) > 0)
    {
        # Apply rules on identification of associated diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsLikelyAssociated'.
        Candidates <- Candidates %>%
                          mutate(IsLikelyAssociated = eval(parse(text = RuleCalls[["IsLikelyAssociated"]])))

        Associations <- Candidates %>%
                            filter(IsLikelyAssociated == TRUE) %>%
                            mutate(ReferenceDiagnosisID = Reference$DiagnosisID,
                                   InconsistencyCheck = eval(parse(text = RuleCalls[["InconsistencyCheck"]])),
                                   ImplausibilityCheck = eval(parse(text = RuleCalls[["ImplausibilityCheck"]])),
                                   Relation_ICD10 = eval(parse(text = RuleCalls[["Relation_ICD10"]])),
                                   Relation_ICDOTopography = eval(parse(text = RuleCalls[["Relation_ICDOTopography"]])),
                                   Relation_LocalizationSide = eval(parse(text = RuleCalls[["Relation_LocalizationSide"]])),
                                   Relation_ICDOMorphology = eval(parse(text = RuleCalls[["Relation_ICDOMorphology"]])),
                                   Relation_Grading = eval(parse(text = RuleCalls[["Relation_Grading"]])),
                                   IsLikelyProgression = eval(parse(text = RuleCalls[["IsLikelyProgression"]])),
                                   IsLikelyRecoding = eval(parse(text = RuleCalls[["IsLikelyRecoding"]]))) %>%
                            nest(Association = c(InconsistencyCheck,
                                                 ImplausibilityCheck,
                                                 starts_with(c("Relation")),
                                                 IsLikelyProgression,
                                                 IsLikelyRecoding))


        # Add both Reference diagnosis entry and associated diagnosis entries to Output data frame
        Output <- bind_rows(Output,
                            Reference,
                            Associations)

        # Assign new reference diagnosis
        Reference <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE) %>%
                          slice_min(tibble(DiagnosisDate, HistologyDate)) %>%
                          arrange(desc(ICD10Code)) %>%      # If there are more than one diagnoses with the same date, prefer the one that starts with "D" over one that starts with "C" (to account for line in progression)
                          slice_head() %>%
                          mutate(ReferenceDiagnosisID = DiagnosisID)

        # Assign new candidates for associated diagnoses
        Candidates <- Candidates %>%
                          filter(IsLikelyAssociated == FALSE) %>%
                          filter(DiagnosisID != Reference$DiagnosisID)
    }


    # In case the original input data frame consists of one single diagnosis entry, the loop above will not be executed, therefore the Output is simply the one diagnosis entry
    # In case the loop ended but there is still a Reference row, it will be added to the Output

    Output <- bind_rows(Output,
                        Reference)


    # Process Output data frame
    Output <- Output %>%
                  select(-c(ICD10CodeShort,
                            ICD10Group,
                            ICDOTopographyCodeShort,
                            ICDOMorphologyCodeShort)) %>%
                  mutate(Association = ifelse(IsLikelyAssociated == FALSE,
                                              list(),
                                              Association)) %>%
                  relocate(ReferenceDiagnosisID, .before = DiagnosisID)


    return(as.data.frame(Output))
}
