
#' ClassifyDiagnosisDuplicates
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' In patients with multiple diagnosis entries: Identify and classify duplicate diagnosis entries.
#'
#' @param DiagnosisEntries Data frame (partially curated) that contains multiple diagnosis entries
#' @param RulesProfile String | Profile name of rule set defined in \code{\link{RuleSet_DiagnosisDuplicates}
#'
#' @return Data frame of classified duplicate diagnosis entries
#' @export
#'
#' @examples
#' @author Bastian Reiter
ClassifyDiagnosisAssociations <- function(DiagnosisEntries,
                                          RulesProfile = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)
    require(stringr)
    require(tidyr)


    # For function testing purposes
    DiagnosisEntries <- df_CDS_Diagnosis %>% filter(PatientID == "Pat_10186")
    RulesProfile = "Default"
    # print(DiagnosisEntries$DiagnosisID[1])


    # Load meta data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # a) Rule set for classification of duplicate diagnosis entries
    RuleSet_DiagnosisDuplicates <- dsCCPhos::RuleSet_DiagnosisDuplicates %>%
                                        filter(Profile == RulesProfile)

    # b) Vector of names of features that determine identification of duplicate diagnosis entry
    PredictorFeatures_DiagnosisDuplicates = c("InitialDiagnosisDate",
                                              "ICD10Code",
                                              "ICDOTopographyCode",
                                              "LocalizationSide",
                                              "HistologyDate",
                                              "ICDOMorphologyCode",
                                              "Grading")


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Arrange diagnosis entries by InitialDiagnosisDate and lowest number of NAs
    # and enhance by auxiliary feature CountNAs
    DiagnosisEntries <- DiagnosisEntries %>%
                            select(DiagnosisID, all_of(PredictorFeatures_DiagnosisDuplicates)) %>%
                            rowwise() %>%
                            mutate(CountNAs = sum(is.na(across(everything())))) %>%
                            ungroup() %>%
                            arrange(InitialDiagnosisDate, CountNAs)

    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      slice_min(tibble(InitialDiagnosisDate, HistologyDate)) %>%      # Select earliest diagnosis (or diagnoses)
                      first() %>%
                      mutate(JointDiagnosisID = DiagnosisID)

    # First set of candidates that are compared to reference diagnosis
    Candidates <- DiagnosisEntries %>%
                      filter(DiagnosisID != Reference$DiagnosisID)

    # Add CountDeviatingValues (calculated below) to vector of predicting feature names
    PredictorFeatures_DiagnosisDuplicates <- c("CountDeviatingValues", PredictorFeatures_DiagnosisDuplicates)

    # Initiation of Output data frame
    Output <- NULL

    if (nrow(Candidates) == 0)      # In case the input data frame consists of one single diagnosis entry the loop below will not be executed, therefore the Output is simply the one diagnosis entry
    {
        Output <- Reference
    }

    # Loop through all remaining "unassociated" diagnoses, as long as there are still candidates that are likely identical to reference
    while (nrow(Candidates) > 0)
    {
        # Apply rules on identification of duplicate diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsDuplicateEntry'.
        Candidates <- Candidates %>%
                          rowwise() %>%
                              mutate(CountDeviatingValues = CountDeviations(CandidateEntry = pick(all_of(setdiff(PredictorFeatures_DiagnosisDuplicates, "CountDeviatingValues"))),
                                                                            ReferenceEntry = Reference,
                                                                            ComparedFeatures = setdiff(PredictorFeatures_DiagnosisDuplicates, "CountDeviatingValues"))) %>%

                          ungroup() %>%
                          mutate(IsLikelyDuplicate = eval(parse(text = CompileClassificationCall(TargetFeature = "IsLikelyDuplicate",
                                                                                                 PredictorFeatures = PredictorFeatures_DiagnosisDuplicates,
                                                                                                 RuleSet = RuleSet_DiagnosisDuplicates,
                                                                                                 ValueIfNoRuleMet = FALSE))))

        Duplicates <- Candidates %>%
                          filter(IsLikelyDuplicate == TRUE) %>%
                          mutate(JointDiagnosisID = Reference$DiagnosisID,
                                 DuplicateIDs = list(DiagnosisID))

        JointDiagnosis <- bind_rows(Reference, Duplicates) %>%
                              fill(everything(), .direction = "down") %>%
                              fill(everything(), .direction = "up") %>%
                              filter(DiagnosisID == Reference$DiagnosisID)


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
                         -ICD10Group,
                         -ICDOTopographyCodeShort)
                  nest(Association = c(InconsistencyCheck,
                                       ImplausibilityCheck,
                                       starts_with(c("Relation")),
                                       IsLikelyProgression,
                                       IsLikelyRecoding)) %>%
                  mutate(Association = ifelse(is.na(IsLikelyAssociated),
                                              list(),
                                              Association)) %>%
                  relocate(ReferenceDiagnosisID, .before = DiagnosisID)


    return(as.data.frame(Output))
}
