
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
#' @param RulesProfile String | Profile name of rule set defined in \code{\link{RuleSet_DiagnosisAssociation}
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
    # DiagnosisEntries <- df_CDS_Diagnosis %>% filter(PatientID == "Pat_10186")
    # RulesProfile = "Default"
    # print(DiagnosisEntries$DiagnosisID[1])


    # Load meta data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # a) Rule set for classification of associated diagnosis entries
    RuleSet_DiagnosisAssociation <- dsCCPhos::RuleSet_DiagnosisAssociation %>%
                                        filter(Profile == RulesProfile)

    # b) Vector of names of features that determine classification of diagnosis association
    PredictorFeatures_DiagnosisAssociation = c("ICD10Code",
                                               "ICD10CodeShort",
                                               "ICD10Group",
                                               "ICDOTopographyCode",
                                               "ICDOTopographyCodeShort",
                                               "LocalizationSide",
                                               "ICDOMorphologyCode",
                                               "ICDOMorphologyCodeShort",
                                               "Grading")

    # c) Cancer diagnosis groups as proposed by ICD-10
    Meta_ICD10Group <- dsCCPhos::Meta_CancerGrouping %>%
                            select(ICD10CodeShort,
                                   CancerTopographyGroup_ICD10) %>%
                            rename(ICD10Group = CancerTopographyGroup_ICD10)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Enhance diagnosis entries by auxiliary variables
    DiagnosisEntries <- DiagnosisEntries %>%
                            arrange(InitialDiagnosisDate) %>%
                            mutate(ICD10CodeShort = case_when(str_starts(ICD10Code, "C") ~ str_sub(ICD10Code, end = 3),
                                                              str_starts(ICD10Code, "D") ~ str_sub(ICD10Code, end = 5),
                                                              TRUE ~ ICD10Code),
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
        # Apply rules on identification of associated diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsLikelyAssociated'.
        Candidates <- Candidates %>%
                          mutate(IsLikelyAssociated = eval(parse(text = CompileClassificationCall(TargetFeature = "IsLikelyAssociated",
                                                                                                  PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                  RuleSet = RuleSet_DiagnosisAssociation,
                                                                                                  ValueIfNoRuleMet = FALSE))))

        Associations <- Candidates %>%
                            filter(IsLikelyAssociated == TRUE) %>%
                            mutate(ReferenceDiagnosisID = Reference$DiagnosisID,
                                   InconsistencyCheck = eval(parse(text = CompileClassificationCall(TargetFeature = "InconsistencyCheck",
                                                                                                    PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                    RuleSet = RuleSet_DiagnosisAssociation,
                                                                                                    ValueIfNoRuleMet = "No apparent inconsistency"))),
                                   ImplausibilityCheck = eval(parse(text = CompileClassificationCall(TargetFeature = "ImplausibilityCheck",
                                                                                                     PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                     RuleSet = RuleSet_DiagnosisAssociation,
                                                                                                     ValueIfNoRuleMet = "No apparent implausibility"))),
                                   Relation_ICD10 = eval(parse(text = CompileClassificationCall(TargetFeature = "Relation_ICD10",
                                                                                                PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                RuleSet = RuleSet_DiagnosisAssociation))),
                                   Relation_ICDOTopography = eval(parse(text = CompileClassificationCall(TargetFeature = "Relation_ICDOTopography",
                                                                                                         PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                         RuleSet = RuleSet_DiagnosisAssociation))),
                                   Relation_LocalizationSide = eval(parse(text = CompileClassificationCall(TargetFeature = "Relation_LocalizationSide",
                                                                                                           PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                           RuleSet = RuleSet_DiagnosisAssociation))),
                                   Relation_ICDOMorphology = eval(parse(text = CompileClassificationCall(TargetFeature = "Relation_ICDOMorphology",
                                                                                                         PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                         RuleSet = RuleSet_DiagnosisAssociation))),
                                   Relation_Grading = eval(parse(text = CompileClassificationCall(TargetFeature = "Relation_Grading",
                                                                                                  PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                  RuleSet = RuleSet_DiagnosisAssociation))),
                                   IsLikelyProgression = eval(parse(text = CompileClassificationCall(TargetFeature = "IsLikelyProgression",
                                                                                                     PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                     RuleSet = RuleSet_DiagnosisAssociation))),
                                   IsLikelyRecoding = eval(parse(text = CompileClassificationCall(TargetFeature = "IsLikelyRecoding",
                                                                                                  PredictorFeatures = PredictorFeatures_DiagnosisAssociation,
                                                                                                  RuleSet = RuleSet_DiagnosisAssociation))))


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
