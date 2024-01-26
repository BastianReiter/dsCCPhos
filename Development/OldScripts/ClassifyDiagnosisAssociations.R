
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
ClassifyDiagnosisAssociations <- function(DiagnosisEntries,
                                          Method = "Rule-based",
                                          ClassificationRules = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(stringr)
    require(tidyr)

    # For function testing purposes
    #DiagnosisEntries <- df_CDS_Diagnosis %>% filter(PatientID == "Pat_13641")

    # Load meta data
    #~~~~~~~~~~~~~~~
    # a) Cancer diagnosis groups as proposed by ICD-10
    Meta_ICD10Group <- dsCCPhos::Meta_CancerGrouping %>%
                            select(ICD10CodeShort,
                                   CancerTopographyGroup_ICD10) %>%
                            rename(ICD10Group = CancerTopographyGroup_ICD10)

    # b) Rules for identification of associated diagnosis entries
    Rules_DiagnosisAssociation <- dsCCPhos::Meta_DiagnosisAssociation

    # c) Rules for classification of associated diagnosis entries
    Rules_DiagnosisAssociationClassification <- dsCCPhos::Meta_DiagnosisAssociationClassification


    # Enhance diagnosis entries by auxiliary variables
    DiagnosisEntries <- DiagnosisEntries %>%
                            arrange(InitialDiagnosisDate) %>%
                            mutate(ICD10CodeShort = str_sub(ICD10Code, end = 3),
                                   ICDOTopographyCodeShort = str_sub(ICDOTopographyCode, end = 3),
                                   ICDOMorphologyCodeShort = str_sub(ICDOMorphologyCode, end = 4)) %>%
                            left_join(Meta_ICD10Group, by = join_by(ICD10CodeShort))


    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      slice_min(InitialDiagnosisDate, HistologyDate) %>%      # Select oldest diagnosis (or diagnoses)
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
                          mutate(#--- Boolean variables that inform about equality relation of variable values between reference and candidate ---
                                 IsSame_ICD10Code = ICD10Code[row_number()] == Reference$ICD10Code,
                                 IsSame_ICD10CodeShort = ICD10CodeShort[row_number()] == Reference$ICD10CodeShort,
                                 IsSame_ICD10Group = ICD10Group[row_number()] == Reference$ICD10Group,
                                 IsSame_ICDOTopographyCode = ICDOTopographyCode[row_number()] == Reference$ICDOTopographyCode,
                                 IsSame_ICDOTopographyCodeShort = ICDOTopographyCodeShort[row_number()] == Reference$ICDOTopographyCodeShort,
                                 IsSame_LocalizationSide = LocalizationSide[row_number()] == Reference$LocalizationSide,
                                 IsSame_ICDOMorphologyCode = ICDOMorphologyCode[row_number()] == Reference$ICDOMorphologyCode,
                                 IsSame_ICDOMorphologyCodeShort = ICDOMorphologyCodeShort[row_number()] == Reference$ICDOMorphologyCodeShort,
                                 IsSame_Grading = Grading[row_number()] == Reference$Grading,
                                 #--- Character variables that specify relation of variable values between reference and candidate ---
                                 Relation_ICD10Code = if(eval(parse(paste0(Reference$ICD10Code, "->", ICD10Code[row_number()]),
                                 Relation_ICD10CodeShort = paste0(Reference$ICD10CodeShort, "->", ICD10CodeShort[row_number()]),
                                 Relation_ICD10Group = paste0(Reference$ICD10Group, "->", ICD10Group[row_number()]),
                                 Relation_ICDOTopographyCode = paste0(Reference$ICDOTopographyCode, "->", ICDOTopographyCode[row_number()]),
                                 Relation_ICDOTopographyCodeShort = paste0(Reference$ICDOTopographyCodeShort, "->", ICDOTopographyCodeShort[row_number()]),
                                 Relation_LocalizationSide = paste0(Reference$LocalizationSide, "->", LocalizationSide[row_number()]),
                                 Relation_ICDOMorphologyCode = paste0(Reference$ICDOMorphologyCode, "->", ICDOMorphologyCode[row_number()]),
                                 Relation_ICDOMorphologyCodeShort = paste0(Reference$ICDOMorphologyCodeShort, "->", ICDOMorphologyCodeShort[row_number()]),
                                 Relation_Grading = paste0(Reference$Grading, "->", Grading[row_number()]))

        # Apply rules on identification of associated diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsLikelyAssociated'.
        Candidates <- Candidates %>%
                          left_join(Rules_DiagnosisAssociation, by = join_by(starts_with("Relation"))) %>%
                          mutate(IsLikelyAssociated = ifelse(is.na(IsLikelyAssociated), FALSE, IsLikelyAssociated))      # Replace NA values with 'FALSE'


        Associations <- Candidates %>%
                            filter(IsLikelyAssociated == TRUE) %>%
                            mutate(ReferenceDiagnosisID = Reference$DiagnosisID) %>%
                            left_join(Rules_DiagnosisAssociationClassification, by = join_by(starts_with("Relation")))
                                   # Relation_ICD10Code = case_when(IsSameICD10Code ~ "Same",
                                   #                                IsSameICD10CodeShort ~ "Same organ",
                                   #                                IsSameICD10TopographyGroup ~ "Same group",
                                   #                                TRUE ~ "Different organ"),
                                   # AssociationTopography = case_when(IsSameICDOTopographyCode ~ "Same",
                                   #                                   IsSameICDOTopographyCodeShort ~ "Slight change",
                                   #                                   TRUE ~ "Different topography"),
                                   # AssociationSide = case_when(IsSameLocalizationSide ~ "Same",
                                   #                             (LocalizationSide[row_number()] == "Left" & Reference$LocalizationSide == "Right")
                                   #                                | (LocalizationSide[row_number()] == "Right" & Reference$LocalizationSide == "Left") ~ "Opposite side",
                                   #                             (LocalizationSide[row_number()] %in% c("Left", "Right") & Reference$LocalizationSide == "Midline") ~ "Shift to midline",
                                   #                             (LocalizationSide[row_number()] == "Midline" & Reference$LocalizationSide %in% c("Left", "Right")) ~ "Shift to side",
                                   #                             !IsSameLocalizationSide ~ "Different side information"),
                                   # AssociationMorphology = case_when(IsSameICDOMorphologyCode ~ "Same morphology",
                                   #                                   IsSameICDOMorphologyCodeShort ~ "Neighbouring morphology",
                                   #                                   # TO DO: Morphology Group
                                   #                                   TRUE ~ "Different topography"),
                                   # AssociationGrading = case_when(IsSameGrading ~ "Same grading",
                                   #                                # TO DO: Direction of grading development
                                   #                                TRUE ~ "Different grading"),
                                   # AssociationInterpretation = case_when(AssociationEntity == "Same entity"
                                   #                                          & AssociationTopography == "Same topography"
                                   #                                          & AssociationSide == "Same side"
                                   #                                          & AssociationMorphology == "Same morphology"
                                   #                                          & AssociationGrading == "Same grading" ~ "Same diagnosis",
                                   #                                       AssociationEntity == "Same entity"
                                   #                                          & AssociationTopography == "Same topography"
                                   #                                          & AssociationSide == "Opposite side" ~ "Progression: Bilateral"),
                                   # IsLikelyProgression = case_when(str_starts(AssociationInterpretation, "Progression") ~ TRUE))

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
