
#' ClassifyDiagnosisRedundancies
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' In patients with multiple diagnosis entries: Identify and classify redundant diagnosis entries.
#'
#' @param DiagnosisEntries Data frame (partially curated) that (usually) contains multiple diagnosis entries
#' @param RulesProfile String | Profile name of rule set defined in \code{\link{RuleSet_DiagnosisRedundancies}
#' @param ProgressBarObject Optionally pass object of type progress::progress_bar to display progress
#'
#' @return Data frame of classified redundant diagnosis entries
#' @export
#'
#' @examples
#' @author Bastian Reiter
ClassifyDiagnosisRedundancies <- function(DiagnosisEntries,
                                          RulesProfile = "Default",
                                          ProgressBarObject = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(dsCCPhos)
    require(stringr)
    require(tidyr)


    # For function testing purposes
    # DiagnosisEntries <- df_CDS_Diagnosis %>% filter(PatientID == "Pat_10013")
    # RulesProfile = "Default"
    # print(DiagnosisEntries$DiagnosisID[1])


    # Update progress bar object, if assigned in function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(ProgressBarObject)) { ProgressBarObject$tick() }


    # Load meta data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # a) Rule set for classification of redundant diagnosis entries
    RuleSet_DiagnosisRedundancies <- dsCCPhos::RuleSet_DiagnosisRedundancies %>%
                                          filter(Profile == RulesProfile)

    # b) Vector of names of features that determine identification of redundant diagnosis entry
    PredictorFeatures_DiagnosisRedundancies = c("InitialDiagnosisDate",
                                                "ICD10Code",
                                                "ICDOTopographyCode",
                                                "LocalizationSide",
                                                "HistologyDate",
                                                "ICDOMorphologyCode",
                                                "Grading")
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Arrange diagnosis entries by InitialDiagnosisDate and lowest number of relevant NAs
    DiagnosisEntries <- DiagnosisEntries %>%
                            rowwise() %>%
                            mutate(CountRelevantNAs = sum(is.na(across(all_of(PredictorFeatures_DiagnosisRedundancies))))) %>%
                            ungroup() %>%
                            arrange(InitialDiagnosisDate, CountRelevantNAs)

    # First reference diagnosis
    Reference <- DiagnosisEntries %>%
                      slice_min(tibble(InitialDiagnosisDate, HistologyDate)) %>%      # Select earliest diagnosis (or diagnoses)
                      first()

    # First set of candidates that are compared to reference diagnosis
    Candidates <- DiagnosisEntries %>%
                      filter(DiagnosisID != Reference$DiagnosisID)

    # Add CountDeviatingValues (calculated below) to vector of predicting feature names
    PredictorFeatures_DiagnosisRedundancies <- c("CountDeviatingValues", PredictorFeatures_DiagnosisRedundancies)

    # Initiation of Output data frame
    Output <- NULL

    if (nrow(Candidates) == 0)      # In case the input data frame consists of one single diagnosis entry the loop below will not be executed, therefore the Output is simply the one diagnosis entry
    {
        Output <- Reference
    } else
    {
        # Loop through all remaining "unassociated" diagnoses, as long as there are still candidates that are likely redundant with reference
        while (nrow(Candidates) > 0)
        {
            # Apply rules on identification of redundant diagnosis entries set by predefined data frame. Creates TRUE values in new variable 'IsLikelyRedundant'.
            Candidates <- Candidates %>%
                              rowwise() %>%
                                  mutate(CountDeviatingValues = CountDeviations(CandidateEntry = pick(all_of(setdiff(PredictorFeatures_DiagnosisRedundancies, "CountDeviatingValues"))),
                                                                                ReferenceEntry = Reference,
                                                                                ComparedFeatures = setdiff(PredictorFeatures_DiagnosisRedundancies, "CountDeviatingValues"))) %>%

                              ungroup() %>%
                              mutate(IsLikelyRedundant = eval(parse(text = CompileClassificationCall(TargetFeature = "IsLikelyRedundant",
                                                                                                     PredictorFeatures = PredictorFeatures_DiagnosisRedundancies,
                                                                                                     RuleSet = RuleSet_DiagnosisRedundancies,
                                                                                                     ValueIfNoRuleMet = FALSE))))

            # Filter for diagnoses classified as redundancies
            # Collect DiagnosisIDs and number of redundancies
            Redundancies <- Candidates %>%
                              filter(IsLikelyRedundant == TRUE) %>%
                              mutate(CountRedundancies = n(),
                                     RedundantIDs = list(DiagnosisID),
                                     RedundantOldIDs = list(OldDiagnosisID))

            # If there are missings in Reference diagnosis, take values from redundant entries if available there
            # and consolidate in one joint diagnosis entry
            JointDiagnosis <- bind_rows(Reference, Redundancies) %>%
                                  select(-c(CountDeviatingValues,
                                            IsLikelyRedundant)) %>%
                                  fill(everything(), .direction = "down") %>%
                                  fill(everything(), .direction = "up") %>%
                                  filter(DiagnosisID == Reference$DiagnosisID)

            # Add consolidated JointDiagnosis to Output
            Output <- bind_rows(Output,
                                JointDiagnosis)

            # Assign new reference diagnosis
            Reference <- Candidates %>%
                              filter(IsLikelyRedundant == FALSE)

            # If a new Reference exists
            if (nrow(Reference) > 0)
            {
                Reference <- Reference %>%
                                  slice_min(tibble(InitialDiagnosisDate, HistologyDate)) %>%
                                  first()
            }

            # Assign new candidates for redundant diagnoses
            Candidates <- Candidates %>%
                              filter(IsLikelyRedundant == FALSE) %>%
                              filter(DiagnosisID != Reference$DiagnosisID)
        }

        # If there are no more candidates (end of while-loop) but a single Reference entry
        if (nrow(Reference) > 0)
        {
            # Delete auxiliary features
            Reference <- Reference %>%
                              select(-c(CountDeviatingValues,
                                        IsLikelyRedundant))
            # Add last Reference to Output
            Output <- bind_rows(Output,
                                Reference)
        }
    }

    # Process Output data frame
    Output <- Output %>%
                  select(-CountRelevantNAs)


    return(as.data.frame(Output))
}
