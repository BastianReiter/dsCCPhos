
#' ClassifyDiagnosisRedundancy
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' In patients with multiple diagnosis entries: Identify, classify and remove redundant diagnosis entries.
#'
#' @param DiagnosisEntries Data frame (partially curated) that (usually) contains multiple diagnosis entries
#' @param RuleCalls List | List of unevaluated cplyr::case_when-Statements compiled from rules defined in \code{\link{RuleSet_DiagnosisRedundancy}
#' @param ProgressBarObject Optionally pass object of type progress::progress_bar to display progress
#'
#' @return Data frame of consolidated diagnosis entries
#' @export
#'
#' @examples
#' @author Bastian Reiter
ClassifyDiagnosisRedundancy <- function(DiagnosisEntries,
                                        RuleCalls,
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


    # Create vector of names of features that determine identification of redundant diagnosis entry
    PredictorFeatures = c("InitialDiagnosisDate",
                          "ICD10Code",
                          "ICDOTopographyCode",
                          "LocalizationSide",
                          "HistologyDate",
                          "ICDOMorphologyCode",
                          "Grading")

    # Arrange diagnosis entries by InitialDiagnosisDate and lowest number of relevant NAs
    DiagnosisEntries <- DiagnosisEntries %>%
                            rowwise() %>%
                                mutate(CountRelevantNAs = sum(is.na(across(all_of(PredictorFeatures))))) %>%
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
    PredictorFeatures <- c("CountDeviatingValues", PredictorFeatures)

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
                                  mutate(CountDeviatingValues = CountDeviations(CandidateEntry = pick(all_of(setdiff(PredictorFeatures, "CountDeviatingValues"))),
                                                                                ReferenceEntry = Reference,
                                                                                ComparedFeatures = setdiff(PredictorFeatures, "CountDeviatingValues"))) %>%
                              ungroup() %>%
                              mutate(IsLikelyRedundant = eval(parse(text = RuleCalls[["IsLikelyRedundant"]])))

            # Filter for diagnoses classified as redundancies
            # Collect DiagnosisIDs and number of redundancies
            Redundancies <- Candidates %>%
                              filter(IsLikelyRedundant == TRUE) %>%
                              mutate(CountRedundancies = n(),
                                     RedundantIDs = list(DiagnosisID),
                                     RedundantOriginalIDs = list(OriginalDiagnosisID))

            # If there are missings in Reference diagnosis, take values from redundant entries if available there
            # and consolidate in one joint diagnosis entry
            JointDiagnosis <- bind_rows(Reference, Redundancies) %>%
                                  select(-c(CountDeviatingValues,
                                            IsLikelyRedundant)) %>%
                                  fill(everything(), .direction = "down") %>%      # Partly responsible for slow function performance
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

    # In case no redundancies were classified anywhere, attach empty column "CountRedundancies" for subsequential syntax consistency
    if ("CountRedundancies" %in% names(Output) == FALSE)
    {
        Output <- Output %>%
                      mutate(CountRedundancies = NA)
    }


    return(as.data.frame(Output))
}
