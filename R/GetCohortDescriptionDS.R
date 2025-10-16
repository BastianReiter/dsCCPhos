
#' GetCohortDescriptionDS
#'
#' What it does
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param Stage.S \code{string} - Indicating the transformation stage of the described data set, one of "Raw" / "Curated" / "Augmented" - Default: "Augmented"
#'
#' @return \code{list}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCohortDescriptionDS <- function(DataSetName.S = "AugmentedDataSet",
                                   Stage.S = "Augmented")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSet <- AugmentedDataSet

  # --- Argument Validation ---
  assert_that(is.string(DataSetName.S),
              is.string(Stage.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  CohortData <- DataSet$Patient %>%
                    left_join(DataSet$Diagnosis, by = join_by(PatientID))

  CohortDataSingleDiag <- CohortData %>%
                              group_by(PatientID) %>%
                                  arrange(DiagnosisDate, .by_group = TRUE) %>%
                                  slice_head() %>%
                              ungroup()


#-------------------------------------------------------------------------------
# Cohort Size
#-------------------------------------------------------------------------------
  CohortSize <- CohortData %>%
                    summarize(PatientCount = n_distinct(PatientID),
                              DiagnosisCount = n_distinct(DiagnosisID)) %>%
                    mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount)

  # Cohort Size over time
  CohortSize_OverTime <- CohortData %>%
                              mutate(DiagnosisYear = lubridate::year(DiagnosisDate)) %>%
                              group_by(DiagnosisYear) %>%
                                  summarize(PatientCount = n_distinct(PatientID),
                                            DiagnosisCount = n_distinct(DiagnosisID)) %>%
                                  mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount)


#-------------------------------------------------------------------------------
# Age Groups
#-------------------------------------------------------------------------------
  Age <- CohortDataSingleDiag %>%
            select(PatientID, DiagnosisID, DiagnosisDate, PatientAgeAtDiagnosis) %>%
            mutate(AgeGroup = case_when(PatientAgeAtDiagnosis < 18 ~ "< 18",
                                        PatientAgeAtDiagnosis %>% between(18, 24) ~ "18 - 24",
                                        PatientAgeAtDiagnosis %>% between(25, 29) ~ "25 - 29",
                                        PatientAgeAtDiagnosis %>% between(30, 34) ~ "30 - 34",
                                        PatientAgeAtDiagnosis %>% between(35, 39) ~ "35 - 39",
                                        PatientAgeAtDiagnosis %>% between(40, 44) ~ "40 - 44",
                                        PatientAgeAtDiagnosis %>% between(45, 49) ~ "45 - 49",
                                        PatientAgeAtDiagnosis %>% between(50, 54) ~ "50 - 54",
                                        PatientAgeAtDiagnosis %>% between(55, 59) ~ "55 - 59",
                                        PatientAgeAtDiagnosis %>% between(60, 64) ~ "60 - 64",
                                        PatientAgeAtDiagnosis %>% between(65, 69) ~ "65 - 69",
                                        PatientAgeAtDiagnosis %>% between(70, 74) ~ "70 - 74",
                                        PatientAgeAtDiagnosis %>% between(75, 79) ~ "75 - 79",
                                        PatientAgeAtDiagnosis %>% between(80, 84) ~ "80 - 84",
                                        PatientAgeAtDiagnosis %>% between(85, 89) ~ "85 - 89",
                                        PatientAgeAtDiagnosis > 89 ~ "> 89"),
                   AgeGroup = factor(AgeGroup,
                                     levels = c("< 18", "18 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "> 89"),
                                     ordered = TRUE)) %>%
            group_by(AgeGroup) %>%
                summarize(N = n()) %>%
                mutate(Proportion = N / sum(N)) %>%
            ungroup() %>%
            arrange(AgeGroup)


#-------------------------------------------------------------------------------
# Sex Distribution
#-------------------------------------------------------------------------------

  Sex <- CohortDataSingleDiag %>%
              group_by(Sex) %>%
                  summarize(N = n()) %>%
                  mutate(Proportion = N / sum(N))

  Sex_OverTime <- CohortDataSingleDiag %>%
                      mutate(DiagnosisYear = lubridate::year(DiagnosisDate)) %>%
                      group_by(DiagnosisYear, Sex) %>%
                          summarize(N = n()) %>%
                      ungroup() %>%
                      pivot_wider(names_from = Sex,
                                  values_from = N)


#-------------------------------------------------------------------------------
  return(list(CohortSize = CohortSize,
              CohortSize_OverTime = CohortSize_OverTime,
              Sex = Sex,
              Sex_OverTime = Sex_OverTime,
              Age = Age))
}
