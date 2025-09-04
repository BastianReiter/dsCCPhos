
#' GetCohortDescriptionDS
#'
#' What it does
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param CCPDataSetType.S \code{string} - Indicating the type of CCP data set that should be described, one of "RDS" / "CDS" / "ADS" - Default: "ADS"
#'
#' @return \code{list}
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCohortDescriptionDS <- function(DataSetName.S = "AugmentedDataSet",
                                   CCPDataSetType.S = "ADS")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Package requirements -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
require(tidyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Evaluate and parse input -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(DataSetName.S))
{
    DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: DataSetName.S must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}


### For testing purposes
# DataSet <- AugmentedDataSet


CohortData <- DataSet$Patient %>%
                  left_join(DataSet$Diagnosis, by = join_by(PatientID))

CohortDataSingleDiag <- CohortData %>%
                            group_by(PatientID) %>%
                                arrange(DiagnosisDate, .by_group = TRUE) %>%
                                slice_head() %>%
                            ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cohort Size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CohortSize <- CohortData %>%
                  summarize(PatientCount = n_distinct(PatientID),
                            DiagnosisCount = n_distinct(DiagnosisID)) %>%
                  mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount)

# Cohort Size over time
CohortSize_OverTime <- CohortData %>%
                            mutate(DiagnosisYear = year(DiagnosisDate)) %>%
                            group_by(DiagnosisYear) %>%
                                summarize(PatientCount = n_distinct(PatientID),
                                          DiagnosisCount = n_distinct(DiagnosisID)) %>%
                                mutate(DiagnosesPerPatient = DiagnosisCount / PatientCount)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age Groups
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Age <- CohortDataSingleDiag %>%
          select(PatientID, DiagnosisID, DiagnosisDate, PatientAgeAtDiagnosis) %>%
          mutate(AgeGroup = case_when(PatientAgeAtDiagnosis < 18 ~ "< 18",
                                      PatientAgeAtDiagnosis %>% between(18, 29) ~ "18 - 29",
                                      PatientAgeAtDiagnosis %>% between(30, 39) ~ "30 - 39",
                                      PatientAgeAtDiagnosis %>% between(40, 49) ~ "40 - 49",
                                      PatientAgeAtDiagnosis %>% between(50, 59) ~ "50 - 59",
                                      PatientAgeAtDiagnosis %>% between(60, 69) ~ "60 - 69",
                                      PatientAgeAtDiagnosis %>% between(70, 79) ~ "70 - 79",
                                      PatientAgeAtDiagnosis %>% between(80, 89) ~ "80 - 89",
                                      PatientAgeAtDiagnosis > 89 ~ "> 89"),
                 AgeGroup = factor(AgeGroup,
                                   levels = c("< 18", "18 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80 - 89", "> 89"),
                                   ordered = TRUE)) %>%
          group_by(AgeGroup) %>%
              summarize(N = n()) %>%
              mutate(Proportion = N / sum(N)) %>%
          ungroup() %>%
          arrange(AgeGroup)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex Distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sex <- CohortDataSingleDiag %>%
            group_by(Sex) %>%
                summarize(N = n()) %>%
                mutate(Proportion = N / sum(N))

Sex_OverTime <- CohortDataSingleDiag %>%
                    mutate(DiagnosisYear = year(DiagnosisDate)) %>%
                    group_by(DiagnosisYear, Sex) %>%
                        summarize(N = n()) %>%
                    ungroup() %>%
                    pivot_wider(names_from = Sex,
                                values_from = N)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return(list(CohortSize = CohortSize,
            CohortSize_OverTime = CohortSize_OverTime,
            Sex = Sex,
            Sex_OverTime = Sex_OverTime,
            Age = Age))
}
