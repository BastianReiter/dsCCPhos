
#' MapSystemicTherapyRegimen
#'
#' Performs mapping from substances to regimen in systemic cancer therapy
#'
#' @param InputData \code{data.frame} - Containing data on the following features:
#'                      \itemize{ \item  }
#' @param Map.Regimens \code{data.frame}
#' @param Substances.MinimumCongruence \code{double}
#' @param Substances.AllowedCountDifferenceRange \code{numeric vector}
#' @param Substances.AllowAdditionals \code{logical scalar}
#'
#' @return A \code{list}:
#'          \itemize{ \item OutputData - The input \code{data.frame} with additional data
#'                    \item Tracker - A deidentified version of 'OutputData' for reporting }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MapSystemicTherapyRegimen <- function(InputData,
                                      Map.Regimens,
                                      Substances.MinimumCongruence = 0.8,
                                      Substances.AllowedCountDifferenceRange = c(0, 2),
                                      Substances.AllowAdditionals = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # InputData <- Sel.SystemicTherapyRecords.Imputation
  # Map.Regimens <- Res.SystemicTherapy.Regimens
  # Substances.MinimumCongruence <- 0.8
  # Substances.AllowedCountDifferenceRange <- c(0, 2)
  # Substances.AllowAdditionals <- FALSE

  # --- Argument Validation ---
  assert_that(is.data.frame(InputData),
              is.data.frame(Map.Regimens),
              is.numeric(Substances.MinimumCongruence),
              Substances.MinimumCongruence > 0,
              Substances.MinimumCongruence <= 1,
              is.numeric(Substances.AllowedCountDifferenceRange))

  if (length(InputData) == 0 || nrow(InputData) == 0)
  {
      warning("'InputData' is not a valid non-empty data.frame.")
      return(InputData)
  }

  # --- Rename argument to avoid naming conflicts ---
  .Param.Substances.MinimumCongruence <- Substances.MinimumCongruence
  .Param.Substances.AllowedCountDifferenceRange <- Substances.AllowedCountDifferenceRange
  .Param.Substances.AllowAdditionals <- Substances.AllowAdditionals

#-------------------------------------------------------------------------------

  # First, compare substance vector from every row in 'InputData' to all reference Regimen substance vectors and calculate various comparing parameters
  RegimenMapping.Processing <- InputData %>%
                                    mutate(Substances = map(Substances, ~ na.omit(.x))) %>%      # In case 'Substances' vectors were not cleaned of NAs before
                                    filter(map_lgl(Substances, ~ length(.x) > 0 && !all(.x == ".Ineligible"))) %>%      # In case insufficient 'Substances' vectors were not filtered out before
                                    mutate(.Substances.CountDifference = map(Substances,      # How big is the difference in number of substances? If this value is negative, the observed substance number is larger than the one in a reference regimen.
                                                                             \(CurrentSubstances) { map_int(Map.Regimens$Substances,
                                                                                                            \(RegimenSubstances) { length(RegimenSubstances) - length(CurrentSubstances) }) }),
                                           .Substances.Congruence = map(Substances,      # MOST TIME-CONSUMING. This is equivalent to the Jaccard similarity of two vectors (|Intersection| / |Union|).
                                                                        \(CurrentSubstances) { map_dbl(Map.Regimens$Substances,
                                                                                                       \(RegimenSubstances) { IntersectionLength <- length(intersect(CurrentSubstances, RegimenSubstances))
                                                                                                                              return(IntersectionLength / (length(RegimenSubstances) + length(CurrentSubstances) - IntersectionLength)) }) }),      # We want to calculate |Intersection| / |Union|. For efficiency reasons we use |Union| = |X| + |Y| - |Intersection|.
                                           .Substances.NoAdditionals = map(Substances,      # Check if 'CurrentSubstances' contain no substances that are not part of respective reference Regimens
                                                                           \(CurrentSubstances) { map_lgl(Map.Regimens$Substances,
                                                                                                          \(RegimenSubstances) { all(CurrentSubstances[CurrentSubstances != ".Ineligible"] %in% RegimenSubstances) }) }))

  # For further screening which reference Regimen vectors are candidate matches for a given substance vector, calculate various logical vectors and the combine them satisfying criteria specific criteria for a defined matching quality grade
  RegimenMapping.Screening <- RegimenMapping.Processing %>%
                                  mutate(.IsCompliant.Substances.Congruence = map(.Substances.Congruence, ~ .x >= .Param.Substances.MinimumCongruence),
                                         .IsCompliant.Substances.CountDifference = map(.Substances.CountDifference, ~ .x >= min(.Param.Substances.AllowedCountDifferenceRange, na.rm = TRUE) & .x <= max(.Param.Substances.AllowedCountDifferenceRange, na.rm = TRUE)),
                                         .IsCompliant.Substances.NoAdditionals = map(.Substances.NoAdditionals, \(X) { if (.Param.Substances.AllowAdditionals == TRUE) { rep(TRUE, times = length(X)) } else { X } }),
                                         .IsCompliant = pmap(list(.IsCompliant.Substances.Congruence,
                                                                  .IsCompliant.Substances.CountDifference,
                                                                  .IsCompliant.Substances.NoAdditionals),
                                                             ~ ..1 & ..2 & ..3),
                                         .IsMaxSubstanceCongruence = map(.Substances.Congruence, ~ .x == max(.x, na.rm = TRUE)),
                                         .IsPerfectSubstanceCongruence = map(.Substances.Congruence, ~ .x == 1),
                                         .Match.ICD10Code = map(ICD10Code, ~ str_detect(Map.Regimens$Match.ICD10Code, fixed(.x))),
                                         .Match.ICD10Code.Short = map(ICD10Code.Short, ~ str_detect(Map.Regimens$Match.ICD10Code.Short, fixed(.x))),
                                         .Match.ICD10Code.SecondaryUse = map(ICD10Code, ~ str_detect(Map.Regimens$Match.ICD10Code.SecondaryUse, fixed(.x))),
                                         .Match.ICD10Code.Short.SecondaryUse = map(ICD10Code.Short, ~ str_detect(Map.Regimens$Match.ICD10Code.Short.SecondaryUse, fixed(.x))),
                                         .Candidates.Grade1 = pmap(list(.IsPerfectSubstanceCongruence,
                                                                        .Match.ICD10Code,
                                                                        .Match.ICD10Code.Short),
                                                                   ~ which(..1 & (..2 | ..3))),
                                         .Candidates.Grade2 = pmap(list(.IsCompliant,
                                                                        .IsMaxSubstanceCongruence,
                                                                        .Match.ICD10Code,
                                                                        .Match.ICD10Code.Short),
                                                                   ~ which(..1 & ..2 & (..3 | ..4))),
                                         .Candidates.Grade3 = pmap(list(.IsCompliant,
                                                                        .IsMaxSubstanceCongruence,
                                                                        .Match.ICD10Code.SecondaryUse,
                                                                        .Match.ICD10Code.Short.SecondaryUse),
                                                                   ~ which(..1 & ..2 & (..3 | ..4))),
                                         .Candidates.Grade4 = pmap(list(.IsCompliant,
                                                                        .Match.ICD10Code,
                                                                        .Match.ICD10Code.Short),
                                                                   ~ which(..1 & (..2 | ..3))),
                                         .Candidates.Grade5 = pmap(list(.IsCompliant,
                                                                        .Match.ICD10Code.SecondaryUse,
                                                                        .Match.ICD10Code.Short.SecondaryUse),
                                                                   ~ which(..1 & (..2 | ..3))),
                                         .Candidates.Grade6 = map(.IsPerfectSubstanceCongruence, ~ which(.x)))

  # For every given substance vector, identify the best mapping candidates
  RegimenMapping.Candidates <- RegimenMapping.Screening %>%
                                    mutate(Candidates = pmap(list(.Candidates.Grade1,
                                                                  .Candidates.Grade2,
                                                                  .Candidates.Grade3,
                                                                  .Candidates.Grade4,
                                                                  .Candidates.Grade5,
                                                                  .Candidates.Grade6),
                                                             ~ { case_when(length(..1) > 0 ~ list(RegimenMapping.CandidateRows = ..1, RegimenMapping.CandidateCount = length(..1), RegimenMapping.Grade = 1),      # Approach with case_when() enables step-wise screening: Only if no Grade 1 candidates are found, Grade 2 candidates are looked for, and so forth...
                                                                           length(..2) > 0 ~ list(RegimenMapping.CandidateRows = ..2, RegimenMapping.CandidateCount = length(..2), RegimenMapping.Grade = 2),
                                                                           length(..3) > 0 ~ list(RegimenMapping.CandidateRows = ..3, RegimenMapping.CandidateCount = length(..3), RegimenMapping.Grade = 3),
                                                                           length(..4) > 0 ~ list(RegimenMapping.CandidateRows = ..4, RegimenMapping.CandidateCount = length(..4), RegimenMapping.Grade = 4),
                                                                           length(..5) > 0 ~ list(RegimenMapping.CandidateRows = ..5, RegimenMapping.CandidateCount = length(..5), RegimenMapping.Grade = 5),
                                                                           length(..6) > 0 ~ list(RegimenMapping.CandidateRows = ..6, RegimenMapping.CandidateCount = length(..6), RegimenMapping.Grade = 6),
                                                                           .default = list(RegimenMapping.CandidateRows = NULL, RegimenMapping.CandidateCount = 0, RegimenMapping.Grade = NA)) })) %>%
                                    unnest_wider(Candidates) %>%
                                    mutate(RegimenMapping.Choice = case_when(RegimenMapping.CandidateCount == 1 ~ "Distinct",
                                                                              RegimenMapping.CandidateCount > 1 ~ "Indistinct",
                                                                              .default = "None"))

  # Perform Regimen mapping: Identify Regimen values in map table based on candidate rows
  RegimenMapping <- RegimenMapping.Candidates %>%
                        select(c(names(InputData),
                                 RegimenMapping.CandidateRows,
                                 RegimenMapping.CandidateCount,
                                 RegimenMapping.Grade,
                                 RegimenMapping.Choice)) %>%
                        mutate(RegimenMapping.Candidates = map(RegimenMapping.CandidateRows, ~ Map.Regimens$Regimen[.x]),
                               Regimen.Mapped = map2_chr(.x = RegimenMapping.Choice,
                                                         .y = RegimenMapping.Candidates,
                                                         ~ ifelse(.x == "Distinct", .y, NA)))

  # Create 'Tracker' for reporting (does not contain any patient- or diagnosis-identifying data)
  Tracker <- RegimenMapping %>%
                  select(Substances,
                         ICD10Code,
                         RegimenMapping.Choice,
                         RegimenMapping.Candidates,
                         Regimen.Mapped,
                         RegimenMapping.Grade)


  # Create 'OutputData'
  OutputData <- RegimenMapping %>%
                    select(c(names(InputData),
                             Regimen.Mapped,
                             RegimenMapping.Grade))

  # Report <- Tracker %>%
  #               group_by(RegimenMapping.Choice, RegimenMapping.Grade) %>%
  #                   summarize(Count = n()) %>%
  #               ungroup() %>%
  #               group_by(RegimenMapping.Choice) %>%
  #                   mutate(Proportion = Count / sum(Count))


#-------------------------------------------------------------------------------
  return(list(OutputData = OutputData,
              Tracker = Tracker))
}
