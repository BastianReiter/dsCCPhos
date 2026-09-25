
#' MapSystemicTherapyRegimen
#'
#' Performs mapping from substances to regimen in systemic cancer therapy
#'
#' @param InputData \code{data.frame} - Containing data on the following features:
#'                      \itemize{ \item  }
#' @param Res.SystemicTherapy.Regimens \code{data.frame}
#' @param Substances.MinimumCongruence \code{double} - The threshold defining how much congruence in fuzzy matching is needed to accept a match. Given as a ratio number (Count of matching features in relation to number of all features used for matching). Must be a value between 0 and 1.
#'
#' @return The input \code{data.frame} with additional data
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MapSystemicTherapyRegimen <- function(InputData,
                                      Res.SystemicTherapy.Regimens,
                                      Substances.MinimumCongruence = 0.8,
                                      Substances.AllowedCountDifferenceRange = c(0, 2))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # InputData <- Sel.SystemicTherapyRecords.Imputation
  # Res.SystemicTherapy.Regimens <- Res.SystemicTherapy.Regimens
  # Substances.MinimumCongruence <- 0.8
  # Substances.AllowedCountDifferenceRange <- c(0, 2)

  # --- Argument Validation ---
  assert_that(is.data.frame(InputData),
              is.data.frame(Res.SystemicTherapy.Regimens),
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

#-------------------------------------------------------------------------------

  RegimenMatching.Processing <- InputData %>%
                                    mutate(.Substances.CountDifference = map(Substances,      # How big is the difference in number of substances? If this value is negative, the observed substance number is larger than the one in a reference regimen.
                                                                              \(CurrentSubstances) { map_int(Res.SystemicTherapy.Regimens$Substances,
                                                                                                             \(RegimenSubstances) { length(RegimenSubstances) - length(CurrentSubstances) }) }),
                                           .Substances.JaccardSimilarity = map(Substances,
                                                                               \(CurrentSubstances) { map_dbl(Res.SystemicTherapy.Regimens$Substances,
                                                                                                              \(RegimenSubstances) { length(intersect(CurrentSubstances, RegimenSubstances)) / length(union(CurrentSubstances, RegimenSubstances)) }) }))

  RegimenMatching.Screening <- RegimenMatching.Processing %>%
                                    mutate(.IsCompliant.Substances.JaccardSimilarity = map(.Substances.JaccardSimilarity, ~ which(.x >= .Param.Substances.MinimumCongruence)),
                                           .IsCompliant.Substances.CountDifference = map(.Substances.CountDifference, ~ which(.x >= min(.Param.Substances.AllowedCountDifferenceRange, na.rm = TRUE) & .x <= max(.Param.Substances.AllowedCountDifferenceRange, na.rm = TRUE))),
                                           .MaxJaccardSimilarity = map(.Substances.JaccardSimilarity, ~ max(.x, na.rm = TRUE)),
                                           .MaxJaccardSimilarityRows = map2(.x = .Substances.JaccardSimilarity,
                                                                            .y = .MaxJaccardSimilarity,
                                                                            ~ which(.x == .y)),
                                           .PerfectSubstanceCongruenceRows = map(.Substances.JaccardSimilarity, ~ which(.x == 1)),
                                           .Match.ICD10Code = map(ICD10Code, ~ str_which(Res.SystemicTherapy.Regimens$Match.ICD10Code, fixed(.x))),
                                           .Match.ICD10Code.Short = map(ICD10Code.Short, ~ str_which(Res.SystemicTherapy.Regimens$Match.ICD10Code.Short, fixed(.x))),
                                           .PrimaryCandidates.MapRows = pmap(list(.IsCompliant.Substances.JaccardSimilarity,
                                                                                  .IsCompliant.Substances.CountDifference,
                                                                                  .MaxJaccardSimilarityRows,
                                                                                  .Match.ICD10Code,
                                                                                  .Match.ICD10Code.Short),
                                                                             ~ union(reduce(list(..1, ..2, ..3, ..4), intersect),      # Union(Intersect of vectors in first 3 and 4th list, Intersect of vectors in first 4 and 5th list)
                                                                                     reduce(list(..1, ..2, ..3, ..5), intersect))),
                                           .PrimaryCandidates.MapRows.Count = map_int(.PrimaryCandidates.MapRows, ~ length(.x)))


#-------------------------------------------------------------------------------
# Primary candidates
#-------------------------------------------------------------------------------

  # 1A) Primary candidates, simple case: Distinct reference Regimen was already found only based on primary matching criteria
  RegimenMatching.PrimaryCandidates.Simple <- RegimenMatching.Screening %>%
                                                  filter(.PrimaryCandidates.MapRows.Count == 1)

  if (nrow(RegimenMatching.PrimaryCandidates.Simple) > 0)      # The mutating statements only work on a non-empty data.frame (and on rows that have non-empty vectors in '.CandidateMapRows')
  {
      RegimenMatching.PrimaryCandidates.Simple <- RegimenMatching.PrimaryCandidates.Simple %>%
                                                      mutate(.PrimaryCandidates.MapRows = unlist(.PrimaryCandidates.MapRows),
                                                             Regimen = Res.SystemicTherapy.Regimens$Regimen[.PrimaryCandidates.MapRows])
  }


  # 1B) Primary candidates, complex case: Screening yielded multiple Regimen candidates and further criteria need to be assessed
  RegimenMatching.PrimaryCandidates.Complex <- RegimenMatching.Screening %>%
                                                    filter(.PrimaryCandidates.MapRows.Count > 1)

  # if (nrow(RegimenMatching.PrimaryCandidates.Complex) > 0)
  # {
  #     RegimenMatching.PrimaryCandidates.Complex <- RegimenMatching.PrimaryCandidates.Complex %>%
  #
  # }


#-------------------------------------------------------------------------------
# Secondary candidates
#-------------------------------------------------------------------------------

  RegimenMatching.Screening.Secondary <- RegimenMatching.Screening %>%
                                              filter(.PrimaryCandidates.MapRows.Count == 0) %>%
                                              mutate(.Match.ICD10Code.SecondaryUse = map(ICD10Code, ~ str_which(Res.SystemicTherapy.Regimens$Match.ICD10Code.SecondaryUse, fixed(.x))),
                                                     .Match.ICD10Code.Short.SecondaryUse = map(ICD10Code.Short, ~ str_which(Res.SystemicTherapy.Regimens$Match.ICD10Code.Short.SecondaryUse, fixed(.x))),
                                                     .SecondaryCandidates.MapRows = pmap(list(.IsCompliant.Substances.JaccardSimilarity,
                                                                                              .IsCompliant.Substances.CountDifference,
                                                                                              .MaxJaccardSimilarityRows,
                                                                                              .Match.ICD10Code.SecondaryUse,
                                                                                              .Match.ICD10Code.Short.SecondaryUse),
                                                                                         ~ union(reduce(list(..1, ..2, ..3, ..4), intersect),      # Union(Intersect of vectors in first 3 and 4th list, Intersect of vectors in first 4 and 5th list)
                                                                                                 reduce(list(..1, ..2, ..3, ..5), intersect))),
                                                     .SecondaryCandidates.MapRows.Count = map_int(.SecondaryCandidates.MapRows, ~ length(.x)))


  # 2A) Secondary candidates, simple case: Distinct reference Regimen was already found only based on primary matching criteria
  RegimenMatching.SecondaryCandidates.Simple <- RegimenMatching.Screening.Secondary %>%
                                                    filter(.SecondaryCandidates.MapRows.Count == 1)

  if (nrow(RegimenMatching.SecondaryCandidates.Simple) > 0)
  {
      RegimenMatching.SecondaryCandidates.Simple <- RegimenMatching.SecondaryCandidates.Simple %>%
                                                        mutate(.SecondaryCandidates.MapRows = unlist(.SecondaryCandidates.MapRows),
                                                               Regimen = Res.SystemicTherapy.Regimens$Regimen[.SecondaryCandidates.MapRows])
  }


  # 2B) Secondary candidates, complex case: Screening yielded multiple Regimen candidates and further criteria need to be assessed
  RegimenMatching.SecondaryCandidates.Complex <- RegimenMatching.Screening.Secondary %>%
                                                      filter(.SecondaryCandidates.MapRows.Count > 1)

  # if (nrow(RegimenMatching.SecondaryCandidates.Complex) > 0)
  # {
  #     RegimenMatching.SecondaryCandidates.Complex <- RegimenMatching.SecondaryCandidates.Complex %>%
  #                                                         mutate(.IsCompliant.)
  #
  # }


#-------------------------------------------------------------------------------
# Tertiary candidates
#-------------------------------------------------------------------------------

  RegimenMatching.Screening.Tertiary <- RegimenMatching.Screening.Secondary %>%
                                            filter(.SecondaryCandidates.MapRows.Count == 0) %>%
                                            mutate(.TertiaryCandidates.A.MapRows = pmap(list(.IsCompliant.Substances.JaccardSimilarity,
                                                                                           .IsCompliant.Substances.CountDifference,
                                                                                           .Match.ICD10Code,
                                                                                           .Match.ICD10Code.Short),
                                                                                      ~ union(reduce(list(..1, ..2, ..3), intersect),
                                                                                              reduce(list(..1, ..2, ..4), intersect))),
                                                   .TertiaryCandidates.A.MapRows.Count = map_int(.TertiaryCandidates.A.MapRows, ~ length(.x)),
                                                   .TertiaryCandidates.B.MapRows = pmap(list(.IsCompliant.Substances.JaccardSimilarity,
                                                                                           .IsCompliant.Substances.CountDifference,
                                                                                           .Match.ICD10Code.SecondaryUse,
                                                                                           .Match.ICD10Code.Short.SecondaryUse),
                                                                                      ~ union(reduce(list(..1, ..2, ..3), intersect),
                                                                                              reduce(list(..1, ..2, ..4), intersect))),
                                                   .TertiaryCandidates.B.MapRows.Count = map_int(.TertiaryCandidates.B.MapRows, ~ length(.x)))



  RegimenMatching.Screening.Quaternary <- RegimenMatching.Screening.Tertiary %>%
                                              filter(.TertiaryCandidates.A.MapRows.Count == 0 & .TertiaryCandidates.B.MapRows.Count == 0) %>%
                                              mutate(.QuaternaryCandidates.MapRows.Count = map_int(.IsPerfectSubstanceCongruence, ~ length(.x)))

  # For quaternary candidates, only keep the ones with one distinct regimen match
  RegimenMatching.QuaternaryCandidates <- RegimenMatching.Screening.Quaternary %>%
                                              filter(.QuaternaryCandidates.MapRows.Count == 1)





  # # 1B) Case when screening yielded multiple TNMGroup candidates and further criteria need to be assessed
  # TNMGroupMatching.Complex <- TNMGroupMatching.Screening %>%
  #                                 filter(.CountCandidateMapRows > 1) %>%
  #                                 mutate(TNMGroup = NA)
  #
  # if (nrow(TNMGroupMatching.Complex) > 0)
  # {
  #     TNMGroupMatching.Complex <- TNMGroupMatching.Complex %>%
  #                                     mutate(.IsCompliant.Inclusion.ICD10Code.Short = map2(.x = ICD10Code.Short,
  #                                                                                          .y = .CandidateMapRows,
  #                                                                                          ~ f.IsCompliant.Inclusion(.x, Res.TNMGroupMapping$Inclusion.ICD10Code.Short[.y])),
  #                                            .IsCompliant.Inclusion.ICDOMorphologyHistologyCode = map2(.x = ICDOMorphologyHistologyCode,
  #                                                                                                      .y = .CandidateMapRows,
  #                                                                                                      ~ f.IsCompliant.Inclusion(.x, Res.TNMGroupMapping$Inclusion.ICDOMorphologyHistologyCode[.y])),
  #                                            .IsCompliant.Inclusion.PatientAgeAtStaging = map2(.x = PatientAgeAtStaging,
  #                                                                                              .y = .CandidateMapRows,
  #                                                                                              ~ f.IsCompliant.Inclusion.Expression(.x, Res.TNMGroupMapping$Inclusion.PatientAgeAtStaging[.y])),
  #                                            .IsCompliant.Exclusion.ICDOTopographyCode = map2(.x = ICDOTopographyCode,
  #                                                                                             .y = .CandidateMapRows,
  #                                                                                             ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICDOTopographyCode[.y])),
  #                                            .IsCompliant.Exclusion.ICD10Code.Short = map2(.x = ICD10Code.Short,
  #                                                                                          .y = .CandidateMapRows,
  #                                                                                          ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICD10Code.Short[.y])),
  #                                            .IsCompliant.Exclusion.ICDOMorphologyHistologyCode = map2(.x = ICDOMorphologyHistologyCode,
  #                                                                                                      .y = .CandidateMapRows,
  #                                                                                                      ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICDOMorphologyHistologyCode[.y]))) %>%
  #                                     mutate(.MatchedSubRows = pmap(list(.IsCompliant.Inclusion.ICD10Code.Short,
  #                                                                        .IsCompliant.Inclusion.ICDOMorphologyHistologyCode,
  #                                                                        .IsCompliant.Inclusion.PatientAgeAtStaging,
  #                                                                        .IsCompliant.Exclusion.ICDOTopographyCode,
  #                                                                        .IsCompliant.Exclusion.ICD10Code.Short,
  #                                                                        .IsCompliant.Exclusion.ICDOMorphologyHistologyCode),
  #                                                                   ~ which(..1 & ..2 & ..3 & ..4 & ..5 & ..6)),
  #                                            .CountMatchedSubRows = map_int(.MatchedSubRows, \(X) length(X))) %>%
  #                                     filter(.CountMatchedSubRows == 1) %>%
  #                                     mutate(.MatchedCandidateRow = map2_int(.x = .CandidateMapRows,
  #                                                                            .y = .MatchedSubRows,
  #                                                                            ~ .x[.y]),
  #                                            TNMGroup = Res.TNMGroupMapping$TNMGroup[.MatchedCandidateRow])
  # }
  #
  # # 1c) Select only relevant variables and row-bind both data.frames
  # TNMGroupMatched <- bind_rows(select(TNMGroupMatching.Simple, all_of(c(names(InputData), "TNMGroup"))),
  #                              select(TNMGroupMatching.Complex, all_of(c(names(InputData), "TNMGroup"))))
  #
  #
  # # 2) Employ TNMGroup-specific TNM-to-UICC-stage mapping
  # UICCStageMatching <- TNMGroupMatched %>%
  #                           mutate(.Match.TNM.T = pmap(list(TNM.T,
  #                                                           TNMGroup,
  #                                                           TNMVersion),
  #                                                      ~ f.TNMMatching(x = ..1,      # x-value: The actual TNM.T value
  #                                                                      Y = subset(Res.UICCStageMapping,      # Y-vector: The TNM.T values of the mapping table for a specific TNMGroup and a TNMVersion
  #                                                                                 TNMGroup == ..2 & TNMVersion == ..3)$TNM.T)),
  #                                  .Match.TNM.T.Short = pmap(list(TNM.T.Short,
  #                                                                 TNMGroup,
  #                                                                 TNMVersion),
  #                                                            ~ f.TNMMatching(x = ..1,
  #                                                                            Y = subset(Res.UICCStageMapping,
  #                                                                                       TNMGroup == ..2 & TNMVersion == ..3)$TNM.T)),
  #                                  .Match.TNM.N = pmap(list(TNM.N,
  #                                                           TNMGroup,
  #                                                           TNMVersion),
  #                                                      ~ f.TNMMatching(x = ..1,
  #                                                                      Y = subset(Res.UICCStageMapping,
  #                                                                                 TNMGroup == ..2 & TNMVersion == ..3)$TNM.N)),
  #                                  .Match.TNM.N.Short = pmap(list(TNM.N.Short,
  #                                                                 TNMGroup,
  #                                                                 TNMVersion),
  #                                                            ~ f.TNMMatching(x = ..1,
  #                                                                            Y = subset(Res.UICCStageMapping,
  #                                                                                       TNMGroup == ..2 & TNMVersion == ..3)$TNM.N)),
  #                                  .Match.TNM.M = pmap(list(TNM.M,
  #                                                           TNMGroup,
  #                                                           TNMVersion),
  #                                                       ~ f.TNMMatching(x = ..1,
  #                                                                       Y = subset(Res.UICCStageMapping,
  #                                                                                  TNMGroup == ..2 & TNMVersion == ..3)$TNM.M)),
  #                                  .Match.TNM.M.Short = pmap(list(TNM.M.Short,
  #                                                                 TNMGroup,
  #                                                                 TNMVersion),
  #                                                            ~ f.TNMMatching(x = ..1,
  #                                                                            Y = subset(Res.UICCStageMapping,
  #                                                                                       TNMGroup == ..2 & TNMVersion == ..3)$TNM.M)),
  #                                  .Match.TNM.S = pmap(list(TNM.S,
  #                                                           TNMGroup,
  #                                                           TNMVersion),
  #                                                      ~ f.TNMMatching(x = ..1,
  #                                                                      Y = subset(Res.UICCStageMapping,
  #                                                                                 TNMGroup == ..2 & TNMVersion == ..3)$TNM.S)),
  #                                  .Match.Grading = pmap(list(Grading,
  #                                                             TNMGroup,
  #                                                             TNMVersion),
  #                                                        ~ f.TNMMatching(x = ..1,
  #                                                                        Y = subset(Res.UICCStageMapping,
  #                                                                                   TNMGroup == ..2 & TNMVersion == ..3)$Grading))) %>%
  #                           mutate(.Row.ExactMatch = pmap_int(list(.Match.TNM.T,
  #                                                                  .Match.TNM.N,
  #                                                                  .Match.TNM.M,
  #                                                                  .Match.TNM.S,
  #                                                                  .Match.Grading),
  #                                                             ~ { ExactMatchRow <- which(..1 & ..2 & ..3 & ..4 & ..5)      # Returns a vector
  #                                                                 ifelse(length(ExactMatchRow) == 1, ExactMatchRow, NA) }),
  #                                  .Row.BestFuzzyMatch = pmap_int(list(.Match.TNM.T,
  #                                                                      .Match.TNM.T.Short,
  #                                                                      .Match.TNM.N,
  #                                                                      .Match.TNM.N.Short,
  #                                                                      .Match.TNM.M,
  #                                                                      .Match.TNM.M.Short,
  #                                                                      .Match.TNM.S,
  #                                                                      .Match.Grading),
  #                                                                 ~ which.max(..1 + ..2 + ..3 + ..4 + ..5 + ..6 + ..7 + ..8)),
  #                                  .Row.BestFuzzyMatch.Congruence = pmap(list(.Match.TNM.T,
  #                                                                             .Match.TNM.T.Short,
  #                                                                             .Match.TNM.N,
  #                                                                             .Match.TNM.N.Short,
  #                                                                             .Match.TNM.M,
  #                                                                             .Match.TNM.M.Short,
  #                                                                             .Match.TNM.S,
  #                                                                             .Match.Grading),
  #                                                                        ~ max((..1 + ..2 + ..3 + ..4 + ..5 + ..6 + ..7 + ..8) / 8)),
  #                                  .ChosenRow = case_when(!is.na(.Row.ExactMatch) ~ .Row.ExactMatch,
  #                                                         .Row.BestFuzzyMatch.Congruence >= .Param.AcceptableTNMCongruence ~ .Row.BestFuzzyMatch,
  #                                                         .default = NA),
  #                                  UICCStage.Mapped = pmap_chr(list(TNMGroup,
  #                                                                   TNMVersion,
  #                                                                   .ChosenRow),
  #                                                              ~ subset(Res.UICCStageMapping,
  #                                                                       TNMGroup == ..1 & TNMVersion == ..2)$UICCStage[..3]),
  #                                  UICCStage.Mapped.Category = case_when(str_starts(UICCStage.Mapped, "0") ~ "0",
  #                                                                                   UICCStage.Mapped %in% c("I", "IS") | str_starts(UICCStage.Mapped, "IA|IB|IC") ~ "I",
  #                                                                                   UICCStage.Mapped == "II" | str_starts(UICCStage.Mapped, "IIA|IIB|IIC") ~ "II",
  #                                                                                   UICCStage.Mapped == "III" | str_starts(UICCStage.Mapped, "IIIA|IIIB|IIIC|IIID") ~ "III",
  #                                                                                   str_starts(UICCStage.Mapped, "IV") ~ "IV",
  #                                                                                   .default = NA_character_))
  #
  # # 3) Output complete InputData with added 'TNMGroup', 'UICCStage.Mapped' and 'UICCStage.Mapped.Category'
  # Output <- InputData %>%
  #               left_join(UICCStageMatching, by = names(InputData)) %>%      # Join by all common variables (natural join)
  #               select(all_of(c(names(InputData),
  #                               "TNMGroup",
  #                               "UICCStage.Mapped",
  #                               "UICCStage.Mapped.Category")))

#-------------------------------------------------------------------------------
  return(Output)
}
