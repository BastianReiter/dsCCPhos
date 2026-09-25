
#' MapUICCStage
#'
#' Performs mapping from TNM data to UICC stage according to TNM classification
#'
#' @param InputData \code{data.frame} - Containing data on the following features:
#'                      \itemize{ \item ICD10Code
#'                                \item ICD10Code.Short
#'                                \item ICDOTopographyCode
#'                                \item ICDOTopographyCode.Short
#'                                \item ICDOMorphologyCode
#'                                \item ICDOMorphologyCode.Short
#'                                \item ICDOMorphologyHistologyCode
#'                                \item Grading
#'                                \item TNM.T
#'                                \item TNM.N
#'                                \item TNM.M
#'                                \item TNMVersion }
#' @param Res.TNMGroupMapping \code{data.frame}
#' @param Res.UICCStageMapping \code{data.frame}
#' @param AcceptableTNMCongruence \code{double} - The threshold defining how much congruence in fuzzy matching is needed to accept a match. Given as a ratio number (Count of matching features in relation to number of all features used for matching). Must be a value between 0 and 1.
#'
#' @return The input \code{data.frame} with additional data
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MapUICCStage <- function(InputData,
                         Res.TNMGroupMapping,
                         Res.UICCStageMapping,
                         AcceptableTNMCongruence = 0.8)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # InputData <- Sel.StagingRecords
  # Res.TNMGroupMapping <- Res.UICCMapping$TNMGroupMapping
  # Res.UICCStageMapping <- Res.UICCMapping$UICCStageMapping
  # AcceptableTNMCongruence <- 0.8

  # --- Argument Validation ---
  assert_that(is.data.frame(InputData),
              is.data.frame(Res.TNMGroupMapping),
              is.data.frame(Res.UICCStageMapping),
              is.numeric(AcceptableTNMCongruence),
              AcceptableTNMCongruence > 0,
              AcceptableTNMCongruence <= 1)

  if (length(InputData) == 0 || nrow(InputData) == 0)
  {
      warning("'InputData' is not a valid non-empty data.frame.")
      return(InputData)
  }

  # --- Rename argument to avoid naming conflicts ---
  .Param.AcceptableTNMCongruence <- AcceptableTNMCongruence

#-------------------------------------------------------------------------------

# --- AUXILIARY FUNCTIONS ------------------------------------------------------

  f.IsCompliant.Inclusion <- function(x, Y)
  {
      if (sum(!is.na(Y)) == 0) return(rep(TRUE, length(Y)))
      Detection <- str_detect(Y, fixed(x))
      Logical <- coalesce(Detection, TRUE)   # Replace NAs with 'TRUE'
      return(Logical)
  }

  f.IsCompliant.Exclusion <- function(x, Y)
  {
      if (sum(!is.na(Y)) == 0) return(rep(TRUE, length(Y)))
      Detection <- str_detect(Y, fixed(x), negate = TRUE)
      Logical <- coalesce(Detection, TRUE)   # Replace NAs with 'TRUE'
      return(Logical)
  }

  f.IsCompliant.Inclusion.Expression <- function(x, Y)
  {
      if (sum(!is.na(Y)) == 0) return(rep(TRUE, length(Y)))
      Linkage <- paste0(x, Y)
      NAInterpretation <- case_when(Linkage == "NANA" ~ "TRUE",      # Case when both input and expression are NA
                                    str_starts(Linkage, "NA") ~ "FALSE",      # Case when input value is NA although there is a non-NA expression
                                    str_ends(Linkage, "NA") ~ "TRUE",      # Case when only expression is NA
                                    .default = Linkage)      # Case when neither input value nor expression are NA
      Evaluation <- map_lgl(NAInterpretation, \(string) eval(str2lang(string)))
      Logical <- coalesce(Evaluation, TRUE)
      return(Logical)
  }

  f.TNMMatching <- function(x, Y)
  {
      if (sum(!is.na(Y)) == 0) return(rep(TRUE, length(Y)))
      Match <- case_when(x == Y ~ TRUE,
                         is.na(x) & is.na(Y) ~ TRUE,
                         !is.na(x) & is.na(Y) ~ TRUE,
                         is.na(x) & !is.na(Y) ~ FALSE,
                         .default = FALSE)
      return(Match)
  }

#-------------------------------------------------------------------------------


  # 1) For each record, attempt to find the correct TNMGroup
  TNMGroupMatching.Screening <- InputData %>%
                                    mutate(.Match.TNMVersion = map(TNMVersion, ~ str_which(Res.TNMGroupMapping$Match.TNMVersion, fixed(.x))),
                                           .Match.ICDOTopographyCode = map(ICDOTopographyCode, ~ str_which(Res.TNMGroupMapping$Match.ICDOTopographyCode, fixed(.x))),
                                           .Match.ICDOTopographyCode.Short = map(ICDOTopographyCode.Short, ~ str_which(Res.TNMGroupMapping$Match.ICDOTopographyCode.Short, fixed(.x)))) %>%
                                    mutate(.CandidateMapRows = pmap(list(.Match.TNMVersion,
                                                                         .Match.ICDOTopographyCode,
                                                                         .Match.ICDOTopographyCode.Short),
                                                                    ~ union(intersect(..1, ..2),
                                                                            intersect(..1, ..3))),
                                           .CountCandidateMapRows = map_int(.CandidateMapRows, \(X) length(X)))


  # 1A) Case when distinct TNMGroup was already found only based on basic matching criteria
  TNMGroupMatching.Simple <- TNMGroupMatching.Screening %>%
                                  filter(.CountCandidateMapRows == 1) %>%
                                  mutate(TNMGroup = NA)

  if (nrow(TNMGroupMatching.Simple) > 0)      # The mutating statements only work on a non-empty data.frame (and on rows that have non-empty vectors in '.CandidateMapRows')
  {
      TNMGroupMatching.Simple <- TNMGroupMatching.Simple %>%
                                      mutate(.CandidateMapRows = unlist(.CandidateMapRows),
                                             TNMGroup = Res.TNMGroupMapping$TNMGroup[.CandidateMapRows])
  }


  # 1B) Case when screening yielded multiple TNMGroup candidates and further criteria need to be assessed
  TNMGroupMatching.Complex <- TNMGroupMatching.Screening %>%
                                  filter(.CountCandidateMapRows > 1) %>%
                                  mutate(TNMGroup = NA)

  if (nrow(TNMGroupMatching.Complex) > 0)
  {
      TNMGroupMatching.Complex <- TNMGroupMatching.Complex %>%
                                      mutate(.IsCompliant.Inclusion.ICD10Code.Short = map2(.x = ICD10Code.Short,
                                                                                           .y = .CandidateMapRows,
                                                                                           ~ f.IsCompliant.Inclusion(.x, Res.TNMGroupMapping$Inclusion.ICD10Code.Short[.y])),
                                             .IsCompliant.Inclusion.ICDOMorphologyHistologyCode = map2(.x = ICDOMorphologyHistologyCode,
                                                                                                       .y = .CandidateMapRows,
                                                                                                       ~ f.IsCompliant.Inclusion(.x, Res.TNMGroupMapping$Inclusion.ICDOMorphologyHistologyCode[.y])),
                                             .IsCompliant.Inclusion.PatientAgeAtStaging = map2(.x = PatientAgeAtStaging,
                                                                                               .y = .CandidateMapRows,
                                                                                               ~ f.IsCompliant.Inclusion.Expression(.x, Res.TNMGroupMapping$Inclusion.PatientAgeAtStaging[.y])),
                                             .IsCompliant.Exclusion.ICDOTopographyCode = map2(.x = ICDOTopographyCode,
                                                                                              .y = .CandidateMapRows,
                                                                                              ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICDOTopographyCode[.y])),
                                             .IsCompliant.Exclusion.ICD10Code.Short = map2(.x = ICD10Code.Short,
                                                                                           .y = .CandidateMapRows,
                                                                                           ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICD10Code.Short[.y])),
                                             .IsCompliant.Exclusion.ICDOMorphologyHistologyCode = map2(.x = ICDOMorphologyHistologyCode,
                                                                                                       .y = .CandidateMapRows,
                                                                                                       ~ f.IsCompliant.Exclusion(.x, Res.TNMGroupMapping$Exclusion.ICDOMorphologyHistologyCode[.y]))) %>%
                                      mutate(.MatchedSubRows = pmap(list(.IsCompliant.Inclusion.ICD10Code.Short,
                                                                         .IsCompliant.Inclusion.ICDOMorphologyHistologyCode,
                                                                         .IsCompliant.Inclusion.PatientAgeAtStaging,
                                                                         .IsCompliant.Exclusion.ICDOTopographyCode,
                                                                         .IsCompliant.Exclusion.ICD10Code.Short,
                                                                         .IsCompliant.Exclusion.ICDOMorphologyHistologyCode),
                                                                    ~ which(..1 & ..2 & ..3 & ..4 & ..5 & ..6)),
                                             .CountMatchedSubRows = map_int(.MatchedSubRows, \(X) length(X))) %>%
                                      filter(.CountMatchedSubRows == 1) %>%
                                      mutate(.MatchedCandidateRow = map2_int(.x = .CandidateMapRows,
                                                                             .y = .MatchedSubRows,
                                                                             ~ .x[.y]),
                                             TNMGroup = Res.TNMGroupMapping$TNMGroup[.MatchedCandidateRow])
  }

  # 1c) Select only relevant variables and row-bind both data.frames
  TNMGroupMatched <- bind_rows(select(TNMGroupMatching.Simple, all_of(c(names(InputData), "TNMGroup"))),
                               select(TNMGroupMatching.Complex, all_of(c(names(InputData), "TNMGroup"))))


  # 2) Employ TNMGroup-specific TNM-to-UICC-stage mapping
  UICCStageMatching <- TNMGroupMatched %>%
                            mutate(.Match.TNM.T = pmap(list(TNM.T,
                                                            TNMGroup,
                                                            TNMVersion),
                                                       ~ f.TNMMatching(x = ..1,      # x-value: The actual TNM.T value
                                                                       Y = subset(Res.UICCStageMapping,      # Y-vector: The TNM.T values of the mapping table for a specific TNMGroup and a TNMVersion
                                                                                  TNMGroup == ..2 & TNMVersion == ..3)$TNM.T)),
                                   .Match.TNM.T.Short = pmap(list(TNM.T.Short,
                                                                  TNMGroup,
                                                                  TNMVersion),
                                                             ~ f.TNMMatching(x = ..1,
                                                                             Y = subset(Res.UICCStageMapping,
                                                                                        TNMGroup == ..2 & TNMVersion == ..3)$TNM.T)),
                                   .Match.TNM.N = pmap(list(TNM.N,
                                                            TNMGroup,
                                                            TNMVersion),
                                                       ~ f.TNMMatching(x = ..1,
                                                                       Y = subset(Res.UICCStageMapping,
                                                                                  TNMGroup == ..2 & TNMVersion == ..3)$TNM.N)),
                                   .Match.TNM.N.Short = pmap(list(TNM.N.Short,
                                                                  TNMGroup,
                                                                  TNMVersion),
                                                             ~ f.TNMMatching(x = ..1,
                                                                             Y = subset(Res.UICCStageMapping,
                                                                                        TNMGroup == ..2 & TNMVersion == ..3)$TNM.N)),
                                   .Match.TNM.M = pmap(list(TNM.M,
                                                            TNMGroup,
                                                            TNMVersion),
                                                        ~ f.TNMMatching(x = ..1,
                                                                        Y = subset(Res.UICCStageMapping,
                                                                                   TNMGroup == ..2 & TNMVersion == ..3)$TNM.M)),
                                   .Match.TNM.M.Short = pmap(list(TNM.M.Short,
                                                                  TNMGroup,
                                                                  TNMVersion),
                                                             ~ f.TNMMatching(x = ..1,
                                                                             Y = subset(Res.UICCStageMapping,
                                                                                        TNMGroup == ..2 & TNMVersion == ..3)$TNM.M)),
                                   .Match.TNM.S = pmap(list(TNM.S,
                                                            TNMGroup,
                                                            TNMVersion),
                                                       ~ f.TNMMatching(x = ..1,
                                                                       Y = subset(Res.UICCStageMapping,
                                                                                  TNMGroup == ..2 & TNMVersion == ..3)$TNM.S)),
                                   .Match.Grading = pmap(list(Grading,
                                                              TNMGroup,
                                                              TNMVersion),
                                                         ~ f.TNMMatching(x = ..1,
                                                                         Y = subset(Res.UICCStageMapping,
                                                                                    TNMGroup == ..2 & TNMVersion == ..3)$Grading))) %>%
                            mutate(.Row.ExactMatch = pmap_int(list(.Match.TNM.T,
                                                                   .Match.TNM.N,
                                                                   .Match.TNM.M,
                                                                   .Match.TNM.S,
                                                                   .Match.Grading),
                                                              ~ { ExactMatchRow <- which(..1 & ..2 & ..3 & ..4 & ..5)      # Returns a vector
                                                                  ifelse(length(ExactMatchRow) == 1, ExactMatchRow, NA) }),
                                   .Row.BestFuzzyMatch = pmap_int(list(.Match.TNM.T,
                                                                       .Match.TNM.T.Short,
                                                                       .Match.TNM.N,
                                                                       .Match.TNM.N.Short,
                                                                       .Match.TNM.M,
                                                                       .Match.TNM.M.Short,
                                                                       .Match.TNM.S,
                                                                       .Match.Grading),
                                                                  ~ which.max(..1 + ..2 + ..3 + ..4 + ..5 + ..6 + ..7 + ..8)),
                                   .Row.BestFuzzyMatch.Congruence = pmap(list(.Match.TNM.T,
                                                                              .Match.TNM.T.Short,
                                                                              .Match.TNM.N,
                                                                              .Match.TNM.N.Short,
                                                                              .Match.TNM.M,
                                                                              .Match.TNM.M.Short,
                                                                              .Match.TNM.S,
                                                                              .Match.Grading),
                                                                         ~ max((..1 + ..2 + ..3 + ..4 + ..5 + ..6 + ..7 + ..8) / 8)),
                                   .ChosenRow = case_when(!is.na(.Row.ExactMatch) ~ .Row.ExactMatch,
                                                          .Row.BestFuzzyMatch.Congruence >= .Param.AcceptableTNMCongruence ~ .Row.BestFuzzyMatch,
                                                          .default = NA),
                                   UICCStage.Mapped = pmap_chr(list(TNMGroup,
                                                                    TNMVersion,
                                                                    .ChosenRow),
                                                               ~ subset(Res.UICCStageMapping,
                                                                        TNMGroup == ..1 & TNMVersion == ..2)$UICCStage[..3]),
                                   UICCStage.Mapped.Category = case_when(str_starts(UICCStage.Mapped, "0") ~ "0",
                                                                                    UICCStage.Mapped %in% c("I", "IS") | str_starts(UICCStage.Mapped, "IA|IB|IC") ~ "I",
                                                                                    UICCStage.Mapped == "II" | str_starts(UICCStage.Mapped, "IIA|IIB|IIC") ~ "II",
                                                                                    UICCStage.Mapped == "III" | str_starts(UICCStage.Mapped, "IIIA|IIIB|IIIC|IIID") ~ "III",
                                                                                    str_starts(UICCStage.Mapped, "IV") ~ "IV",
                                                                                    .default = NA_character_))

  # 3) Output complete InputData with added 'TNMGroup', 'UICCStage.Mapped' and 'UICCStage.Mapped.Category'
  Output <- InputData %>%
                left_join(UICCStageMatching, by = names(InputData)) %>%      # Join by all common variables (natural join)
                select(all_of(c(names(InputData),
                                "TNMGroup",
                                "UICCStage.Mapped",
                                "UICCStage.Mapped.Category")))

#-------------------------------------------------------------------------------
  return(Output)
}
