
#' GetFuzzyStringMatches
#'
#' Employ fuzzy string matching (using package \code{stringdist}) to try and match elements of a character vector to a set of valid strings and return the transformed vector.
#' It is designed to maximize effectiveness and computational efficiency.
#'
#' @param Vector \code{character vector} - Containing original values
#' @param EligibleStrings \code{character vector} - A set of valid/eligible strings
#' @param PreferredMethod \code{string} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param FindBestMethod \code{logical} - Indicating whether this function should try out all available methods and choose the one that yields the best result in terms of highest proportion of correctly matched vector elements. - Default: \code{FALSE}
#' @param Tolerance \code{double} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If a string in 'Vector' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, it is set \code{NA}. Default: 0.3
#' @param Preprocessing.FlattenCase \code{logical} - If set to \code{TRUE} all letters in both the input and valid strings are transformed to lower case
#' @param Preprocessing.RemoveAllWhiteSpace \code{logical} - If set to \code{TRUE} all white space in both the input and valid strings is being removed
#' @param Preprocessing.SquishWhiteSpace \code{logical} - If set to \code{TRUE} all leading and trailing white space in both the input and valid strings is being removed and internal white space removed to a single space
#' @param StringdistArguments \code{list} containing additional arguments used in \code{stringdist::stringdist()}
#'
#' @return \code{character vector} of transformed strings
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetFuzzyStringMatches <- function(Vector,
                                  EligibleStrings,
                                  PreferredMethod = "jw",
                                  FindBestMethod = FALSE,
                                  Tolerance = 0.3,
                                  Preprocessing.FlattenCase = TRUE,
                                  Preprocessing.RemoveAllWhiteSpace = FALSE,
                                  Preprocessing.SquishWhiteSpace = TRUE,
                                  StringdistArguments = list())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringdist)
  require(stringr)

  # --- For testing purposes ---
  # Vector <- DataSet$SystemicTherapy$Substance
  # EligibleStrings <- EligibleValueSets$Substance
  # PreferredMethod <- "jw"
  # FindBestMethod <- FALSE
  # Tolerance <- 0.2
  # Preprocessing.FlattenCase <- TRUE
  # Preprocessing.RemoveAllWhiteSpace <- FALSE
  # Preprocessing.SquishWhiteSpace <- TRUE
  # StringdistArguments = NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # 'Vector' and 'EligibleStrings' should be of type character already, explicitly convert here to make sure
  Vector <- as.character(Vector)
  EligibleStrings <- unique(as.character(EligibleStrings))      # Additionally make sure 'EligibleStrings' only contains unique elements

  # If 'Vector' or 'EligibleStrings' are NULL or of length 0, stop function and return input 'Vector'
  if (length(Vector) == 0 || length(EligibleStrings) == 0) { return(Vector) }

  # Initialize 'VectorTracker' to track processing and matching and to preserve original input vector
  VectorTracker <- tibble(ID = 1:length(Vector),
                          Vector = Vector,
                          NeedsMatching = !is.na(Vector) & !(Vector %in% EligibleStrings),
                          Modification = Vector)

  # If there are no non-NA ineligible strings in 'Vector', stop function and return input 'Vector'
  if (sum(VectorTracker$NeedsMatching) == 0) { return(Vector) }

  # Initialize 'EligibleStringsTracker' to track processing of 'EligibleStrings' (they may be modified to facilitate matching)
  EligibleStringsTracker <- tibble(Original = EligibleStrings,
                                   Modification = EligibleStrings)

  # For every preprocessing step elements in both 'Vector' AND 'EligibleStrings' are being modified to enhance chance for matching
  if (Preprocessing.FlattenCase == TRUE)
  {
      VectorTracker <- VectorTracker %>% mutate(Modification = tolower(Modification))
      EligibleStringsTracker <- EligibleStringsTracker %>% mutate(Modification = tolower(Modification))
  }
  if (Preprocessing.RemoveAllWhiteSpace == TRUE)
  {
      VectorTracker <- VectorTracker %>% mutate(Modification = str_replace_all(Modification, " ", ""))      # Remove ALL white space
      EligibleStringsTracker <- EligibleStringsTracker %>% mutate(Modification = str_replace_all(Modification, " ", ""))
  }
  if (Preprocessing.SquishWhiteSpace == TRUE)
  {
      VectorTracker <- VectorTracker %>% mutate(Modification = str_squish(Modification))      # 'str_squish' removes all leading and trailing white space and reduces all internal white space to a single one
      EligibleStringsTracker <- EligibleStringsTracker %>% mutate(Modification = str_squish(Modification))
  }


  # Filter out non-NA ineligible strings (ie strings that need matching)
  IneligibleStrings <- VectorTracker %>%
                          filter(NeedsMatching == TRUE) %>%
                          distinct(Modification) %>%
                          pull(Modification)


  # Initialize vector for resulting best matches
  BestMatches <- NULL

  # If option 'FindBestMethod' is set to TRUE, try out available methods and choose the one with the highest posterior validity grade
  if (FindBestMethod == TRUE)
  {
      # Suitable methods implemented in 'stringdist::stringdist()'
      Methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")

      # Apply all methods and return results as well as method-specific performance (validity grade)
      MethodApplication <- list()
      FoundPerfectValidityGrade <- FALSE

      i <- 1
      while (FoundPerfectValidityGrade == FALSE & i <= length(Methods))
      {
          CurrentMethod <- Methods[i]

          # Get best matches for every element in 'IneligibleStrings' with the current method
          BestMatches <- IneligibleStrings %>%
                              map_vec(\(string) dsCCPhos::GetBestStringMatch(String = string,
                                                                             EligibleStrings = EligibleStringsTracker$Modification,
                                                                             Method = CurrentMethod,
                                                                             Tolerance = Tolerance,
                                                                             StringdistArguments = StringdistArguments)) %>%
                              setNames(IneligibleStrings)

          # Posterior validity grade: To measure method performance calculate proportion of valid elements in 'BestMatches' after matching
          ValidityGrade <- sum(BestMatches %in% EligibleStringsTracker$Modification, na.rm = TRUE) / length(BestMatches)

          MethodApplication[[CurrentMethod]] <- list(BestMatches = BestMatches,
                                                     ValidityGrade = ValidityGrade)

          # If a method results in a perfect validity grade (so all elements in 'IneligibleStrings' can be matched with one in 'EligibleStringsOriginal'), the while-loop is stopped to save computational cost
          if (ValidityGrade == 1) { FoundPerfectValidityGrade <- TRUE }
          i <- i + 1
      }

      # Transpose list to ease further processing
      MethodApplication <- MethodApplication %>% list_transpose()

      # Determine best method based on highest posterior validity grade
      BestMethod <- Methods[which.max(MethodApplication$ValidityGrade)]

      # Get best matches of best method
      BestMatches <- MethodApplication$BestMatches[[BestMethod]]

  } else {

      # Get best matches for every element in 'IneligibleStrings' with the method given in 'PreferredMethod'
      BestMatches <- IneligibleStrings %>%
                          map_vec(\(string) dsCCPhos::GetBestStringMatch(String = string,
                                                                         EligibleStrings = EligibleStringsTracker$Modification,
                                                                         Method = PreferredMethod,
                                                                         Tolerance = Tolerance,
                                                                         StringdistArguments = StringdistArguments)) %>%
                          setNames(IneligibleStrings)
  }


  # For all rows belonging to 'Vector' elements that need matching
  VectorTracker.Matches <- VectorTracker %>%
                              filter(NeedsMatching == TRUE) %>%
                              mutate(ModificationMatch = BestMatches[Modification]) %>%      # Match every modified vector string to the corresponding eligible string in 'BestMatches' using named vector syntax
                              left_join(EligibleStringsTracker, by = join_by(ModificationMatch == Modification)) %>%      # Turn modified eligible strings back into the original form
                              rename(Match = "Original") %>%
                              mutate(Output = case_when(!is.na(Match) ~ Match,      # Output vector takes matched eligible string unless no match could be found, than it takes original string
                                                        .default = Vector))

  # Row-bind Matches to rest of 'VectorTracker' (so all elements that did not need matching in the first place)
  VectorTracker <- VectorTracker %>%
                        filter(NeedsMatching == FALSE) %>%
                        mutate(Output = Vector) %>%
                        bind_rows(VectorTracker.Matches) %>%
                        arrange(ID)      # This is crucial to maintain original vector element order

  # Return 'Output' vector of 'VectorTracker'
  return(VectorTracker$Output)
}

