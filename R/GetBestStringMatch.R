
#' GetBestStringMatch
#'
#' Employ fuzzy string matching (using package \code{stringdist}) to try and match a given string to an element in a given set of 'eligible' strings.
#'
#' @param String \code{string}
#' @param EligibleStrings \code{character vector} - A set of valid/eligible strings
#' @param Method \code{string} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param Tolerance \code{double} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If 'String' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, return \code{NA}. Default: 0.3
#' @param StringdistArguments \code{list} - Additional arguments used in \code{stringdist::stringdist()}
#'
#' @return A \code{named vector} with the input string as name and match as value
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetBestStringMatch <- function(String,
                               EligibleStrings,
                               Method = "jw",
                               Tolerance = 0.3,
                               StringdistArguments = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(stringdist)

  # Return NA if String is NA
  if (is.na(String)) { return(NA) }

  # If additional arguments for stringdist::stringdist() are passed through list 'StringdistArguments' check their validity (stringdist can not handle NA arguments)
  if (!is.null(StringdistArguments) & is.list(StringdistArguments))
  {
      # Check if 'weight' argument is valid (Conditions: numeric, 4 values, no NAs)
      Weights <- StringdistArguments$weight
      Weights.Conditions <- is.numeric(Weights) & length(Weights) == 4 & !any(is.na(Weights))
      if (!is.null(Weights) & Weights.Conditions == FALSE) { StringdistArguments$weight <- NULL }

      # Delete any arguments in StringdistArguments that are NA
      StringdistArguments <- StringdistArguments[!is.na(StringdistArguments)]
  }

  # Compile all arguments for stringdist()
  Arguments <- c(list(a = String,
                      b = EligibleStrings,
                      method = Method),
                 StringdistArguments)

  # Compute string distances from current string to all valid strings
  Distances <- do.call(stringdist, Arguments)

  # The current string's lowest distance to any of the valid strings
  lowestdistance <- min(Distances)

  # Identify the valid string with smallest distance to current string
  bestmatch <- EligibleStrings[which.min(Distances)]

  # Depending on the used method 'lowestdistance' can either already be a number between 0 (no distance) and 1 (maximal distance) or it needs to be normalized accordingly to make the 'Tolerance' argument meaningful
  lowestdistance.norm <- lowestdistance      # Methods 'jaccard', 'jw', 'cosine', 'soundex' return distance measures between 0 and 1 already and do not need to be normalized

  if (Method %in% c("osa", "lv", "dl"))      # Length-based edit distances
  {
      lowestdistance.norm <- lowestdistance / max(nchar(String), nchar(bestmatch))
  }
  if (Method %in% c("lcs", "qgram"))
  {
      lowestdistance.norm <- lowestdistance / (nchar(String) + nchar(bestmatch))
  }
  # Note: Method 'hamming' returns Inf for two strings with different numbers of characters and is therefore not supported (because we can not find a maximum that would allow normalization)

  # If the normalized distance is lower or equal than the 'Tolerance' value, the best match is returned (as a named vector with input string as name and match as value), otherwise return NA
  if (lowestdistance.norm <= Tolerance)
  {
      return(setNames(bestmatch, nm = String))

  } else { return(NA) }
}
