
#' GetCrossTabDS
#'
#' Create cross tab (contingency table) with arbitrary number of table features.
#'
#' Server-side AGGREGATE method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame}
#' @param FeatureNames.S \code{string} - Names of features separated by ','
#'
#' @return A \code{list} containing
#'            \itemize{ \item CrossTab (\code{data.frame})
#'                      \item ChiSq.PValue (\code{numeric}) }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCrossTabDS <- function(TableName.S,
                          FeatureNames.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(purrr)

  # --- For Testing Purposes ---
  # TableName.S <- "ADS_Patient"
  # Features <- c("Gender", "CountDiagnoses")

  # Argument assertions
  assert_that(is.string(TableName.S),
              is.string(FeatureNames.S))

  #-----------------------------------------------------------------------------

  # Evaluation of table derived from 'TableName.S'
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Decode 'FeatureNames.S' and get separate feature names
  Features <- .decode_tidy_eval(FeatureNames.S, .get_encode_dictionary())
  Features <- strsplit(Features, ",")[[1]] %>% str_trim()

  # Get CCPhos disclosure settings
  DisclosureProfile = dsCCPhos::DisclosureSettings$Profile
  NThreshold <- dsCCPhos::DisclosureSettings$NThreshold

  # Get cross tab with option 'useNA' set to 'ifany' and turn counts below 'NThreshold' into NA
  CrossTab <- do.call(table, c(Table[Features], list(useNA = "ifany")))

  # Calculate all marginal absolute frequencies (needed in client-side calculation of relative frequencies)
  MarginalCounts <- lapply(1:length(Features), \(marginnumber) c(margin.table(CrossTab, margin = marginnumber))) %>%
                        setNames(nm = Features)

  # Turn 'CrossTab' into a data.frame and mask all Counts that are below 'NThreshold'
  CrossTab <- CrossTab %>%
                    as.data.frame() %>%
                    rename(Count = "Freq") %>%
                    mutate(NBelowThreshold = case_when(Count <= NThreshold ~ TRUE,
                                                       .default = FALSE),
                           Count = case_when(NBelowThreshold == TRUE ~ NA,
                                             .default = Count))

  # Perform Chi-Squared-Test and retrieve p-value
  ChiSq.PValue <- do.call(table, c(Table[Features])) %>%      # The cross tab used for test ignores NA values
                      as.matrix(.) %>%
                      chisq.test(x = .) %>%
                      pluck("p.value")


  #-----------------------------------------------------------------------------
  if (DisclosureProfile == "loose")
  {
      # Return list with CrossTab and ChiSq.PValue
      return(list(CrossTab = as.data.frame(CrossTab),
                  MarginalCounts = MarginalCounts,
                  ChiSq.PValue = ChiSq.PValue))
  } else {
      # !!! TO DO: Modification of 'CrossTab' when DisclosureProfile 'strict'
      return(list(ChiSq.PValue = ChiSq.PValue))
  }
}
