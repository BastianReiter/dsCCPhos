
#' GetCrossTabDS
#'
#' Perform cross tabulation with arbitrary number of table features.
#'
#' Server-side AGGREGATE method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame}
#' @param FeatureNames.S \code{string} - Names of features separated by ','
#' @param RemoveNA.S \code{logical} - Indicating whether missing values should be removed prior to cross tabulation - Default: \code{FALSE}
#'
#' @return A \code{list} containing
#'            \itemize{ \item CrossTab (\code{data.frame})
#'                      \item ChiSq.PValue (\code{numeric}) }
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCrossTabDS <- function(TableName.S,
                          FeatureNames.S,
                          RemoveNA.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(purrr)
  require(stringr)

  # --- For Testing Purposes ---
  # Table <- AugmentationOutput$AugmentedDataSet$Patient
  # Features <- c("Sex", "CountDiagnoses")

  # --- Argument Assertions ---
  assert_that(is.string(TableName.S),
              is.string(FeatureNames.S),
              is.logical(RemoveNA.S))

  #-----------------------------------------------------------------------------

  # Evaluation of table derived from 'TableName.S'
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Decode 'FeatureNames.S' and get separate feature names
  Features <- .decode_tidy_eval(FeatureNames.S, .get_encode_dictionary())
  Features <- strsplit(Features, ",")[[1]] %>% str_trim()

  # Get CCPhos disclosure settings
  DisclosureProfile = dsCCPhos::DisclosureSettings$Profile
  NThreshold <- dsCCPhos::DisclosureSettings$NThreshold

  # Depending on argument 'RemoveNA.S' define option that will control removal of NAs in cross tabulation
  OptionUseNA <- "ifany"
  if (RemoveNA.S == TRUE) { OptionUseNA <- "no" }

  # Get cross tab with joint counts
  CrossTab <- do.call(table, c(Table[Features], list(useNA = OptionUseNA))) %>%
                  as.data.frame() %>%
                  rename(JointCount = "Freq")

  # Calculate marginal counts and add them to 'CrossTab'
  for (featurename in Features)
  {
      MarginalCounts <- CrossTab %>%
                            group_by(across(all_of(featurename))) %>%
                                summarize(!!sym(paste0("MargCount.", featurename)) := sum(JointCount, na.rm = TRUE)) %>%
                            ungroup()

      CrossTab <- CrossTab %>%
                      left_join(MarginalCounts,
                                by = join_by(!!sym(featurename)))
  }


  # If 'DisclosureProfile' is 'loose' lower NThreshold to -1 which effectively prevents subsequent masking
  if (DisclosureProfile == "loose") { NThreshold <- -1 }

  # Mask all Counts that are below 'NThreshold'
  CrossTab <- CrossTab %>%
                  mutate(across(c("JointCount", starts_with("MargCount.")),
                                ~ case_when(.x <= NThreshold ~ TRUE,
                                            .default = FALSE),
                                .names = "IsMasked.{.col}")) %>%
                  mutate(across(c("JointCount", starts_with("MargCount.")),
                                ~ case_when(.x <= NThreshold ~ NA,
                                            .default = .x)))


  # Add relative frequencies (including marginal frequencies)
  # CrossTab <- CrossTab %>%
  #                 mutate(JointRelFreq = JointCount / sum(JointCount, na.rm = TRUE), .after = JointCount) %>%
  #                 mutate(across(starts_with("MargCount."),
  #                               ~ .x / sum(JointCount, na.rm = TRUE),
  #                               .names = "MargRelFreq.{.col}")) %>%
  #                 rename_with(.cols = starts_with("MargRelFreq.MargCount."),
  #                             .fn = ~ str_remove(.x, "MargCount."))


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
                  ChiSq.PValue = ChiSq.PValue))
  } else {
      # !!! TO DO: Implement cases for other Disclosure profiles
      return(list(ChiSq.PValue = ChiSq.PValue))
  }
}
