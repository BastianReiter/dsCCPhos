
#' MatchDataDS
#'
#' Obtain matched subsets from a given source \code{data.frame}, usually \code{ADS$Patients} from the Augmented Data Set (ADS).
#'
#' Server-side ASSIGN method
#'
#' @param DataFrame.S \code{string} - Name of \code{data.frame} containing source data
#' @param GroupingVariable.S \code{string} - A character string containing the \code{formula} used in \code{MatchIt::matchit()}. Example: 'Treatment ~ CovA + CovB + CovC + ...'. Note: The 'treatment variable' that is stated in the formula must be a binary variable.
#' @param CustomGroupingVariable.S \code{character} -
#' @param MatchingVariables.S \code{character} -
#' @param MatchItArguments.S \code{list} - Elements correspond to arguments of \code{MatchIt::matchit()}
#'
#' @return A ...
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MatchDataDS <- function(DataFrame.S,
                        GroupingVariable.S,
                        CustomGroupingVariable.S = NULL,
                        MatchingVariables.S,
                        MatchItArguments.S = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataFrame.S <- "ADS$Patients"
  # GroupingVariable.S <- "IsDocumentedDeceased"
  # # CustomGroupingVariable.S <-
  # MatchingVariables.S <- c("Gender", "PatientAgeAtDiagnosis", "UICCStageCategory")
  # MatchItArguments.S <- list(method = "nearest",
  #                            distance = "glm")

  # --- Argument Validation ---
  # assert_that(is.data.frame(DataFrame.S),
  #             is.string(GroupingVariable.S))

#-------------------------------------------------------------------------------


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Initial statements -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print starting message
  cat("\n")
  Message <- paste0("Starting Data Matching...")
  cli::cat_bullet(Message, bullet = "star")
  cat("\n")

  # Suppress summarize info messages
  options(dplyr.summarise.inform = FALSE)

  # Initiate Messaging objects
  Messages <- list()


  # Rename 'MatchItArguments.S' argument for better code readability
  MatchItArguments <- MatchItArguments.S

  # If list of 'MatchItArguments' passed to function is incomplete, complete it with default values
  if (is.null(MatchItArguments$method)) { MatchItArguments$method <- "nearest" }
  if (is.null(MatchItArguments$distance)) { MatchItArguments$distance <- "glm" }
  if (is.null(MatchItArguments$link)) { MatchItArguments$link <- "logit" }
  if (is.null(MatchItArguments$estimand)) { MatchItArguments$estimand <- "ATT" }
  if (is.null(MatchItArguments$discard)) { MatchItArguments$discard <- "none" }
  if (is.null(MatchItArguments$ratio)) { MatchItArguments$ratio <- 1 }



  # Eliminate entries in 'DataFrame' that lack values in relevant features ('GroupingVariable' and all 'MatchingVariables')
  DataFrame <- DataFrame %>%
                   filter(if_all(c(GroupingVariable.S, MatchingVariables.S), ~ !is.na(.)))

  # Compile formula from feature names passed in arguments ('GroupingVariable.S ~ MatchingVariables.S')
  MatchingFormula <- stats::as.formula(paste0(GroupingVariable.S, " ~ ", paste0(MatchingVariables.S, collapse = " + ")))


  # Compute 'matchit' object to assess initial covariate (im)balance
  InitialBalance <- MatchIt::matchit(formula = MatchingFormula,
                                     data = DataFrame,
                                     method = NULL,
                                     distance = "glm")

  # Look at statistics of covariate (im)balance
  summary(InitialBalance)

  # Compute 'matchit' object
  MatchingObject <- MatchIt::matchit(formula = MatchingFormula,
                                     data = DataFrame,
                                     method = MatchItArguments$method,
                                     distance = MatchItArguments$distance,
                                     link = MatchItArguments$link,
                                     estimand = MatchItArguments$estimand,
                                     exact = MatchItArguments$exact,
                                     mahvars = MatchItArguments$mahvars,
                                     antiexact = MatchItArguments$antiexact,
                                     discard = MatchItArguments$discard,
                                     ratio = MatchItArguments$ratio)

  # Re-assess covariate (im)balance after matching
  summary(MatchingObject, un = FALSE)

  # Get matched data set
  MatchedDataSet <- MatchIt::match_data(MatchingObject)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plots to assess Matching
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # Plot of propensity score distribution
  plot_MatchingPropensityScorePlot <- plot(MatchingObject, type = "jitter", interactive = FALSE)

  # Plot of variable distributions before and after Matching
  plot_MatchingDistributionPlots <- plot(MatchingObject, type = "density", interactive = FALSE)

  # Love plot for reporting
  plot_MatchingLovePlot <- plot(summary(MatchingObject))

}
