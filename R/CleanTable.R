
#' CleanTable
#'
#' Auxiliary function in \code{dsCCPhos::CurateDataDS()} to perform exclusion of invalid table entries.
#'
#' @param Table \code{data.frame} or \code{tibble} - The table object to be cleaned
#' @param TableNameLookup \code{character} - The table name(s) to be looked up in 'FeatureObligations_RuleSet'. Can be a single string or a character vector.
#' @param RemoveRedundantEntries \code{logical} - Whether redundant entries should be removed
#' @param FeatureObligations_RuleSet \code{data.frame}
#' @param FeatureObligations_Profile \code{character}
#'
#' @return \code{tibble} - Clean table
#' @export
#'
#' @author Bastian Reiter
CleanTable <- function(Table,
                       TableNameLookup,
                       RemoveRedundantEntries = TRUE,
                       FeatureObligations_RuleSet,
                       FeatureObligations_Profile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(rlang)

    ### For testing purposes
    # Table <- DataSet$BioSampling
    # TableNameLookup <- "BioSampling"
    # RemoveRedundantEntries <- TRUE
    # FeatureObligations_RuleSet <- Meta_FeatureObligations
    # FeatureObligations_Profile <- "Default"


    # 1) Remove entries that have missing values in strictly obligatory features
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get table's set of (strictly) obligatory features from meta data passed to function
    ObligatoryFeatures <- FeatureObligations_RuleSet %>%
                              rename(Rule = all_of(FeatureObligations_Profile)) %>%      # Renaming feature based on passed argument
                              filter(Table %in% TableNameLookup, Rule == "Obligatory") %>%
                              pull(Feature)

    # Filter out entries that have NAs in any of strictly obligatory features
    CleanedTable <- Table %>%
                        filter(if_all(all_of(ObligatoryFeatures), ~ !is.na(.)))


    # 2) Remove duplicate entries (optional)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (RemoveRedundantEntries == TRUE)
    {
        # Get table's primary key feature (ID column), which is being ignored when determining if table entries are redundant
        IDFeature <- dsCCPhos::Meta_Features %>%
                          filter(TableName_Curated %in% TableNameLookup, IsPrimaryKey == TRUE) %>%
                          pull(FeatureName_Curated)

        CleanedTable <- CleanedTable %>%
                            distinct(across(-all_of(IDFeature)), .keep_all = TRUE)
    }


    # 3) Remove entries that are not consistent with special trans-feature obligation rules
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get current table's set of trans-feature obligation rules (stated as pseudo-code), if there are any defined in meta data passed to function
    TransFeatureRules <- FeatureObligations_RuleSet %>%
                              rename(Rule = all_of(FeatureObligations_Profile)) %>%
                              filter(Table %in% TableNameLookup, !is.na(Rule), Rule != "NA", Rule != "Obligatory") %>%
                              distinct(Rule) %>%
                              pull()

    # If there are any of these rules for the current table, filter out entries that are not consistent with them
    if (!is_empty(TransFeatureRules))
    {
        TransFeatureRules <- TransFeatureRules %>%
                                  CompileTransFeatureRules() %>%      # ... use dsCCPhos::CompileTransFeatureRule to turn the pseudo-code into evaluable strings...
                                  lapply(., rlang::parse_expr) %>%      # ... then transform them into a list of expressions
                                  setNames(paste0("TransFeatureRule_", 1:length(TransFeatureRules)))      # The keys of the list will be the column names of auxiliary features used to determine whether entries are consistent with rules (s. proceedings)

        CleanedTable <- CleanedTable %>%
                            mutate(!!!TransFeatureRules) %>%      # Use list of expressions created earlier to apply trans-feature rules to all entries...
                            filter(if_all(starts_with("TransFeatureRule"), ~ .x == TRUE)) %>%      # ... and filter out any entries that are not consistent with those rules
                            select(-starts_with("TransFeatureRule"))      # Remove auxiliary columns
    }


    # Return cleaned table
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(CleanedTable)
}
