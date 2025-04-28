
#' FindRedundantEntries
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Identify and remove any entries that contain less essential information compared to a previous entry.
#'
#' @param Table \code{data.frame} or \code{tibble}
#' @param PrimaryKeyFeature \code{character} - Name of primary key feature
#' @param DiscriminatoryFeatures \code{vector} - Names of features that are used to strictly discriminate between different table entries (used in \code{group_by}-statement)
#' @param EssentialFeatures \code{vector} - Names of features that are considered essential in the informational value of a table entry
#' @param RemoveRedundantEntries \code{logical} - Indicates whether to remove table entries that are considered redundant. If set to \code{FALSE}, the redundant entries are marked as such and preserved. - Default: \code{FALSE}
#'
#' @return \code{data.frame} or \code{tibble} cleaned of redundant entries
#' @export
#'
#' @author Bastian Reiter
FindRedundantEntries <- function(Table,
                                 PrimaryKeyFeature,
                                 DiscriminatoryFeatures,
                                 EssentialFeatures,
                                 RemoveRedundantEntries = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(tidyr)


    ### For function testing purposes
    # Table <- DataSet$BioSampling
    # PrimaryKeyFeature <- Meta_Features %>% filter(TableName_Curated == "BioSampling", IsPrimaryKey == TRUE) %>% pull(FeatureName_Curated)
    # DiscriminatoryFeatures <- Meta_Features %>% filter(TableName_Curated == "BioSampling", IsDiscriminatory == TRUE) %>% pull(FeatureName_Curated)
    # EssentialFeatures <- Meta_Features %>% filter(TableName_Curated == "BioSampling", IsEssential == TRUE) %>% pull(FeatureName_Curated)
    # RemoveRedundantEntries <- TRUE


    # Create auxiliary ID feature to keep track of table entries throughout function
    if (!("AuxID" %in% names(Table)))
    {
        Table <- Table %>% ungroup() %>% mutate(AuxID = row_number())

    } else {    # In the (unlikely) case of preexistence of a feature named 'AuxID' stop and print error message

        stop("ERROR: The passed table contains a feature called 'AuxID' which interferes with function protocol. Please rename this feature and try again.", call. = FALSE)
    }


    # Reorder feature names in 'DiscriminatoryFeatures' according to correct stratification, defined by column order in 'Table'
    DiscriminatoryFeatures <- names(Table)[names(Table) %in% DiscriminatoryFeatures]

    # Reduce table to subset of entries that are potentially redundant to decrease computational workload
    PotentialRedundancies <- Table %>%
                                group_by(across(all_of(DiscriminatoryFeatures))) %>%      # Group by features defined 'DiscriminatoryFeatures' to perform sensible initial stratification
                                    summarize(EntryCount = n()) %>%
                                filter(EntryCount > 1) %>%      # Filter out entries that have no potential redundancies
                                select(-EntryCount) %>%
                                ungroup() %>%
                                left_join(Table) %>%      # Per default the join operation is performed on all common features
                                suppressMessages()

    # Initially all entries in 'PotentialRedundancies' need to be checked
    IDsToCheck <- PotentialRedundancies %>% pull(AuxID)

    # Data frame that holds pairs of IDs of redundant and respective reference entries
    RedundancyPairs <- NULL

    # Vector that holds the persisting (so non-redundant) entry IDs throughout function
    FinalPersistentIDs <- NULL


    while (length(IDsToCheck) > 0)
    {
        # Subsetting of entries to be investigated in current loop instance
        CurrentEntries <- PotentialRedundancies %>%
                              filter(AuxID %in% IDsToCheck) %>%
                              mutate(CountRelevantNAs = rowSums(is.na(across(all_of(EssentialFeatures))))) %>%
                              group_by(across(all_of(DiscriminatoryFeatures))) %>%
                                  arrange(CountRelevantNAs, .by_group = TRUE) %>%      # Put entry with least missing values on first position ('head')
                                  mutate(AuxReferenceID = AuxID[row_number() == 1])

        # Pull out and temporarily save IDs of all current 'reference entries' which are the ones that have the least missing values
        CurrentReferenceIDs <- CurrentEntries %>%
                                    #--- <Still grouped> ---
                                        slice_head() %>%
                                    ungroup() %>%
                                    pull(AuxID)

        # Obtain IDs (based on introduced 'AuxID') of persisting table entries
        # Using combination of fill() and distinct() has the effect that all entries that have less or equal informational value compared to 'reference entry' are considered redundant and therefore removed by 'distinct' command
        CurrentPersistentIDs <- CurrentEntries %>%
                                    #--- <Still grouped> ---
                                        fill(all_of(EssentialFeatures), .direction = "down") %>%
                                        distinct(across(all_of(EssentialFeatures)), .keep_all = TRUE) %>%
                                    ungroup() %>%
                                    pull(AuxID)

        # Obtain pairs of removed AuxIDs and respective AuxReferenceIDs
        CurrentRemovedIDs <- CurrentEntries %>%
                                  ungroup() %>%
                                  filter(!(AuxID %in% CurrentPersistentIDs)) %>%
                                  select(AuxID, AuxReferenceID)

        # New vector of IDs to be checked
        IDsToCheck <- CurrentPersistentIDs %>% base::setdiff(CurrentReferenceIDs)

        # Update 'RedundancyPairs'
        RedundancyPairs <- rbind(RedundancyPairs, CurrentRemovedIDs)

        # Update 'FinalPersistentIDs'
        FinalPersistentIDs <- c(FinalPersistentIDs, CurrentReferenceIDs)
    }


    # Continue with finalization only if 'RedundancyPairs' is not empty, e.g. if any redundancies were found
    if (!is.null(RedundancyPairs))
    {
        # Lookup table for AuxIDs and corresponding table-specific IDs (needed only for redundancy pairs)
        AuxIDLookup <- Table %>%
                            select(AuxID, all_of(PrimaryKeyFeature)) %>%
                            rename(c(ReferenceID = PrimaryKeyFeature))

        # Modify 'RedundancyPairs' before linking it with 'Table'
        RedundancyPairs <- RedundancyPairs %>%
                                mutate(IsRedundant = TRUE) %>%      # This will mark all entries in 'Table' that are considered redundant
                                select(AuxID, IsRedundant, AuxReferenceID)

        # Mark redundant entries in original table preserving info about their referenced entries
        Table <- Table %>%
                      left_join(RedundancyPairs, by = join_by(AuxID)) %>%      # This adds column 'AuxReferenceID' to Table, containing AuxIDs of referenced entries for redundant entries
                      mutate(IsRedundant = coalesce(IsRedundant, FALSE)) %>%      # Replaces all missing values in 'IsRedundant' with 'FALSE'
                      left_join(AuxIDLookup, by = join_by(AuxReferenceID == AuxID)) %>%      # Turn AuxIDs into original primary keys using previously created 'AuxIDLookup'
                      arrange(AuxID) %>%       # Reestablish original table order
                      select(-AuxID,
                             -AuxReferenceID)      # Auxiliary IDs not needed anymore
    } else {

        Table <- Table %>%
                      mutate(IsRedundant = FALSE,
                             ReferenceID = NA)
    }


    # Optionally remove redundant entries from original table. Also remove informative columns about redundancy.
    if (RemoveRedundantEntries == TRUE)
    {
        Table <- Table %>%
                      filter(!(IsRedundant == TRUE)) %>%
                      select(-IsRedundant,
                             -ReferenceID)
    }

    return(Table)

}
