
#' AdjustDiagnosisIDs
#'
#' After classification of duplicated and associated diagnosis entries, adjust DiagnosisID in related tables.
#'
#' @param Table Table from partially curated CCP data set
#'
#' @return Data frame with adjusted DiagnosisID entries
#' @export
#'
#' @examples
#' @author Bastian Reiter
AdjustDiagnosisIDs <- function(Table,
                               IDList_Duplicates,
                               IDList_Associations)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    Table <- Table %>%
                  left_join(IDList_Duplicates, by = join_by(PatientID,
                                                            DiagnosisID == OldDiagnosisID)) %>%
                  mutate(DiagnosisID = ifelse(!is.na(NewDiagnosisID),
                                              NewDiagnosisID,
                                              DiagnosisID)) %>%
                  select(-NewDiagnosisID)


    Table <- Table %>%
                  left_join(IDList_Associations, by = join_by(PatientID,
                                                              DiagnosisID)) %>%

    return(Table)
}
