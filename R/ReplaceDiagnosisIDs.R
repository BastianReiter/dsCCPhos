
#' ReplaceDiagnosisIDs
#'
#' After classification of redundant and associated diagnosis entries, replace DiagnosisIDs in related table.
#'
#' @param Table Table from partially curated CCP data set
#' @param IDMapping Data frame with mapping of DiagnosisIDs to be replaced
#'
#' @return Data frame with replaced DiagnosisIDs
#' @export
#'
#' @examples
#' @author Bastian Reiter
ReplaceDiagnosisIDs <- function(Table,
                                IDMapping)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)

    Table <- Table %>%
                  left_join(IDMapping, by = join_by(PatientID,
                                                    DiagnosisID == OldDiagnosisID)) %>%
                  mutate(DiagnosisID = ifelse(!is.na(NewDiagnosisID),
                                              NewDiagnosisID,
                                              DiagnosisID)) %>%
                  select(-NewDiagnosisID)

    return(Table)
}
