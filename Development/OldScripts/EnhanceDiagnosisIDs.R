
#' EnhanceDiagnosisIDs
#'
#' After classification of associated diagnosis entries, enhance DiagnosisID in related table by ReferenceDiagnosisID.
#'
#' @param Table Table from partially curated CCP data set
#' @param IDMapping Data frame with mapping of DiagnosisIDs to be enhanced
#'
#' @return Data frame with enhanced DiagnosisIDs
#' @export
#'
#' @examples
#' @author Bastian Reiter
EnhanceDiagnosisIDs <- function(Table,
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
                  select(-NewDiagnosisID) %>%
                  relocate(c(ReferenceDiagnosisID, DiagnosisID), .after = PatientID)

    return(Table)
}
