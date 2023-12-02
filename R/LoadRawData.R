
#' LoadRawData
#'
#' @return A "raw data model" (RDM) as a list of tibbles
#' @export
#'
#' @examples
LoadRawData <- function()
{
    require(dplyr)

    rm(list = ls())

    #OpalDBConnection <- ConnectToOpalDB()

    ConnectToOpalDB()
    OpalDBConnection <- TestDB


    #ls_RDM_Tables <- DBI::dbListTables(OpalDBConnection)


    ls_RawDataModel <- list(Patient = DBI::dbGetQuery(conn = OpalDBConnection,
                                                      statement = "SELECT * FROM patient") %>% tibble::as_tibble(),

                            Diagnosis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                        statement = "SELECT * FROM diagnosis") %>% tibble::as_tibble(),

                            Histology = DBI::dbGetQuery(conn = OpalDBConnection,
                                                        statement = "SELECT * FROM histology") %>% tibble::as_tibble(),

                            Metastasis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                         statement = "SELECT * FROM metastasis") %>% tibble::as_tibble(),

                            MolecularDiagnostics = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                   statement = "SELECT * FROM 'molecular-marker'") %>% tibble::as_tibble(),

                            Progress = DBI::dbGetQuery(conn = OpalDBConnection,
                                                       statement = "SELECT * FROM progress") %>% tibble::as_tibble(),

                            RadiationTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                               statement = "SELECT * FROM 'radiation-therapy'") %>% tibble::as_tibble(),

                            Surgery = DBI::dbGetQuery(conn = OpalDBConnection,
                                                      statement = "SELECT * FROM surgery") %>% tibble::as_tibble(),

                            SystemicTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                              statement = "SELECT * FROM 'system-therapy'") %>% tibble::as_tibble(),

                            Staging = DBI::dbGetQuery(conn = OpalDBConnection,
                                                      statement = "SELECT * FROM tnm") %>% tibble::as_tibble(),

                            BioSampling = DBI::dbGetQuery(conn = OpalDBConnection,
                                                          statement = "SELECT * FROM sample") %>% tibble::as_tibble())

    # Disconnect from Opal DB
    #RPostgres::dbDisconnect(conn = OpalDBConnection)

    # Disconnect
    duckdb::dbDisconnect(conn = OpalDBConnection,
                         shutdown = TRUE)

    return(ls_RawDataModel)
}
