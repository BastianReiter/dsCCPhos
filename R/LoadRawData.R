
#' LoadRawData
#'
#' @return A list of tibbles containing raw data as described by the Raw Data Model
#' @export
#'
#' @examples
LoadRawData <- function(OpalDBConnection)
{
    require(dplyr)

    #OpalDBConnection <- ConnectToOpalDB()

    # Load data from data base into a list of data frames, sorted alphabetically
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ls_RawData <- list(BioSampling = DBI::dbGetQuery(conn = OpalDBConnection,
                                                     statement = "SELECT * FROM sample") %>% tibble::as_tibble(),

                       Diagnosis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                   statement = "SELECT * FROM diagnosis") %>% tibble::as_tibble(),

                       Histology = DBI::dbGetQuery(conn = OpalDBConnection,
                                                   statement = "SELECT * FROM histology") %>% tibble::as_tibble(),

                       Metastasis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                    statement = "SELECT * FROM metastasis") %>% tibble::as_tibble(),

                       MolecularDiagnostics = DBI::dbGetQuery(conn = OpalDBConnection,
                                                              statement = "SELECT * FROM 'molecular-marker'") %>% tibble::as_tibble(),

                       Patient = DBI::dbGetQuery(conn = OpalDBConnection,
                                                 statement = "SELECT * FROM patient") %>% tibble::as_tibble(),

                       Progress = DBI::dbGetQuery(conn = OpalDBConnection,
                                                  statement = "SELECT * FROM progress") %>% tibble::as_tibble(),

                       RadiationTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                          statement = "SELECT * FROM 'radiation-therapy'") %>% tibble::as_tibble(),

                       Staging = DBI::dbGetQuery(conn = OpalDBConnection,
                                                 statement = "SELECT * FROM tnm") %>% tibble::as_tibble(),

                       Surgery = DBI::dbGetQuery(conn = OpalDBConnection,
                                                 statement = "SELECT * FROM surgery") %>% tibble::as_tibble(),

                       SystemicTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                         statement = "SELECT * FROM 'system-therapy'") %>% tibble::as_tibble())


    # Disconnect from Opal DB
    #RPostgres::dbDisconnect(conn = OpalDBConnection)

    # Disconnect from Test DB
    duckdb::dbDisconnect(conn = OpalDBConnection,
                         shutdown = TRUE)

    return(ls_RawData)
}
