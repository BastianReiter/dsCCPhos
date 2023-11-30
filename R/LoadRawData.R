
#' LoadRawData
#'
#' @return A "raw data model" (RDM) as a list of tibbles
#' @export
#'
#' @examples
LoadRawData <- function()
{
    rm(list = ls())

    #OpalDBConnection <- ConnectToOpalDB()

    ConnectToOpalDB()
    OpalDBConnection <- TestDB


    #ls_RDM_Tables <- DBI::dbListTables(OpalDBConnection)


    ls_RawDataModel <- list(df_RDM_Patients = DBI::dbGetQuery(conn = OpalDBConnection,
                                                              statement = "SELECT * FROM patient") %>% tibble::as_tibble(),

                            df_RDM_Diagnosis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                statement = "SELECT * FROM diagnosis") %>% tibble::as_tibble(),

                            df_RDM_Histology = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                statement = "SELECT * FROM histology") %>% tibble::as_tibble(),

                            df_RDM_Metastasis = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                 statement = "SELECT * FROM metastasis") %>% tibble::as_tibble(),

                            df_RDM_MolecularDiagnostics = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                           statement = "SELECT * FROM 'molecular-marker'") %>% tibble::as_tibble(),

                            df_RDM_Progress = DBI::dbGetQuery(conn = OpalDBConnection,
                                                               statement = "SELECT * FROM progress") %>% tibble::as_tibble(),

                            df_RDM_RadiationTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                       statement = "SELECT * FROM 'radiation-therapy'") %>% tibble::as_tibble(),

                            df_RDM_Surgery = DBI::dbGetQuery(conn = OpalDBConnection,
                                                              statement = "SELECT * FROM surgery") %>% tibble::as_tibble(),

                            df_RDM_SystemicTherapy = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                      statement = "SELECT * FROM 'system-therapy'") %>% tibble::as_tibble(),

                            df_RDM_Staging = DBI::dbGetQuery(conn = OpalDBConnection,
                                                              statement = "SELECT * FROM tnm") %>% tibble::as_tibble(),

                            df_RDM_BioSampling = DBI::dbGetQuery(conn = OpalDBConnection,
                                                                  statement = "SELECT * FROM sample") %>% tibble::as_tibble())

    # Disconnect from Opal DB
    RPostgres::dbDisconnect(conn = OpalDBConnection)

    return(ls_RawDataModel)
}
