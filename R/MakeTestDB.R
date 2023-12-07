
#' MakeTestDB
#'
#' Takes CCP test data, loads it into a virtual data base and returns the data base connection object.
#'
#' @param TestData A list of data frames representing tables to be loaded into the data base. Must have the structure of the current CCP Raw Data Model.
#'
#' @return A data bases connection object
#' @export
#'
#' @examples
MakeTestDB <- function(TestData)
{
    # Initialize data base connection object, with data base in memory
    TestDB <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

    DBI::dbWriteTable(TestDB, name = "diagnosis", value = TestData$Diagnosis, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "histology", value = TestData$Histology, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "metastasis", value = TestData$Metastasis, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "molecular-marker", value = TestData$MolecularDiagnostics, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "patient", value = TestData$Patient, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "progress", value = TestData$Progress, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "radiation-therapy", value = TestData$RadiationTherapy, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "sample", value = TestData$BioSampling, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "surgery", value = TestData$Surgery, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "system-therapy", value = TestData$SystemicTherapy, overwrite = TRUE)
    DBI::dbWriteTable(TestDB, name = "tnm", value = TestData$Staging, overwrite = TRUE)

    return(TestDB)
}
