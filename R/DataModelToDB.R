
#' DataModelToDB
#'
#' Make (virtual) data base from a data model
#'
#' @param DataModel A list of data frames describing a data model
#' @param DBPath
#'
#' @return A data base connection object
#' @export
#'
#' @examples
#' @author Bastian Reiter
DataModelToDB <- function(DataModel,
                          DBPath = ":memory:")
{
    # Initialize data base connection object, with data base in memory
    DBConnection <- DBI::dbConnect(duckdb::duckdb(), dbdir = DBPath)

    # Load empty data frames from list object into data base
    for (i in 1:length(DataModel))
    {
        DBI::dbWriteTable(conn = DBConnection,
                          name = names(DataModel)[i],
                          value = DataModel[[i]])
    }

    # Return the data base connection object
    return(DBConnection)
}
