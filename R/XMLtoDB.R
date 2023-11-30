
#' XMLtoDB
#'
#' Make (virtual) data base from XML Schema
#'
#' @param XMLSchema
#' @param TableNodeElement
#' @param TableNameAttribute
#' @param ColumnNodeElement
#' @param ColumnNameAttribute
#' @param ColumnTypeAttribute
#' @param DBPath
#'
#' @return A data base connection object
#' @export
#'
#' @examples
XMLtoDB <- function(XMLSchemaFilePath = "./Development/MetaData/SchemaOpalDB.xml",
                    TableNodeElement = "container",
                    TableNameAttribute = "opal-table",
                    ColumnNodeElement = "attribute",
                    ColumnNameAttribute = "csv-column",
                    ColumnTypeAttribute = "opal-value-type",
                    DBPath = ":memory:")
{
    # Read xml file
    XMLSchema = xml2::read_xml(XMLSchemaFilePath)

    # Find "Table" nodes in Schema
    xml_TableNodes <- xml2::xml_find_all(x = XMLSchema,
                                         xpath = paste0(".//", TableNodeElement))

    # Get names of tables
    vc_TableNames <- xml2::xml_attr(x = xml_TableNodes,
                                    attr = TableNameAttribute)

    # Get a list of data frames being named after the containers in the XML Schema
    ls_DataModel <- purrr::map(.x = vc_TableNames,
                               .f = function(x)
                                    {
                                        xml_ColumnNodes <- xml2::xml_find_all(x = XMLSchema,
                                                                              xpath = paste0(".//", TableNodeElement, "[@", TableNameAttribute, "='", x, "']/", ColumnNodeElement))

                                        vc_ColumnNames <- xml2::xml_attr(x = xml_ColumnNodes,
                                                                         attr = ColumnNameAttribute)

                                        vc_ColumnTypes <- xml2::xml_attr(x = xml_ColumnNodes,
                                                                         attr = ColumnTypeAttribute)

                                        DataFrame <- data.frame()
                                        for (i in 1:length(vc_ColumnNames))
                                        {
                                            Column <- as.character(NULL)
                                            if (vc_ColumnTypes[i] == "integer") { Column <- as.integer(NULL) }
                                            if (vc_ColumnTypes[i] == "double") { Column <- as.double(NULL) }
                                            if (vc_ColumnTypes[i] == "boolean") { Column <- as.logical(NULL) }
                                            if (vc_ColumnTypes[i] == "date") { Column <- as.Date(NULL) }

                                            DataFrame <- cbind(DataFrame, Column)
                                        }

                                        colnames(DataFrame) <- vc_ColumnNames

                                        return(DataFrame)

                                     }) %>%
                          purrr::set_names(nm = vc_TableNames)      # Set list element names


    # Initialize data base connection object, with data base in memory
    DBConnection <- DBI::dbConnect(duckdb::duckdb(), dbdir = DBPath)

    # Load empty data frames into data base
    for (i in 1:length(ls_DataModel))
    {
        DBI::dbWriteTable(conn = DBConnection,
                          name = names(ls_DataModel)[i],
                          value = ls_DataModel[[i]])
    }

    # Return the data base connection object
    return(DBConnection)
}
