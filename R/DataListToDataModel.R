
#' DataListToDataModel
#'
#' Take a list of data frames and return the underlying data model
#'
#' @param DataList
#'
#' @return A list of data frames describing a data model
#' @export
#'
#' @examples
#' @author Bastian Reiter
DataListToDataModel <- function(DataList)
{
    require(dplyr)

    # Get names of tables
    vc_TableNames <- names(DataList)

    # Get a list of
    ls_DataModel <- purrr::map(.x = vc_TableNames,
                               .f = function(x)
                                    {
                                        vc_ColumnNames <- colnames(DataList)

                                        DataFrame <- data.frame()

                                        # Optional for future purposes: Extract column data types
                                        #--------------------------------------------------------
                                        # vc_ColumnTypes <- xml2::xml_attr(x = xml_ColumnNodes,
                                        #                                  attr = ColumnTypeAttribute)

                                        # for (i in 1:length(vc_ColumnNames))
                                        # {
                                        #     Column <- as.character(NULL)
                                        #     if (vc_ColumnTypes[i] == "integer") { Column <- as.integer(NULL) }
                                        #     if (vc_ColumnTypes[i] == "double") { Column <- as.double(NULL) }
                                        #     if (vc_ColumnTypes[i] == "boolean") { Column <- as.logical(NULL) }
                                        #     if (vc_ColumnTypes[i] == "date") { Column <- as.Date(NULL) }
                                        #
                                        #     DataFrame <- cbind(DataFrame, Column)
                                        # }

                                        for (i in 1:length(vc_ColumnNames))
                                        {
                                            Column <- as.character(NULL)
                                            DataFrame <- cbind(DataFrame, Column)
                                        }

                                        colnames(DataFrame) <- vc_ColumnNames

                                        return(DataFrame)

                                     }) %>%
                          purrr::set_names(nm = vc_TableNames)      # Set list element names
}
