
#' XMLToDataModel
#'
#' Make List of data frames describing a data model
#'
#' @param XMLSchemaFilePath
#' @param TableNodeElement
#' @param TableNameAttribute
#' @param FeatureNodeElement
#' @param FeatureNameAttribute
#' @param FeatureTypeAttribute
#'
#' @return A list of data frames
#' @export
#'
#' @examples
XMLToDataModel <- function(XMLSchemaFilePath,
                           TableNodeElement = "container",
                           TableNameAttribute = "opal-table",
                           FeatureNodeElement = "attribute",
                           FeatureNameAttribute = "csv-column",
                           FeatureTypeAttribute = "opal-value-type")
{
    require(dplyr)

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
                                        xml_FeatureNodes <- xml2::xml_find_all(x = XMLSchema,
                                                                               xpath = paste0(".//", TableNodeElement, "[@", TableNameAttribute, "='", x, "']/", FeatureNodeElement))

                                        vc_FeatureNames <- xml2::xml_attr(x = xml_FeatureNodes,
                                                                          attr = FeatureNameAttribute)

                                        vc_FeatureTypes <- xml2::xml_attr(x = xml_FeatureNodes,
                                                                          attr = FeatureTypeAttribute)

                                        DataFrame <- data.frame()

                                        for (i in 1:length(vc_FeatureNames))
                                        {
                                            Feature <- as.character(NULL)
                                            if (vc_FeatureTypes[i] == "integer") { Feature <- as.integer(NULL) }
                                            if (vc_FeatureTypes[i] == "double") { Feature <- as.double(NULL) }
                                            if (vc_FeatureTypes[i] == "boolean") { Feature <- as.logical(NULL) }
                                            if (vc_FeatureTypes[i] == "date") { Feature <- as.Date(NULL) }

                                            DataFrame <- cbind(DataFrame, Feature)
                                        }

                                        colnames(DataFrame) <- vc_FeatureNames

                                        return(DataFrame)

                                     }) %>%
                          purrr::set_names(nm = vc_TableNames)      # Set list element names

    # Return the data model as data frames in a list
    return(ls_DataModel)
}
