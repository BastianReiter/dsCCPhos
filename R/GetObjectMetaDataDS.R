
#' GetObjectMetaDataDS
#'
#' Collect meta data about an R object and return it as a list.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S String | Name of object on server
#'
#' @return A list containing meta data
#' @export
#'
#' @author Bastian Reiter
GetObjectMetaDataDS <- function(ObjectName.S)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check input type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!is.character(ObjectName.S))
    {
        ClientMessage <- "ERROR: 'ObjectName.S' must be specified as a character string"
        stop(ClientMessage, call. = FALSE)
    }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ### For testing purposes
    # ObjectName.S <- "CurationReport"

    # Initiate output list
    MetaData <- list()

    if (exists(ObjectName.S, envir = parent.frame()))
    {
        Object <- get(ObjectName.S, envir = parent.frame())

        MetaData$ObjectExists <- TRUE
        MetaData$Class <- class(Object)[1]      # Some objects return more than one string as class info (e.g. ResourceClient objects). Take only first string for these cases.
        MetaData$Length <- length(Object)
        MetaData$RowCount <- nrow(Object)
        MetaData$Names <- names(Object)

        # If 'Object' is a list or a data.frame, get 1) data types of contained elements and 2) structural overview
        if (MetaData$Class %in% c("list", "data.frame"))
        {
            DataTypesList <- lapply(Object, class)      # Get class/type of every element in 'Object'. This call returns a list...
            MetaData$DataTypes <- sapply(DataTypesList, paste, collapse = "/")      # ... and in case the 'class' of an element of 'Object' returns more than one string, these are concatenated to get one string per element, thus obtaining a character vector for MetaData$DataTypes

            MetaData$Structure <- data.frame(Element = MetaData$Names,
                                             Type = MetaData$DataTypes,
                                             row.names = NULL)
        }

    } else { MetaData$ObjectExists <- FALSE }

    return(MetaData)
}
