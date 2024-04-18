
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

    # Initiate output list
    MetaData <- list()

    if (exists(ObjectName.S, envir = parent.frame()))
    {
        Object <- get(ObjectName.S, envir = parent.frame())

        MetaData$ObjectExists <- TRUE
        MetaData$Class <- class(Object)
        MetaData$Length <- length(Object)
        MetaData$RowCount <- nrow(Object)
        MetaData$Names <- names(Object)
        MetaData$DataTypes <- switch(class(Object) %in% c("list", "data.frame"),      # Only apply if 'Object' is a list or a data.frame
                                     sapply(Object, class))

        MetaData$ContentOverview <- switch(class(Object) %in% c("list", "data.frame"),
                                           data.frame(Element = MetaData$Names,
                                                      Type = MetaData$DataTypes,
                                                      row.names = NULL))
    }
    else { MetaData$ObjectExists <- FALSE }

    return(MetaData)
}
