
#' GetObjectInfoDS
#'
#' Similar to dsBase::testObjExistsDS(). Tests whether a certain object exists physically on server and provides meta info about it.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName String | Name of object on server
#'
#' @return A list containing information about existence and class of object
#' @export
#'
#' @examples
#' @author Bastian Reiter
GetObjectInfoDS <- function(ObjectName)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check input type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!is.character(ObjectName))
    {
        ClientMessage <- "ERROR: 'ObjectName' must be specified as a character string"
        stop(ClientMessage, call. = FALSE)
    }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ObjectExists <- FALSE
    ObjectClass <- NULL

    if (exists(ObjectName, envir = parent.frame()))
    {
        ObjectExists <- TRUE
        ObjectClass <- class(get(ObjectName, envir = parent.frame()))
    }

    return(list(ObjectExists = ObjectExists,
                ObjectClass = ObjectClass))
}
