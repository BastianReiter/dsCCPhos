
#' GetObjectStatusDS
#'
#' Similar to dsBase::testObjExistsDS(). Tests whether a certain object exists physically on server and provides meta info about it.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S String | Name of object on server
#'
#' @return A list containing information about existence and class of object
#' @export
#'
#' @author Bastian Reiter
GetObjectStatusDS <- function(ObjectName.S)
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

    ObjectExists <- FALSE
    ObjectClass <- NULL

    if (exists(ObjectName.S, envir = parent.frame()))
    {
        ObjectExists <- TRUE
        ObjectClass <- class(get(ObjectName.S, envir = parent.frame()))
    }

    return(list(ObjectExists = ObjectExists,
                ObjectClass = ObjectClass))
}
