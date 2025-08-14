
#' GetReportingObjectDS
#'
#' Transports a reporting object from server to client. Its name must be on a list of permitted object names to ensure data privacy.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S \code{string} - Name of reporting object on server
#'
#' @return The reporting object
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetReportingObjectDS <- function(ObjectName.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    if (is.character(ObjectName.S))
    {
	      Object <- eval(parse(text = ObjectName.S), envir = parent.frame())
    }
    else
    {
        ClientMessage <- "ERROR: 'ObjectName.S' must be specified as a character string"
        stop(ClientMessage, call. = FALSE)
    }

    PermittedObjectNames <- c("AugmentationMessages",
                              "AugmentationReport",
                              "CurationMessages",
                              "CurationReport")

    if (ObjectName.S %in% PermittedObjectNames)
    {
        return(Object)
    }
    else
    {
        ClientMessage <- "NOT PERMITTED due to data privacy concerns."
        stop(ClientMessage, call. = FALSE)
    }
}
