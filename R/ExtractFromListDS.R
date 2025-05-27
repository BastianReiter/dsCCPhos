
#' ExtractFromListDS
#'
#' Auxiliary function to extract objects from list on server
#'
#' Server-side ASSIGN method
#'
#' @param ListName.S String | Name of a list object on server
#' @param ObjectName.S String | Name of object inside list
#'
#' @return The object to be extracted
#' @export
#'
#' @author Bastian Reiter
ExtractFromListDS <- function(ListName.S,
                              ObjectName.S)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(ListName.S) & is.character(ObjectName.S))
{
    Object <- eval(parse(text = paste0(ListName.S, "$", ObjectName.S)), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'ListName.S' and 'ObjectName.S' must be specified as character strings"
    stop(ClientMessage, call. = FALSE)
}


# Return extracted object
return(Object)

}
