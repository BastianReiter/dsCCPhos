
#' GetTableCheckDS
#'
#' Checks out a data.frame and returns an informative list object. Makes use of internal function \code{dsCCPhos::CheckTable()}.
#'
#' Server-side AGGREGATE method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame} or \code{tibble} on server
#' @param RequiredFeatureNames.S \code{character vector} - Optional names of required features - Default: \code{names()} applied to Table evaluated from \code{TableName.S}
#' @param GetTemplate.S \code{logical} - If set to \code{TRUE}, the function returns a template incorporating required feature names without actually checking an existing table
#'
#' @return A list containing informative meta data about a \code{data.frame}
#' @export
#'
#' @author Bastian Reiter
GetTableCheckDS <- function(TableName.S,
                            RequiredFeatureNames.S,
                            GetTemplate.S = FALSE)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(TableName.S))
{
    Table <- eval(parse(text = TableName.S), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'TableName.S' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use require() to load package namespaces
require(dplyr)
require(tidyr)


# Initiate output object
TableCheck <- NULL

# Create template only if one of the conditions below is met
if (is.null(Table) | length(Table) == 0 | GetTemplate.S == TRUE)
{
    TableCheck <- CheckTable(Table = NULL,
                             RequiredFeatureNames = RequiredFeatureNames.S)

} else {

    TableCheck <- CheckTable(Table = Table,
                             RequiredFeatureNames = RequiredFeatureNames.S)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(TableCheck)

}
