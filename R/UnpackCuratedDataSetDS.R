
#' UnpackCuratedDataSetDS
#'
#' Make table within Curated Data Set directly addressable by assigning it to a data frame object in R server session
#'
#' Server-side ASSIGN method
#'
#' @param Name_CurationOutput String | Name of the list object created by CurateDataDS() | Default: 'CurationOutput'
#' @param TableName String | Name of Curated Data Set table in CurationOutput as defined in \code{\link{Meta_TableNames}}
#'
#' @return A data frame (table from Curated Data Set)
#' @export
#'
#' @examples
#' @author Bastian Reiter
UnpackCuratedDataSetDS <- function(Name_CurationOutput = "CurationOutput",
                                   TableName)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(Name_CurationOutput))
{
    CurationOutput <- eval(parse(text = Name_CurationOutput), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'Name_CurationOutput' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract Curated Data Set (list of data frames) from curation output
CuratedDataSet <- CurationOutput$CuratedDataSet

OutputTable <- eval(parse(text = paste0("CuratedDataSet$", TableName)))

return(OutputTable)

}
