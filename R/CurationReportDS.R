
#' CurationReportDS
#'
#' What it does
#'
#' Server-side AGGREGATE method
#'
#' @param Name_CurationOutput String | Name of object on server previously assigned by dsCCPhos::CurateData() | Default: 'CurationOutput'
#'
#' @return A list of data frames containing information about occurring data values in different Curation stages ("Raw" / "Transformed" / "Final")
#' @export
#'
#' @examples
CurationReportDS <- function(Name_CurationOutput = "CurationOutput")
{
    if (is.character(Name_CurationOutput))
    {
	      CurationOutput <- eval(parse(text = Name_CurationOutput), envir = parent.frame())
    }
    else
    {
        ClientMessage <- "ERROR: Name_CurationOutput must be specified as a character string"
        stop(ClientMessage, call. = FALSE)
    }

    return(CurationOutput$CurationReport)
}
