
#' GetCohortDescriptionDS
#'
#' What it does
#'
#' Server-side AGGREGATE method
#'
#' @param Name_AugmentationOutput String | Name of object on server previously assigned by dsCCPhos::AugmentData() | Default: 'AugmentationOutput'
#'
#' @return
#' @export
#'
#' @examples
#' @author Bastian Reiter
GetCohortDescriptionDS <- function(Name_AugmentationOutput = "AugmentationOutput")
{
    if (is.character(Name_AugmentationOutput))
    {
	      AugmentationOutput <- eval(parse(text = Name_AugmentationOutput), envir = parent.frame())
    }
    else
    {
        ClientMessage <- "ERROR: Name_AugmentationOutput must be specified as a character string"
        stop(ClientMessage, call. = FALSE)
    }


}
