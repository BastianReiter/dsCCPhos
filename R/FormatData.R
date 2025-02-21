
#' FormatData
#'
#' Format data based on feature type specifications defined in meta data
#'
#' @param TargetVector Vector that formatting is performed on
#' @param Type \code{character} - Character string defining data type other than character
#'
#' @return A vector
#' @export
#'
#' @author Bastian Reiter
FormatData <- function(TargetVector,
                       Type)
{
    require(lubridate)

    vc_Output <- TargetVector

    if (Type == "date") { vc_Output <- as_date(TargetVector) }
    if (Type == "integer") { vc_Output <- as.integer(TargetVector) }
    if (Type == "double") { vc_Output <- as.double(TargetVector) }
    if (Type == "logical") { vc_Output <- as.logical(TargetVector) }

    return(vc_Output)
}
