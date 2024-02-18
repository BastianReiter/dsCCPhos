
#' UnpackAugmentedDataSetDS
#'
#' Make table within Augmented Data Set directly addressable by assigning it to a data frame object in R server session
#'
#' Server-side ASSIGN method
#'
#' @param Name_AugmentationOutput String | Name of the list object created by AugmentDataDS() | Default: 'AugmentationOutput'
#' @param TableName String | Name of Augmented Data Set table in AugmentationOutput
#'
#' @return A data frame (table from Augmented Data Set)
#' @export
#'
#' @examples
#' @author Bastian Reiter
UnpackAugmentedDataSetDS <- function(Name_AugmentationOutput = "AugmentationOutput",
                                     TableName)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Evaluate and parse input before proceeding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (is.character(Name_AugmentationOutput))
{
    AugmentationOutput <- eval(parse(text = Name_AugmentationOutput), envir = parent.frame())
}
else
{
    ClientMessage <- "ERROR: 'Name_AugmentationOutput' must be specified as a character string"
    stop(ClientMessage, call. = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Start of function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract Table from Augmentation Output
OutputTable <- eval(parse(text = paste0("AugmentationOutput$", TableName)))

return(OutputTable)

}
