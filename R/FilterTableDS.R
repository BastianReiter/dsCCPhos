
#' FilterTableDS
#'
#' Perform filtering on a server-side table, making use of \code{dplyr::filter()} operations.
#'
#' Server-side ASSIGN method
#'
#' @param TableName.S \code{string} - Name of Table A on server
#' @param FilterStatement.S \code{string} - The insides of a \code{dplyr::filter()}-Statement defining which rows to select/exclude
#'
#' @return A \code{data.frame} resulting from filter operation
#' @export
#'
#' @author Bastian Reiter
FilterTableDS <- function(TableName.S,
                          FilterStatement.S)
{

### For testing purposes
# TableName.S <- "ADS_Patient"
# FilterStatement.S <- "CountDiagnoses == 1"


require(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Check input types -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    InputTypes <- c(TableName.S = "character",
                    FilterStatement.S = "character")

    for (i in 1:length(InputTypes))
    {
        if (eval(parse(text = paste0("!is.", InputTypes[[i]], "(", names(InputTypes)[i], ")"))))
        {
            stop(paste0("ERROR: '", names(InputTypes)[i], "' must be of type ", InputTypes[[i]], "!"))
        }
    }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Table <- eval(parse(text = TableName.S), envir = parent.frame())

    Output <- Table %>%
                  filter(eval(parse(text = FilterStatement.S)))

    return(as.data.frame(Output))
}
