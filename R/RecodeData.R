
#' RecodeData
#'
#' Uses a dictionary in the form of a named vector to perform recoding on a target vector.
#'
#' @param TargetVector Vector that recoding is performed on
#' @param Dictionary A named character vector. Vector names are look-up values, vector values are substitute values.
#'
#' @return A vector
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RecodeData <- function(TargetVector,
                       Dictionary)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(stringr)

    # Look-up Values come from names(Dictionary)
    vc_LookupValues <- paste0("^",      # Embrace Lookup Values with "^" and "$" to isolate them, so that for example "R" is not recognized (and replaced) in "CR", but only when it stands isolated
                              str_escape(names(Dictionary)),      # Introduce escaping of special characters, for example "(" and "/"
                              "$")

    # Add "Protection Prefix" to mark freshly changed Values, so that they won't be looked up themselves
    vc_NewValues <- paste0("NEW_", Dictionary)
    names(vc_NewValues) <- vc_LookupValues

    vc_Output <- str_replace_all(TargetVector, vc_NewValues)
    vc_Output <- str_remove_all(vc_Output, "NEW_")      # Remove the introduced "Protection Prefix"

    return(vc_Output)
}
