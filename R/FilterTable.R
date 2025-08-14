
#' FilterTableDS
#'
#' Filter a table on server, making use of \code{dplyr} and \code{stringr}.
#'
#' Server-side ASSIGN method
#'
#' @param TableName.S \code{string} - Name of Table A on server
#' @param FilterExpressions
#'
#' @return A \code{data.frame} resulting from filter operation
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FilterTableDS <- function(TableName,
                          FilterExpressions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)

  # --- For Testing Purposes ---
  # TableName.S <- "ADS_Patient"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Function proceedings -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # TableA <- eval(parse(text = TableNameA.S), envir = parent.frame())
  # TableB <- eval(parse(text = TableNameB.S), envir = parent.frame())
  #
  # if (JoinType.S %in% c("left_join", "right_join", "full_join", "inner_join"))
  # {
  #     Output <- eval(parse(text = paste0(JoinType.S, "(TableA, TableB, by = join_by(", ByStatement.S, "))")))
  #
  # } else {
  #     stop(paste0("ERROR: 'JoinType.S' does not provide a valid join operation!"))
  # }
  #
  # return(as.data.frame(Output))
}
