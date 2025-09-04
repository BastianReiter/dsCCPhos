
#' MutateTableDS
#'
#' Make use of \code{dplyr::mutate()} to create new features in a given table (\code{data.frame}).
#'
#' Server-side ASSIGN method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame}
#' @param MutateExpression.S \code{string} - \code{dplyr::mutate} expression as string
#' @param GroupBy.S \code{string} - Optional \code{dplyr::group_by} expression as string
#'
#' @return A \code{data.frame} resulting from mutating operation
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MutateTableDS <- function(TableName.S,
                          MutateExpression.S,
                          GroupBy.S = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)

  # --- For Testing Purposes ---
  # TableName.S <- "CDS$Patient"
  # MutateExpression.S <- "LastVitalStatus == 'Alive'"
  # GroupBy.S <- 3

  # --- Argument Assertions ---
  assert_that(is.string(TableName.S),
              is.string(MutateExpression.S),
              (is.null(GroupBy.S) || is.string(GroupBy.S)))

#-------------------------------------------------------------------------------

  # Get 'Table' object from workspace
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Optionally employ grouping
  if (!is.null(GroupBy.S))
  {
      Table <- eval(parse(text = paste0("dplyr::group_by(Table, ", GroupBy.S, ")")))
  }

  # Decode 'MutateExpression.S'
  MutateExpression.S <- .decode_tidy_eval(MutateExpression.S, .get_encode_dictionary())

  # Make additional namespaces available for evaluated mutate expressions
  require(lubridate)
  require(stringr)

  # Perform mutate operation
  Table <- eval(parse(text = paste0("dplyr::mutate(Table, ", MutateExpression.S, ")")))

  # Ungroup tibble
  Table <- eval(parse(text = "dplyr::ungroup(Table)"))

  # Return as data.frame
  return(as.data.frame(Table))
}
