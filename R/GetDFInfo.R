
#' GetDFInfo
#'
#' Returns information about a data frame's features and data types
#'
#' @param DataFrame A data frame or tibble
#' @param IncludeRandomExampleValues
#' @param IncludeRandomRow
#'
#' @return A tibble containing information about a data frame's features
#' @export
#'
#' @examples
GetDFInfo <- function(DataFrame,
                      IncludeRandomExampleValues = FALSE,         # Boolean whether to include random values in the form of independent values (so not coming from the same row)
                      IncludeRandomRow = FALSE)                   # Boolean whether to include random values in the form of dependent values (from a randow row)
{
    require(dplyr)

    # Get name of data frame
    DFName <- deparse(substitute(DataFrame))

    df_Info <- tibble::tibble(DFName = DFName,       # Data frame name is being repeated for every row
                              FeaturePosition = 1:ncol(DataFrame),
                              FeatureLabel = colnames(DataFrame),
                              FeatureDataType = sapply(DataFrame, class))      # Get data type of each feature

    # Include independent random values
    if (IncludeRandomExampleValues == TRUE)
    {
      vc_RandomValues <- unlist(purrr::map(1:ncol(DataFrame),
                                           function(x) { toString(dplyr::pull(tibble::as_tibble(DataFrame[sample(1:nrow(DataFrame), 1), x]))) }))      # The combination of toString() and pull() is necessary to preserve date format

      df_Info <- df_Info %>% tibble::add_column(ExampleValue = vc_RandomValues)
    }

    # Include random row (dependent values)
    if (IncludeRandomExampleValues == FALSE && IncludeRandomRow == TRUE)
    {
      vc_RandomRow <- unlist(purrr::map(DataFrame[sample(1:nrow(DataFrame), 1), ],
                                        toString))      # Extract random row (as a list to preserve data types) and convert every value to character using toString() (preserves date format) and unlist to get character vector

      df_Info <- df_Info %>% tibble::add_column(ExampleValue = vc_RandomRow)
    }

    return(df_Info)
}
