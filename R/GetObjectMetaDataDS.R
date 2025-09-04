
#' GetObjectMetaDataDS
#'
#' Collect meta data about an R object and return it as a list.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S \code{string} - Name of object on server
#'
#' @return A \code{list} containing meta data
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetObjectMetaDataDS <- function(ObjectName.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check input type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.character(ObjectName.S))
  {
      ClientMessage <- "ERROR: 'ObjectName.S' must be specified as a character string"
      stop(ClientMessage, call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Start of function proceedings -
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # --- For Testing Purposes ---
  # ObjectName.S <- "CDS_Diagnosis"

  # Initiate output list
  MetaData <- list()

  if (exists(ObjectName.S, envir = parent.frame()))
  {
      Object <- get(ObjectName.S, envir = parent.frame())

      MetaData$ObjectExists <- TRUE
      MetaData$Class <- GetClass(Object)   # Internal auxiliary function with more informative output than base::class()
      MetaData$Length <- length(Object)
      MetaData$RowCount <- nrow(Object)
      MetaData$Names <- names(Object)

      # If 'Object' is a list or a data.frame, get 1) data types of contained elements and 2) structural overview
      if (MetaData$Class %in% c("list", "data.frame"))
      {
          DataTypesList <- lapply(Object, GetClass)      # Get class/type of every element in 'Object'. This call returns a list...
          MetaData$DataTypes <- sapply(DataTypesList, paste, collapse = "/")      # ... and in case the 'class' of an element of 'Object' returns more than one string, these are concatenated to get one string per element, thus obtaining a character vector for MetaData$DataTypes

          # if (MetaData$Class == "list")
          # {
              MetaData$Structure <- data.frame(Element = MetaData$Names,
                                               Type = MetaData$DataTypes,
                                               row.names = NULL)
          # }
          #
          # if (MetaData$Class == "data.frame")
          # {
          #     MetaData$Structure <- CheckTable(Table = Object)$FeatureCheck %>%
          #                               select(Feature,
          #                                      Type,
          #                                      NonMissingValueCount,
          #                                      NonMissingValueRate) %>%
          #                               rename(Element = "Feature") %>%
          #                               as.data.frame()
          # }
      }

  } else { MetaData$ObjectExists <- FALSE }

  return(MetaData)
}
