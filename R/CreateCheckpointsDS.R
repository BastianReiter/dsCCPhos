
#' CreateCheckpointsDS
#'
#' Creates named vector in server R session containing status data on defined processing checkpoints
#'
#' Server-side ASSIGN method
#'
#' @return A named vector
#' @export
#'
#' @author Bastian Reiter
CreateCheckpointsDS <- function()
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load package namespaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Checkpoint vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Checkpoints <- c(Connected = "green",
                 RDSLoaded = NA,
                 CheckRDSTables = NA,
                 ValidateRDSData = NA,
                 Curation = NA,
                 ValidateCDSData = NA,
                 Augmentation = NA,
                 ValidateADSData = NA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return(Checkpoints)

}
