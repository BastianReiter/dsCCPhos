
#' DataModelToCSV
#'
#' @param DataModel A list of data frames describing a data model
#' @param OutputPath A string describing the file path of the created .csv-file
#'
#' @return No return, creates a .csv-file.
#' @export
#'
#' @author Bastian Reiter
DataModelToCSV <- function(DataModel,
                           OutputPath = "./DataModel.csv")
{
    require(dplyr)

    df_Output <- tibble::tibble()

    for (i in 1:length(DataModel))
    {
        # Get info about current data frame
        df_Current <- dsCCPhos::GetDFInfo(DataModel[[i]])
        # Overwrite column "DFName" with actual data frame names
        df_Current <- df_Current %>%
                          mutate(DFName = rep(names(DataModel)[i], nrow(df_Current)))
        # Attach current
        df_Output <- rbind(df_Output, df_Current)
    }

    readr::write_csv(df_Output,
                     file = OutputPath)

    cat("CSV file generated.")
}
