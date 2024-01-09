
################################################################################
#
#   R SETUP
#
################################################################################


#devtools::install_github("rchaput/knitrdrawio")

#install.packages("pacman")
pacman::p_load( dplyr,
                ggplot2,
                gt,
                gtExtras,
                purrr,
                readr,
                readxl,
                showtext)


source("./Scripts/Preferences.R")

source("./Scripts/Auxiliary.R")




theme_gt_CDSG <- function(inp_gtObject,
                          inp_ShowNAs = FALSE,
                          inp_TableWidth = NULL,
                          inp_TableAlign = "center",
                          ...)
{
  inp_gtObject %>%
    tab_options(table.width = inp_TableWidth,
                table.align = inp_TableAlign,
                table.font.names = c("Karla", default_fonts()),
                table.font.size = "80%",
                heading.align = "center",
                table.border.top.width = NULL,
                column_labels.background.color = "#05499650",
                column_labels.border.top.width = NULL,
                column_labels.border.bottom.width = 3,
                column_labels.border.bottom.color = "#054996",
                row_group.background.color = "#E0E0E0") %>%
    #--- Style column label text ---
    tab_style(locations = cells_column_labels(),
              style = "vertical-align: middle;
                       text-transform: uppercase;
                       font-weight: bold;
                       color: #054996") %>%
    #--- Format column label text (Replace "_" with " ")
    text_replace(locations = cells_column_labels(),
                 pattern = "[_]",
                 replacement = " ") %>%
    #--- Style row group label ---
    tab_style(locations = cells_row_groups(),
              style = "font-weight: bold;
                       color: #054996") %>%
    { if (inp_ShowNAs == FALSE)
      { sub_missing(., missing_text = "") }
      else {.}
    }
}
