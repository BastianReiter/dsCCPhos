
################################################################################
#------------------------------------------------------------------------------#
#   CUSTOM AUXILIARY FUNCTIONS                                                 #
#------------------------------------------------------------------------------#
################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PACKAGE DEPENDENCIES for this Script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(colorspace)
# library(dplyr)
# library(ggpattern)
# library(ggplot2)
# library(showtext)
# library(sysfonts)



################################################################################
#--------- GENERAL AUXILIARY FUNCTIONS ----------------------------------------#
################################################################################


# Custom Infix Operator %notin%
'%notin%' <- function(x, y) { !(x %in% y) }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Progress Bar Information Object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Opens Progress Bar Widget
#   - Returns list
#-------------------------------------------------------------------------------
f_InitProgressBar <- function(inp_Title,
                              inp_ProgressTotalSteps)
{
    ls_ProgressInfo <- list(ProgressBar = tryCatch(tcltk::tkProgressBar(title = inp_Title), error = NULL),
                            ProgressBarTitle = inp_Title,
                            ProgressTotalSteps = inp_ProgressTotalSteps,
                            ProgressStepInfo = tibble(Step = 0,
                                                      Description = "Initiation",
                                                      TimeStamp = Sys.time(),
                                                      Duration = 0))

    try(tcltk::setTkProgressBar(pb = ls_ProgressInfo$ProgressBar,
                                value = 0,
                                title = inp_Title,
                                label = "0% done"),
        silent = TRUE)

    return(ls_ProgressInfo)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  - Updates a Progress Bar Widget
#  - Returns tibble of Progress Step Info
#  - Prints out informations about finished task
#-------------------------------------------------------------------------------
f_UpdateProgressBar <- function(inp_ProgressInfo,
                                inp_StepDescription = NULL)
{
    df_StepInfo <- inp_ProgressInfo$ProgressStepInfo

    FinishedStep <- last(df_StepInfo$Step) + 1

    df_StepInfo <- df_StepInfo %>%
                        add_row(Step = FinishedStep,
                                Description = inp_StepDescription,
                                TimeStamp = Sys.time(),
                                Duration = round(as.numeric(difftime(TimeStamp, last(df_StepInfo$TimeStamp), units = "secs")), 0))

    NewProgressValue <- FinishedStep / inp_ProgressInfo$ProgressTotalSteps

    Info <- sprintf("%i%% done", round(NewProgressValue * 100, 0))

    try(tcltk::setTkProgressBar(pb = inp_ProgressInfo$ProgressBar,
                                value = NewProgressValue,
                                title = sprintf(paste(inp_ProgressInfo$ProgressBarTitle, "(%s)"), Info),
                                label = Info),
        silent = TRUE)

    # Print out information about finished task
    cat("Performed Step ",
            last(df_StepInfo$Step),
            ": '",
            ifelse(FinishedStep > 0,
                   last(df_StepInfo$Description),
                   "Initiation"),
            "' in ",
            as.character(duration(last(df_StepInfo$Duration), units = "seconds")),
            "\n",
        sep = "")

    return(df_StepInfo)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Attrition Tracker (auxiliary table to track sample sizes after filtering operations)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns a tibble
#-------------------------------------------------------------------------------
f_InitAttritionTracker <- function()
{
    df_AttritionTracker <- tibble(SampleSize = numeric(),
                                  InclusionComment = character(),
                                  ExclusionComment = character(),
                                  AttritionCount = numeric())
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Attrition Tracker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processes a tibble
#-------------------------------------------------------------------------------
f_UpdateAttritionTracker <- function(inp_df_AttritionTracker,
                                     inp_NewSampleSize,
                                     inp_InclusionComment = "",
                                     inp_ExclusionComment = "")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    inp_df_AttritionTracker %>%
        add_row(SampleSize = inp_NewSampleSize,
                InclusionComment = inp_InclusionComment,
                ExclusionComment = inp_ExclusionComment)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Close Attrition Tracker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processes a tibble
#-------------------------------------------------------------------------------
f_CloseAttritionTracker <- function(inp_df_AttritionTracker)
{
    inp_df_AttritionTracker %>%
        mutate(AttritionCount = SampleSize - lead(SampleSize)) %>%
        rowid_to_column() %>%
        rename(Step = rowid)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Table with Information about Data Object Structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns a tibble
#-------------------------------------------------------------------------------
f_GetObjectInfo <- function(inp_ObjectNames,
                            inp_Environment)
{
    df_ObjectInfo <- tibble(Object_Name = character(),
                            Object_R_Type = character(),
                            Object_Category = character(),
                            Object_Attributes = character(),
                            Number_of_Rows = numeric())

    for (i in 1:length(inp_ObjectNames))
    {
        CurrentObject_Name <- inp_ObjectNames[i]
        CurrentObject <- get(CurrentObject_Name, envir = inp_Environment)

        CurrentObject_RType <- type_of(CurrentObject)

        CurrentObject_Category <- "Other"
        if (str_starts(CurrentObject_Name, "df")) { CurrentObject_Category <- "Table" }
        if (str_starts(CurrentObject_Name, "plot")) { CurrentObject_Category <- "Plot Object" }
        if (str_starts(CurrentObject_Name, "model")) { CurrentObject_Category <- "Model Object" }
        if (str_starts(CurrentObject_Name, "ls")) { CurrentObject_Category <- "eCDF Object" }
        if (str_starts(CurrentObject_Name, "Validation")) { CurrentObject_Category <- "Meta Data" }

        CurrentObject_Attributes <- paste(colnames(CurrentObject), collapse = ", ")

        CurrentObject_NumberOfRows <- NA
        if (CurrentObject_Category == "Table") { CurrentObject_NumberOfRows <- nrow(CurrentObject) }

        df_ObjectInfo <- df_ObjectInfo %>%
                              add_row(Object_Name = CurrentObject_Name,
                                      Object_R_Type = CurrentObject_RType,
                                      Object_Category = CurrentObject_Category,
                                      Object_Attributes = CurrentObject_Attributes,
                                      Number_of_Rows = CurrentObject_NumberOfRows)
    }

    return(df_ObjectInfo)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Table with Information about Data Frame Attributes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns a tibble
#-------------------------------------------------------------------------------
f_GetAttributesInfo <- function(inp_DataFrameName,
                                inp_Environment,
                                inp_IncludeRandomExampleValues = FALSE,         # Boolean whether to include random values in the form of independent values (so not coming from the same row)
                                inp_IncludeRandomRow = FALSE)                   # Boolean whether to include random values in the form of dependent values (from a randow row)
{
    df_InputDataFrame <- get(inp_DataFrameName, envir = inp_Environment)

    df_AttributesInfo <- tibble(Object = inp_DataFrameName,
                                AttributeName = colnames(df_InputDataFrame),
                                AttributeDataType = sapply(df_InputDataFrame, class))      # Get Data Type of each Attribute

    # Include independent random values
    if (inp_IncludeRandomExampleValues == TRUE)
    {
      vc_RandomValues <- unlist(map(1:ncol(df_InputDataFrame),
                                    function(x) { toString(pull(df_InputDataFrame[sample(1:nrow(df_InputDataFrame), 1), x])) }))      # The combination of toString() and pull() is necessary to preserve date format

      df_AttributesInfo <- df_AttributesInfo %>% add_column(ExampleValue = vc_RandomValues)
    }

    # Include random row (dependent values)
    if (inp_IncludeRandomExampleValues == FALSE && inp_IncludeRandomRow == TRUE)
    {
      vc_RandomRow <- unlist(map(df_InputDataFrame[sample(1:nrow(df_InputDataFrame), 1), ],
                                 toString))      # Extract random row (as a list to preserve data types) and convert every value to character using toString() (preserves date format) and unlist to get character vector

      df_AttributesInfo <- df_AttributesInfo %>% add_column(ExampleValue = vc_RandomRow)
    }

    return(df_AttributesInfo)
}





################################################################################
#--------- REPORTS ------------------------------------------------------------#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get important sample statistics for a feature of a data frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_GetSampleStatistics <- function(inp_df,
                                  inp_MetricFeature,
                                  inp_GroupingFeature = NULL,
                                  inp_na.rm = FALSE)
{
    df_Output <- inp_df %>%
                      summarize(N = n(),
                                Min = min({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                Max = max({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                Q1 = quantile({{ inp_MetricFeature }}, probs = 0.25, na.rm = inp_na.rm),
                                Median = median({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                Q3 = quantile({{ inp_MetricFeature }}, probs = 0.75, na.rm = inp_na.rm),
                                MAD = mad({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                Mean = mean({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                SD = sd({{ inp_MetricFeature }}, na.rm = inp_na.rm))

    if (quo_is_null(enquo(inp_GroupingFeature)) == FALSE)      # If inp_GroupingFeature is not empty...
    {
        # Get group-specific output
        df_Groupwise <- inp_df %>%
                            group_by(., {{ inp_GroupingFeature }}) %>%
                            summarize(N = n(),
                                      Min = min({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                      Max = max({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                      Q1 = quantile({{ inp_MetricFeature }}, probs = 0.25, na.rm = inp_na.rm),
                                      Median = median({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                      Q3 = quantile({{ inp_MetricFeature }}, probs = 0.75, na.rm = inp_na.rm),
                                      MAD = mad({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                      Mean = mean({{ inp_MetricFeature }}, na.rm = inp_na.rm),
                                      SD = sd({{ inp_MetricFeature }}, na.rm = inp_na.rm))

        # Create Extra column for later rbinding
        df_Output <- df_Output %>%
                          mutate(dummy = "All", .before = 1)

        # Harmonize column names for rbinding
        colnames(df_Output) <- colnames(df_Groupwise)

        df_Output <- rbind(df_Output,
                           df_Groupwise)
    }

    return(df_Output)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get key quantiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_GetSampleQuantiles <- function(inp_df,
                                 inp_MetricFeature,
                                 inp_GroupingFeature = NULL,
                                 inp_na.rm = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    # Create vector of quantile probabilities
    vc_Quantiles <- c(c(0.01, 0.025),
                      seq(from = 0.05, to = 0.95, by = 0.05),
                      c(0.975, 0.99))

    # Auxiliary vector for column names
    vc_QuantileNames <- map_chr(vc_Quantiles, ~paste0("P", .x * 100))

    # Using purrr::map, create a list of functions (in this case a set of quantile functions) to be mapped to the metric feature later on
    # Use purrr::partial to pre-define additional arguments for the set of quantile functions
    ls_f_Quantiles <- map(vc_Quantiles,
                          ~partial(quantile, probs = .x, na.rm = inp_na.rm)) %>%
                      set_names(vc_QuantileNames)

    # Apply mapping of quantile functions to the column defined by inp_MetricFeature
    df_Output <- inp_df %>%
                      summarize(across(.cols = {{ inp_MetricFeature }},
                                       .fns = ls_f_Quantiles,
                                       .names = "{.fn}"))

    if (quo_is_null(enquo(inp_GroupingFeature)) == FALSE)      # If inp_GroupingFeature is not empty...
    {
      # Get group-specific output
      df_Groupwise <- inp_df %>%
                          group_by(., {{ inp_GroupingFeature }}) %>%
                          summarize(across(.cols = {{ inp_MetricFeature }},
                                           .fns = ls_f_Quantiles,
                                           .names = "{.fn}"))

      # Create Extra column for later rbinding
      df_Output <- df_Output %>%
                        mutate(dummy = "All", .before = 1)

      # Harmonize column names for rbinding
      colnames(df_Output) <- colnames(df_Groupwise)

      df_Output <- rbind(df_Output,
                         df_Groupwise)
    }

    return(df_Output)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Empirical Cumulative Distribution Function (ECDF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_GetECDF <- function(inp_df,
                      inp_MetricFeature,
                      inp_GroupingFeature = NULL,
                      inp_na.rm = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    df_Input <- inp_df

    # Remove missing values if desired
    if (inp_na.rm == TRUE)
    {
        df_Input <- df_Input %>%
                        filter(., is.na({{ inp_MetricFeature }}) == FALSE)
    }

    # Cumulated output
    df_Output <- df_Input %>%
                      summarize(eCDF = list(ecdf({{ inp_MetricFeature }})))

    # Groupwise output
    if (quo_is_null(enquo(inp_GroupingFeature)) == FALSE)      # If inp_GroupingFeature is not empty...
    {
        df_Groupwise <- df_Input %>%
                            group_by(., {{ inp_GroupingFeature }}) %>%
                            summarize(eCDF = list(ecdf({{ inp_MetricFeature }})))

        # Create Extra column for later rbinding
        df_Output <- df_Output %>%
                          mutate(dummy = "All", .before = 1)

        # Harmonize column names for rbinding
        colnames(df_Output) <- colnames(df_Groupwise)

        df_Output <- rbind(df_Output,
                           df_Groupwise)
    }

    return(df_Output)
}



################################################################################
#--------- PLOTS --------------------------------------------------------------#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Line Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_MakeLinePlot <- function(inp_df,
                           inp_X,
                           inp_Y,
                           inp_GroupingFeature,
                           inp_ggTheme = function(...) theme_CCP(...),
                           inp_LegendPosition = "right",                        # Optional: "top", "bottom", "left", "none"
                           inp_ls_ThemeArguments = list(),                      # Pass custom theme arguments
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    #--- Process plot data -----------------------------------------------------

    df_Plotdata <- inp_df %>%
                      ungroup() %>%
                      select({{ inp_X }},
                             {{ inp_Y }},
                             {{ inp_GroupingFeature }}) %>%
                      rename(X = {{ inp_X }},      # Rename columns for easier processing
                             Y = {{ inp_Y }},
                             GroupingFeature = {{ inp_GroupingFeature }})


    plot <- ggplot(data = df_Plotdata,
                   aes(x = X,
                       y = Y,
                       group = GroupingFeature)) +
            do.call(inp_ggTheme, c(list(...),      # Apply custom theme with optional arguments as concatenated lists
                                   list(inp_Theme_LegendPosition = inp_LegendPosition),      # Because legend position is often used, it gets its own argument
                                   inp_ls_ThemeArguments)) +
            geom_line()



            #
            # labs(x = inp_AxisTitle_x,
            #      y = inp_AxisTitle_y) +
            # #--- Option: If no axis title, delete space for label --------------
            # {
            #   if (inp_AxisTitle_x == "") { theme(axis.title.x = element_blank()) }
            # } + {
            #   if (inp_AxisTitle_y == "") { theme(axis.title.y = element_blank()) }
            # }


}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Column Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_MakeColumnPlot <- function(inp_df,                                            # data frame that should have tidy format
                             inp_X,                                             # Variable that contains x axis categories
                             inp_XSpecs = NULL,                                 # Optional: Character vector that determines selection and order of categories
                             inp_XAdditionalMapping = "none",                   # Optional: "fill" / "alpha" / "pattern"
                             inp_Y,                                             # Variable that contains values / measurements
                             inp_GroupingFeature = NULL,                        # Optional variable that contains group names
                             inp_GroupingSpecs = NULL,                          # Optional: Character vector that determines three specifications of grouping: Selection and order of group levels (from top to bottom in stacked and left to right in dodged) and custom legend labeling (otherwise legend labels are taken from values of grouping variable)
                             inp_GroupingPosition = position_stack(),           # Optional: position_dodge() / position_fill() / ...
                             inp_GroupingMapping = "fill",                      # Optional: "alpha" / "pattern"
                             inp_FacetFeature = NULL,                           # Optional variable for facet grouping
                             inp_FacetSpecs = NULL,                             # Optional: Character vector that determines selection and order of facet levels
                             inp_FacetMapping = "none",                         # Optional: "fill" / "alpha" / "pattern"
                             inp_ls_FacetArguments = list(),
                             inp_AxisType_y = "absolute",                       # Optional: "proportional"
                             inp_CoordFlip = FALSE,
                             inp_LegendPosition = "right",                      # Optional: "top", "bottom", "left", "none"
                             inp_LegendShowFillGuide = TRUE,                    # Optional: Show / hide legend guide of fill mapping
                             inp_AxisTitle_x = "",
                             inp_AxisTitle_y = "",
                             inp_TickLabelWidth_x = 10,
                             inp_Decimals = 0,
                             inp_ggTheme = function(...) theme_CCP(...),
                             inp_ls_ThemeArguments = list(),                    # Pass custom theme arguments
                             inp_FillPalette = palette_CCP_Categorical,         # Custom color palette passed into function environment
                             inp_ColorPrimary = color_Primary,
                             inp_AlphaPalette = NULL,
                             inp_ColumnWidth = 0.95,
                             ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    #--- Process plot data -----------------------------------------------------

    df_Plotdata <- inp_df %>%
                      ungroup() %>%
                      select({{ inp_X }},
                             {{ inp_GroupingFeature }},
                             {{ inp_FacetFeature }},
                             {{ inp_Y }}) %>%
                      rename(X = {{ inp_X }},      # Rename columns for easier processing
                             Y = {{ inp_Y }},
                             GroupingFeature = {{ inp_GroupingFeature }},
                             FacetFeature = {{ inp_FacetFeature }}) %>%
                      { if (quo_is_null(enquo(inp_GroupingFeature)) == FALSE)      # If inp_GroupingFeature is not empty...
                        { mutate(., GroupingFeature = factor(GroupingFeature)) }      # ...make it a factor (discrete) variable for correct processing in ggplot function, especially scale_-layers
                        else {.}
                      } %>%
                      { if (quo_is_null(enquo(inp_FacetFeature)) == FALSE)
                        { mutate(., FacetFeature = factor(FacetFeature)) }
                        else {.}
                      }

    #--- Initialize internal objects -------------------------------------------
    var_Geom <- geom_col()
    var_Modifications <- list()       # Default: No modifications
    var_LegendLabels <- waiver()      # If no grouping specifications are passed, take legend labels from computed transformation object
    var_XLabels <- NULL

    #--- Option: Determine x value selection and order -------------------------
    if (is.null(inp_XSpecs) == FALSE)
    {
        if (is.null(names(inp_XSpecs)) == TRUE) { var_XLabels = inp_XSpecs }      # If X specs are passed without vector naming, take X labels from vector values
        else { var_XLabels <- names(inp_XSpecs) }      # Else take X labels from X specs vector names

        df_Plotdata <- df_Plotdata %>%
                            mutate(X = factor(X, levels = inp_XSpecs, labels = var_XLabels)) %>%
                            filter(is.na(X) == FALSE)
    }


    #--- Option: Determine group level selection and order ---------------------
    if (is.null(inp_GroupingSpecs) == FALSE)
    {
        df_Plotdata <- df_Plotdata %>%
                            mutate(GroupingFeature = factor(GroupingFeature, levels = inp_GroupingSpecs)) %>%
                            filter(is.na(GroupingFeature) == FALSE)

        vc_RepresentedGroupValues <- unique(df_Plotdata$GroupingFeature)
        vc_RepresentedGroupingSpecs <- inp_GroupingSpecs[inp_GroupingSpecs %in% vc_RepresentedGroupValues]

        if (is.null(names(vc_RepresentedGroupingSpecs)) == TRUE) { var_LegendLabels = vc_RepresentedGroupingSpecs }      # If grouping specs are passed without vector naming, take legend labels from vector values
        else { var_LegendLabels <- names(vc_RepresentedGroupingSpecs) }      # Else take legend labels from grouping specs vector names
    }


    #--- Option: Determine facet level selection and order ---------------------
    if (is.null(inp_FacetSpecs) == FALSE)
    {
        df_Plotdata <- df_Plotdata %>%
                            mutate(FacetFeature = factor(FacetFeature, levels = inp_FacetSpecs)) %>%
                            filter(is.na(FacetFeature) == FALSE)
    }


    var_FillMapping <- inp_ColorPrimary      # Default: Fill color taken from inp_ColorPrimary
    var_AlphaMapping <- "static"      # Default: Consistent, if no alpha mapping
    var_AlphaValues <- 0.8      # Default alpha value, if no alpha mapping
    var_PatternMapping <- NULL

    if (inp_XAdditionalMapping == "fill") { var_FillMapping <- df_Plotdata$X }
    if (inp_XAdditionalMapping == "alpha") { var_AlphaMapping <- df_Plotdata$X }
    if (inp_XAdditionalMapping == "pattern") { var_PatternMapping <- df_Plotdata$X }

    if (quo_is_null(enquo(inp_GroupingFeature)) == FALSE)
    {
        if (inp_GroupingMapping == "fill") { var_FillMapping <- df_Plotdata$GroupingFeature }
        if (inp_GroupingMapping == "alpha") { var_AlphaMapping <- df_Plotdata$GroupingFeature }
        if (inp_GroupingMapping == "pattern") { var_PatternMapping <- df_Plotdata$GroupingFeature }
    }

    if (quo_is_null(enquo(inp_FacetFeature)) == FALSE)
    {
      if (inp_FacetMapping == "fill") { var_FillMapping <- df_Plotdata$FacetFeature }
      if (inp_FacetMapping == "alpha") { var_AlphaMapping <- df_Plotdata$FacetFeature }
      if (inp_FacetMapping == "pattern") { var_PatternMapping <- df_Plotdata$FacetFeature }
    }


    #--- Set aesthetics mapping ------------------------------------------------
    var_aesMapping <- aes(fill = var_FillMapping,
                          alpha = var_AlphaMapping)

    #--- Determine geom() object -----------------------------------------------
    var_Geom <- geom_col(mapping = var_aesMapping,
                         position = inp_GroupingPosition,
                         width = inp_ColumnWidth)


    #--- If fill mapping is set to X or Facet Feature --------------------------
    if (inp_XAdditionalMapping == "fill" | inp_FacetMapping == "fill")
    {
        var_Modifications <- c(var_Modifications,
                               list(scale_fill_manual(labels = var_LegendLabels,
                                                      values = inp_FillPalette,      # Set custom fill colors
                                                      guide = NULL)))      # Do not display corresponding legend
    }

    #--- For other cases of fill mapping pass the option to hide fill guide ----
    if (inp_LegendShowFillGuide == FALSE)
    {
        var_Modifications <- c(var_Modifications,
                               list(guides(fill = "none")))
    }

    #--- If alpha mapping is enabled -------------------------------------------
    if (length(var_AlphaMapping) > 1)
    {
        # Set alpha values
        if (is.null(inp_AlphaPalette) == TRUE) {
          var_AlphaValues <- seq(from = 0.04, to = 0.8, length.out = n_distinct(df_Plotdata$GroupingFeature))
        }
        else {
          var_AlphaValues = inp_AlphaPalette
        }

        var_Modifications <- c(var_Modifications,
                               list(scale_alpha_manual(labels = var_LegendLabels,
                                                       values = var_AlphaValues,
                                                       name = NULL)))
    }

    #--- If pattern mapping is enabled -----------------------------------------
    if (is.null(var_PatternMapping) == FALSE)
    {
        var_aesMapping <- aes(fill = var_FillMapping,
                              alpha = var_AlphaMapping,
                              pattern = var_PatternMapping)

        var_Geom <- geom_col_pattern(mapping = var_aesMapping,
                                     position = inp_GroupingPosition,
                                     width = inp_ColumnWidth,
                                     pattern_color = color_DarkGrey,
                                     pattern_fill = color_DarkGrey,
                                     pattern_density = 0.25,
                                     pattern_spacing = 0.04,
                                     pattern_alpha = var_PatternAlpha,
                                     pattern_key_scale_factor = 0.5)

        var_Modifications <- c(var_Modifications,
                               list(scale_pattern_manual(labels = var_LegendLabels,
                                                         values = c("stripe", "crosshatch", "stripe"),
                                                         name = NULL)))
    }

    #--- Option: Format y axis for display of proportional values --------------
    if (inp_AxisType_y == "proportional")
    {
      # List of ggplot layer objects to modify y axis according to proportional values
      var_Modifications <- c(var_Modifications,
                             list(scale_y_continuous(labels = function(x) paste(round(x * 100, 0), "%"),      # Format y axis tick mark labels: Percent
                                                     expand = expansion(mult = c(0, 0.1))),      # No padding between data lower y limit, 10 % padding on upper y limit
                                  theme(axis.line.y = element_line(arrow = NULL))))
    }


    #--- Plot ------------------------------------------------------------------

    plot <- ggplot(data = df_Plotdata,
                   aes(x = X,
                       y = Y)) +
            do.call(inp_ggTheme, c(list(...),      # Apply custom theme with optional arguments as concatenated lists
                                   list(inp_Theme_LegendPosition = inp_LegendPosition),      # Because legend position is often used, it gets its own argument
                                   inp_ls_ThemeArguments)) +
            var_Geom +
            labs(x = inp_AxisTitle_x,
                 y = inp_AxisTitle_y) +
            #--- Option: If no axis title, delete space for label --------------
            {
              if (inp_AxisTitle_x == "") { theme(axis.title.x = element_blank()) }
            } + {
              if (inp_AxisTitle_y == "") { theme(axis.title.y = element_blank()) }
            } +
            # If x axis variable is not numeric: Set width of x axis tick mark labels, after which linebreak should occur
            {
              if (is.numeric(df_Plotdata$X) == FALSE) { scale_x_discrete(labels = label_wrap(inp_TickLabelWidth_x)) }
            } +
            scale_y_continuous(labels = function(value) round(value, inp_Decimals),      # Format y axis tick mark labels: rounded numbers
                               expand = expansion(mult = c(0, 0.1))) +      # No padding between data lower y limit, 10 % padding on upper y limit
            scale_fill_manual(labels = var_LegendLabels,
                              values = inp_FillPalette,      # Set custom fill colors
                              name = NULL) +
            scale_alpha_manual(values = var_AlphaValues,
                               guide = NULL) +      # Do not display alpha related legend, if alpha mapping is not used
            var_Modifications +      # Pass list of optional modifications determined above
            #--- Option: Flip Coordination System ------------------------------
            {
              if (inp_CoordFlip == TRUE) { coord_flip() }
            } +
            #--- Option: Facet -------------------------------------------------
            {
              if (quo_is_null(enquo(inp_FacetFeature)) == FALSE)      # Check if inp_FacetFeature is empty (after defusing inp_FacetFeature with enquo)
              {
                  do.call(facet_wrap, c(list(facets = vars(FacetFeature)),
                                        inp_ls_FacetArguments))      # Use additional facet arguments via ...-Operator
              }
            }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Box- and/or Violinplot to visualize distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_MakeBoxViolinPlot <- function(inp_df,
                                inp_X,                                          # Contains x Values
                                inp_XSelection = NULL,                          # Optional: Character vector that determines selection and order of categories
                                inp_Y,                                          # Contains y Values
                                inp_OutlierQuantile = 1,                        # If set on "1", no outliers are cut
                                inp_OutlierAcrossAll = TRUE,                    # If TRUE, outliers will be calculated across all subgroups
                                inp_LogTransform = FALSE,
                                inp_ShowViolinPlot = TRUE,                      # Option: Control visibility of violin plot
                                inp_AxisLimits_y = c(NA_integer_, NA_integer_),  # Auto y axis limits as default
                                inp_AxisTitle_x = "",
                                inp_AxisTitle_y = "",
                                inp_TickLabelWidth_x = 10,
                                inp_Decimals = 0,                               # Number of decimals of y axis number format
                                inp_ggTheme = function(...) theme_CCP(...),     # Pass custom theme
                                inp_ls_ThemeArguments = list(),                 # Pass custom theme arguments
                                inp_FillPalette = palette_CCP_Categorical,
                                ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    #--- Process plot data -----------------------------------------------------

    df_Plotdata <- inp_df %>%
                        ungroup() %>%
                        select({{ inp_X }},
                               {{ inp_Y }}) %>%
                        rename(X = {{ inp_X }},      # Rename columns for easier further processing
                               Y = {{ inp_Y }}) %>%
                        { #--- Option: Determine X value selection and order
                          if (is.null(inp_XSelection) == FALSE)
                          { mutate(., X = factor(X, levels = inp_XSelection)) %>%
                            filter(., is.na(X) == FALSE) }
                          else {.}
                        } %>%
                        group_by(X) %>%
                        { #--- Option: Calculate outlier thresholds across all subgroups
                          if (inp_OutlierAcrossAll == TRUE)
                          { ungroup(.) }   # By ungrouping here, the following outlier filtering is applied across all measured individuals and not within subgroups
                          else {.}
                        } %>%
                        #--- Filtering out outliers for "clearer" data visualization
                        filter(Y <= quantile(Y, probs = inp_OutlierQuantile, na.rm = TRUE)) %>%
                        { #--- Option: Logarithmic transformation for "clearer" data visualization
                          if (inp_LogTransform == TRUE)
                          { mutate(., Y = log(Y)) }   # Transforming data with natural logarithm to get "clearer" data visualization
                          else {.}
                        } %>%
                        group_by(X)


    #--- Plot ------------------------------------------------------------------

    plot <- ggplot(data = df_Plotdata,
                   aes(x = X,
                       y = Y,
                       fill = X)) +
            do.call(inp_ggTheme, c(list(...),
                                   list(inp_Theme_SizeFactorTickLabels_x = 1.3),      # Increase x axis tick label text size by default
                                   inp_ls_ThemeArguments)) +      # Pass additional optional arguments
            {
              if (inp_ShowViolinPlot == TRUE)
              {
                  geom_violin(width = 0.8,
                              alpha = 0.4,
                              show.legend = FALSE)
              }
            } +
            geom_boxplot(width = 0.4,
                         alpha = 0.7,
                         outlier.shape = NA,
                         show.legend = FALSE) +
            labs(x = inp_AxisTitle_x,
                 y = inp_AxisTitle_y) +
            #--- Option: If no axis title, delete space for label --------------
            {
              if (inp_AxisTitle_x == "") { theme(axis.title.x = element_blank()) }
            } + {
              if (inp_AxisTitle_y == "") { theme(axis.title.y = element_blank()) }
            } +
            scale_x_discrete(labels = label_wrap(inp_TickLabelWidth_x)) +      # Set width of x axis tick mark labels, after which linebreak should occur
            scale_y_continuous(labels = function(value) round(value, inp_Decimals)) +
            ylim(inp_AxisLimits_y[1], inp_AxisLimits_y[2]) +      # Set y axis limits
            scale_fill_manual(values = inp_FillPalette)      # Set custom fill color palette
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for plot export to raster or vector graphic file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f_ExportPlot <- function(inp_Plot,
                         inp_Directory,
                         inp_FileName = "default",
                         inp_FileFormat = "svg",
                         inp_Width,
                         inp_Height,
                         inp_Unit = "cm",
                         inp_DPI = "print",
                         inp_LegendPosition = NULL,
                         inp_ShowAxisTitle_x = TRUE,
                         inp_ShowAxisTitle_y = TRUE,
                         inp_ShowAxisLabels_x = TRUE,
                         inp_ShowAxisLabels_y = TRUE
                         )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  if (inp_FileName == "default") { inp_FileName = deparse(substitute(inp_Plot)) }

  inp_FileName <- paste0(inp_FileName, ".", inp_FileFormat)

  if (inp_ShowAxisTitle_x == FALSE) { inp_Plot <- inp_Plot + theme(axis.title.x = element_blank()) }
  if (inp_ShowAxisTitle_y == FALSE) { inp_Plot <- inp_Plot + theme(axis.title.y = element_blank()) }
  if (inp_ShowAxisLabels_x == FALSE) { inp_Plot <- inp_Plot + theme(axis.text.x = element_blank()) }
  if (inp_ShowAxisLabels_y == FALSE) { inp_Plot <- inp_Plot + theme(axis.text.y = element_blank()) }

  if (is.null(inp_LegendPosition) == FALSE) { inp_Plot <- inp_Plot + theme(legend.position = inp_LegendPosition) }

  ggplot2::ggsave(plot = inp_Plot,
                  filename = inp_FileName,
                  path = inp_Directory,
                  width = inp_Width,
                  height = inp_Height,
                  units = inp_Unit,
                  dpi = inp_DPI)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

