
################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: MATCHING                                                          #
#------------------------------------------------------------------------------#
################################################################################



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get matched data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute matchit object to assess initial covariate (im)balance
match_InitialBalance <- matchit(PatientSubgroup ~ MainCancerDiagnosisAge +
                                                  Sex +
                                                  ComorbidityScoreAtMainCancerDiagnosis +
                                                  MainCancerDiagnosisYear +
                                                  MainCancerTopographyGroup +
                                                  MainCancerIsCarcinomaInSitu,
                                data = df_ADM_PatientsCancer,
                                method = NULL,
                                distance = "glm")

# Look at statistics of covariate (im)balance
summary(match_InitialBalance)

# Compute matchit object using matching method "nearest neighbor"
match_Nearest <- matchit(PatientSubgroup ~ MainCancerDiagnosisAge +
                                           Sex +
                                           ComorbidityScoreAtMainCancerDiagnosis +
                                           MainCancerDiagnosisYear +
                                           MainCancerTopographyGroup +
                                           MainCancerIsCarcinomaInSitu,
                         data = df_ADM_PatientsCancer,
                         method = "nearest",
                         distance = "glm")

# Re-assess covariate (im)balance after matching
summary(match_Nearest, un = FALSE)

# Get matched data set
df_ADM_PatientsCancer_Matched <- match.data(match_Nearest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create and export plots to assess Matching
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PlotPath <- "Data/SiteOutputData"

# Plot of propensity score distribution
svglite(filename = here(PlotPath, "plot_MatchingPropensityScorePlot.svg"))
    plot_MatchingPropensityScorePlot <- plot(match_Nearest, type = "jitter", interactive = FALSE)
dev.off()

# Plot of variable distributions before and after Matching
svglite(filename = here(PlotPath, "plot_MatchingDistributionPlots.svg"))
    plot_MatchingDistributionPlots <- plot(match_Nearest, type = "density", interactive = FALSE)
dev.off()

# Love plot for reporting
svglite(filename = here(PlotPath, "plot_MatchingLovePlot.svg"))
    plot_MatchingLovePlot <- plot(summary(match_Nearest))
dev.off()

