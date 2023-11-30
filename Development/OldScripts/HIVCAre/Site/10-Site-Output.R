

################################################################################
#------------------------------------------------------------------------------#
#   OUTPUT (SITE)                                                              #
#------------------------------------------------------------------------------#
################################################################################


################################################################################
#
#          Across all primary subgroups
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Postal Code count
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_PostalCodeCounts <- df_ADM_Patients %>%
                                  group_by(PatientSubgroup, PrimaryPostalCode, FirstRelevantAdmissionYear) %>%
                                      summarize(Count = n()) %>%
                                      mutate(Percentage = Count / sum(Count)) %>%
                                  ungroup()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_SampleSize <- df_ADM_Patients %>%
                            group_by(PatientSubgroup, FirstRelevantAdmissionYear) %>%
                            summarize(N = n())

# Column plot over time
plot_Output_SampleSize_OverTime <- df_Output_SampleSize %>%
                                        f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                         inp_Y = N,
                                                         inp_FacetFeature = PatientSubgroup,
                                                         inp_ls_FacetArguments = list(dir = "v",
                                                                                      scales = "free_y"),
                                                         inp_FacetMapping = "fill",
                                                         inp_FillPalette = vc_FillPalette_Subgroup)      # Individual y-scales for different subgroups



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_Sex <- df_ADM_Patients %>%
                      group_by(PatientSubgroup, FirstRelevantAdmissionYear, Sex) %>%
                      summarize(N = n())

# Column plot over time
plot_Output_Sex_OverTime <- df_Output_Sex %>%
                                f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                 inp_Y = N,
                                                 inp_GroupingFeature = Sex,
                                                 inp_GroupingSpecs = c("Diverse" = "U", "Male" = "M", "Female" = "F"),
                                                 inp_GroupingPosition = position_fill(),
                                                 inp_GroupingMapping = "alpha",
                                                 inp_AlphaPalette = vc_AlphaPalette_3,
                                                 inp_AxisType_y = "proportional",
                                                 inp_FacetFeature = PatientSubgroup,
                                                 inp_ls_FacetArguments = list(dir = "v"),
                                                 inp_FacetMapping = "fill",
                                                 inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at first main admission) distribution over time
df_Output_Age <- df_ADM_Patients %>%
                      mutate(AgeGroup = case_when(between(FirstRelevantAdmissionAge, 18, 39) == TRUE ~ "18 - 39 years old",
                                                  between(FirstRelevantAdmissionAge, 40, 59) == TRUE ~ "40 - 59 years old",
                                                  between(FirstRelevantAdmissionAge, 60, 79) == TRUE ~ "60 - 79 years old",
                                                  FirstRelevantAdmissionAge >= 80 ~ "> 80 years old")) %>%
                      group_by(PatientSubgroup, FirstRelevantAdmissionYear, AgeGroup) %>%
                      summarize(N = n())

# Column plot over time
plot_Output_Age_OverTime <- df_Output_Age %>%
                                f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                 inp_Y = N,
                                                 inp_GroupingFeature = AgeGroup,
                                                 inp_GroupingPosition = position_fill(),
                                                 inp_AxisType_y = "proportional",
                                                 inp_GroupingSpecs = c("> 80 years old",
                                                                       "60 - 79 years old",
                                                                       "40 - 59 years old",
                                                                       "18 - 39 years old"),
                                                 inp_GroupingMapping = "alpha",
                                                 inp_AlphaPalette = vc_AlphaPalette_4,
                                                 inp_FacetFeature = PatientSubgroup,
                                                 inp_ls_FacetArguments = list(dir = "v",
                                                                              scales = "free_y"),
                                                 inp_FacetMapping = "fill",
                                                 inp_FillPalette = vc_FillPalette_Subgroup)

# Age distribution summary statistics
df_Output_Age_Summary <- df_ADM_Patients %>%
                              f_GetSampleStatistics(inp_MetricFeature = FirstRelevantAdmissionAge,
                                                    inp_GroupingFeature = PatientSubgroup,
                                                    inp_na.rm = TRUE)

# Age distribution quantiles
df_Output_Age_Quantiles <- df_ADM_Patients %>%
                                f_GetSampleQuantiles(inp_MetricFeature = FirstRelevantAdmissionAge,
                                                     inp_GroupingFeature = PatientSubgroup,
                                                     inp_na.rm = TRUE)

# Get empirical cumulative distribution functions
ls_Output_Age_ECDF <- df_ADM_Patients %>%
                          f_GetECDF(inp_MetricFeature = FirstRelevantAdmissionAge,
                                    inp_GroupingFeature = PatientSubgroup)

#plot(ecdf_Age$eCDF[[1]])

# Box and Violin plots
plot_Output_Age <- df_ADM_Patients %>%
                        f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                            inp_Y = FirstRelevantAdmissionAge,
                                            inp_AxisTitle_y = "Age at Diagnosis",
                                            inp_AxisLimits_y = c(0, NA_integer_),
                                            inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case count per patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Case count over time
df_Output_CaseCount <- df_ADM_Patients %>%
                            mutate(CaseCountGroup = case_when(CaseCount == 1 ~ "1 Case",
                                                              between(CaseCount, 2, 4) == TRUE ~ "2 - 4 Cases",
                                                              between(CaseCount, 5, 10) == TRUE ~ "5 - 10 Cases",
                                                              CaseCount > 10 ~ "More than 10 Cases")) %>%
                            group_by(PatientSubgroup, FirstRelevantAdmissionYear, CaseCountGroup) %>%
                            summarize(N = n())

# Column plot over time
plot_Output_CaseCount_OverTime <- df_Output_CaseCount %>%
                                      f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                       inp_Y = N,
                                                       inp_GroupingFeature = CaseCountGroup,
                                                       inp_GroupingPosition = position_fill(),
                                                       inp_AxisType_y = "proportional",
                                                       inp_GroupingSpecs = c("More than 10 Cases",
                                                                             "5 - 10 Cases",
                                                                             "2 - 4 Cases",
                                                                             "1 Case"),
                                                       inp_GroupingMapping = "alpha",
                                                       inp_AlphaPalette = vc_AlphaPalette_4,
                                                       inp_FacetFeature = PatientSubgroup,
                                                       inp_ls_FacetArguments = list(dir = "v",
                                                                                    scales = "free_y"),
                                                       inp_FacetMapping = "fill",
                                                       inp_FillPalette = vc_FillPalette_Subgroup)

# Case count summary statistics
df_Output_CaseCount_Summary <- df_ADM_Patients %>%
                                    f_GetSampleStatistics(inp_MetricFeature = CaseCount,
                                                          inp_GroupingFeature = PatientSubgroup)

# Case count quantiles
df_Output_CaseCount_Quantiles <- df_ADM_Patients %>%
                                    f_GetSampleQuantiles(inp_MetricFeature = CaseCount,
                                                         inp_GroupingFeature = PatientSubgroup)

# Get empirical cumulative distribution functions
ls_Output_CaseCount_ECDF <- df_ADM_Patients %>%
                                f_GetECDF(inp_MetricFeature = FirstRelevantAdmissionAge,
                                          inp_GroupingFeature = PatientSubgroup)

# Box and Violin plots
plot_Output_CaseCount <- df_ADM_Patients %>%
                              f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                  inp_Y = CaseCount,
                                                  inp_OutlierAcrossAll = TRUE,
                                                  inp_OutlierQuantile = 0.95,
                                                  inp_LogTransform = FALSE,
                                                  inp_ShowViolinPlot = FALSE,
                                                  inp_AxisTitle_y = "Number of cases per patient",
                                                  inp_AxisLimits_y = c(0, NA_integer_),
                                                  inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean length of stay
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MeanLengthOfStay <- df_ADM_Patients %>%
                                  mutate(MeanLengthOfStayGroup = case_when(between(MeanLengthOfStay, 0, 7) == TRUE ~ "Up to 7 days on average",
                                                                           between(MeanLengthOfStay, 7.0001, 14) == TRUE ~ "7 - 14 days on average",
                                                                           between(MeanLengthOfStay, 14.0001, 30) == TRUE ~ "14 - 30 days on average",
                                                                           MeanLengthOfStay > 30 ~ "More than 30 days on average")) %>%
                                  group_by(PatientSubgroup, FirstRelevantAdmissionYear, MeanLengthOfStayGroup) %>%
                                  summarize(N = n())

# Column plot over time
plot_Output_MeanLengthOfStay_OverTime <- df_Output_MeanLengthOfStay %>%
                                              f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                               inp_Y = N,
                                                               inp_GroupingFeature = MeanLengthOfStayGroup,
                                                               inp_GroupingPosition = position_fill(),
                                                               inp_AxisType_y = "proportional",
                                                               inp_GroupingSpecs = c("More than 30 days on average",
                                                                                     "14 - 30 days on average",
                                                                                     "7 - 14 days on average",
                                                                                     "Up to 7 days on average"),
                                                               inp_GroupingMapping = "alpha",
                                                               inp_AlphaPalette = vc_AlphaPalette_4,
                                                               inp_FacetFeature = PatientSubgroup,
                                                               inp_ls_FacetArguments = list(dir = "v",
                                                                                            scales = "free_y"),
                                                               inp_FacetMapping = "fill",
                                                               inp_FillPalette = vc_FillPalette_Subgroup)

df_Output_MeanLengthOfStay_Summary <- df_ADM_Patients %>%
                                          f_GetSampleStatistics(inp_MetricFeature = MeanLengthOfStay,
                                                                inp_GroupingFeature = PatientSubgroup)

df_Output_MeanLengthOfStay_Quantiles <- df_ADM_Patients %>%
                                            f_GetSampleQuantiles(inp_MetricFeature = MeanLengthOfStay,
                                                                 inp_GroupingFeature= PatientSubgroup)

ls_Output_MeanLengthOfStay_ECDF <- df_ADM_Patients %>%
                                        f_GetECDF(inp_MetricFeature = MeanLengthOfStay,
                                                  inp_GroupingFeature = PatientSubgroup)

# Box and Violin plots
plot_Output_MeanLengthOfStay <- df_ADM_Patients %>%
                                    f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                        inp_Y = MeanLengthOfStay,
                                                        inp_OutlierAcrossAll = TRUE,
                                                        inp_OutlierQuantile = 0.95,
                                                        inp_LogTransform = FALSE,
                                                        inp_AxisTitle_y = "Mean length of stay per patient (days)",
                                                        inp_AxisLimits_y = c(0, NA_integer_),
                                                        inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of ICU transfers relative to number of admissions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_ADM_Patients_FilterICU <- df_ADM_Patients %>%
                                  filter(ICUTransfersAbsolute > 0)


df_Output_ICUTransfersRelative_Summary <- df_ADM_Patients_FilterICU %>%
                                              f_GetSampleStatistics(inp_MetricFeature = ICUTransfersRelative,
                                                                    inp_GroupingFeature = PatientSubgroup)

df_Output_ICUTransfersRelative_Quantiles <- df_ADM_Patients_FilterICU %>%
                                                f_GetSampleQuantiles(inp_MetricFeature = ICUTransfersRelative,
                                                                     inp_GroupingFeature = PatientSubgroup)

ls_Output_ICUTransfersRelative_ECDF <- df_ADM_Patients_FilterICU %>%
                                            f_GetECDF(inp_MetricFeature = ICUTransfersRelative,
                                                      inp_GroupingFeature = PatientSubgroup)

plot_Output_ICUTransfersRelative <- df_ADM_Patients_FilterICU %>%
                                        f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                            inp_Y = ICUTransfersRelative,
                                                            inp_OutlierAcrossAll = TRUE,
                                                            inp_OutlierQuantile = 0.95,
                                                            inp_LogTransform = FALSE,
                                                            inp_ShowViolinPlot = FALSE,
                                                            inp_AxisTitle_y = "ICU Transfers / Number of cases per patient",
                                                            inp_AxisLimits_y = c(0, NA_integer_),
                                                            inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time spent in Intensive Care relative to total time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_ICUTimeRelative_Summary <- df_ADM_Patients_FilterICU %>%
                                          f_GetSampleStatistics(inp_MetricFeature = ICUTimeRelative,
                                                                inp_GroupingFeature = PatientSubgroup)

df_Output_ICUTimeRelative_Quantiles <- df_ADM_Patients_FilterICU %>%
                                            f_GetSampleQuantiles(inp_MetricFeature = ICUTimeRelative,
                                                                 inp_GroupingFeature = PatientSubgroup)

ls_Output_ICUTimeRelative_ECDF <- df_ADM_Patients_FilterICU %>%
                                      f_GetECDF(inp_MetricFeature = ICUTimeRelative,
                                                inp_GroupingFeature = PatientSubgroup)

plot_Output_ICUTimeRelative <- df_ADM_Patients_FilterICU %>%
                                        f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                            inp_Y = ICUTimeRelative,
                                                            inp_OutlierAcrossAll = TRUE,
                                                            inp_OutlierQuantile = 0.95,
                                                            inp_LogTransform = FALSE,
                                                            inp_ShowViolinPlot = FALSE,
                                                            inp_AxisTitle_y = "Time spent in ICU / Total time",
                                                            inp_AxisLimits_y = c(0, NA_integer_),
                                                            inp_FillPalette = vc_FillPalette_Subgroup)


################################################################################
#
#          Cancer+/HIV+   vs.   Cancer+/HIV- 
#
################################################################################


#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case Count and Mean Length Of Stay (as also reported above)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Box and Violin plots
plot_Output_CaseCount_Cancer <- df_ADM_PatientsCancer %>%
                                    f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                        inp_Y = CaseCount,
                                                        inp_OutlierAcrossAll = TRUE,
                                                        inp_OutlierQuantile = 0.95,
                                                        inp_LogTransform = FALSE,
                                                        inp_ShowViolinPlot = FALSE,
                                                        inp_AxisTitle_y = "Number of cases per patient",
                                                        inp_AxisLimits_y = c(0, NA_integer_),
                                                        inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit


# Box and Violin plots
plot_Output_MeanLengthOfStay_Cancer <- df_ADM_PatientsCancer %>%
                                            f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                                inp_Y = MeanLengthOfStay,
                                                                inp_OutlierAcrossAll = TRUE,
                                                                inp_OutlierQuantile = 0.95,
                                                                inp_LogTransform = FALSE,
                                                                inp_AxisTitle_y = "Mean length of stay per patient (days)",
                                                                inp_AxisLimits_y = c(0, NA_integer_),
                                                                inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Documented time span of main cancer care
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MainCancerDocumentedTimeSpan_Summary <- df_ADM_PatientsCancer %>%
                                                      f_GetSampleStatistics(inp_MetricFeature = MainCancerDocumentedTimeSpan,
                                                                            inp_GroupingFeature = PatientSubgroup)

df_Output_MainCancerDocumentedTimeSpan_Quantiles <- df_ADM_PatientsCancer %>%
                                                        f_GetSampleQuantiles(inp_MetricFeature = MainCancerDocumentedTimeSpan,
                                                                             inp_GroupingFeature = PatientSubgroup)

ls_Output_MainCancerDocumentedTimeSpan_ECDF <- df_ADM_PatientsCancer %>%
                                                    f_GetECDF(inp_MetricFeature = MainCancerDocumentedTimeSpan,
                                                              inp_GroupingFeature = PatientSubgroup)

# Box and violin plots
plot_Output_MainCancerDocumentedTimeSpan <- df_ADM_PatientsCancer %>%
                                                f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                                    inp_Y = MainCancerDocumentedTimeSpan,
                                                                    inp_OutlierAcrossAll = TRUE,
                                                                    inp_OutlierQuantile = 0.90,
                                                                    inp_LogTransform = FALSE,
                                                                    inp_AxisTitle_y = "Recorded time span of main diagnosis (days)",
                                                                    inp_AxisLimits_y = c(0, NA_integer_),
                                                                    inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at presumed cancer diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at cancer diagnosis) distribution over time
df_Output_AgeAtCancerDiagnosis <- df_ADM_PatientsCancer %>%
                                      mutate(AgeGroup = case_when(between(MainCancerDiagnosisAge, 18, 39) == TRUE ~ "18 - 39 years old",
                                                                  between(MainCancerDiagnosisAge, 40, 59) == TRUE ~ "40 - 59 years old",
                                                                  between(MainCancerDiagnosisAge, 60, 79) == TRUE ~ "60 - 79 years old",
                                                                  MainCancerDiagnosisAge >= 80 ~ "> 80 years old")) %>%
                                      group_by(PatientSubgroup, MainCancerDiagnosisYear, AgeGroup) %>%
                                      summarize(N = n())

# Column plot over time
plot_Output_AgeAtCancerDiagnosis_OverTime <- df_Output_AgeAtCancerDiagnosis %>%
                                                  f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                   inp_Y = N,
                                                                   inp_GroupingFeature = AgeGroup,
                                                                   inp_GroupingPosition = position_fill(),
                                                                   inp_AxisType_y = "proportional",
                                                                   inp_GroupingSpecs = c("> 80 years old",
                                                                                         "60 - 79 years old",
                                                                                         "40 - 59 years old",
                                                                                         "18 - 39 years old"),
                                                                   inp_GroupingMapping = "alpha",
                                                                   inp_AlphaPalette = vc_AlphaPalette_4,
                                                                   inp_FacetFeature = PatientSubgroup,
                                                                   inp_ls_FacetArguments = list(dir = "v",
                                                                                                scales = "free_y"),
                                                                   inp_FacetMapping = "fill",
                                                                   inp_FillPalette = vc_FillPalette_Subgroup)

df_Output_AgeAtCancerDiagnosis_Summary <- df_ADM_PatientsCancer %>%
                                              f_GetSampleStatistics(inp_MetricFeature = MainCancerDiagnosisAge,
                                                                    inp_GroupingFeature = PatientSubgroup,
                                                                    inp_na.rm = TRUE)

df_Output_AgeAtCancerDiagnosis_Quantiles <- df_ADM_PatientsCancer %>%
                                                f_GetSampleQuantiles(inp_MetricFeature = MainCancerDiagnosisAge,
                                                                     inp_GroupingFeature = PatientSubgroup,
                                                                     inp_na.rm = TRUE)

ls_Output_AgeAtCancerDiagnosis_ECDF <- df_ADM_PatientsCancer %>%
                                            f_GetECDF(inp_MetricFeature = MainCancerDiagnosisAge,
                                                      inp_GroupingFeature = PatientSubgroup)

plot_Output_AgeAtCancerDiagnosis <- df_ADM_PatientsCancer %>%
                                        f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                            inp_Y = MainCancerDiagnosisAge,
                                                            inp_AxisTitle_y = "Age at cancer diagnosis",
                                                            inp_AxisLimits_y = c(0, NA_integer_),
                                                            inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_DistinctCodeCountCancer <- df_ADM_PatientsCancer %>%
                                          mutate(CancerCountGroup = case_when(DistinctCodeCountCancer == 1 ~ "1 distinct cancer",
                                                                              DistinctCodeCountCancer == 2 ~ "2 distinct cancers",
                                                                              DistinctCodeCountCancer > 2 ~ "3 or more distinct cancers")) %>%
                                          group_by(PatientSubgroup, FirstRelevantAdmissionYear, CancerCountGroup) %>%
                                          summarize(N = n())

plot_Output_DistinctCodeCountCancer_OverTime <- df_Output_DistinctCodeCountCancer %>%
                                                    f_MakeColumnPlot(inp_X = FirstRelevantAdmissionYear,
                                                                     inp_Y = N,
                                                                     inp_GroupingFeature = CancerCountGroup,
                                                                     inp_GroupingPosition = position_fill(),
                                                                     inp_AxisType_y = "proportional",
                                                                     inp_GroupingSpecs = c("3 or more distinct cancers",
                                                                                           "2 distinct cancers",
                                                                                           "1 distinct cancer"),
                                                                     inp_GroupingMapping = "alpha",
                                                                     inp_AlphaPalette = vc_AlphaPalette_3,
                                                                     inp_FacetFeature = PatientSubgroup,
                                                                     inp_ls_FacetArguments = list(dir = "v",
                                                                                                  scales = "free_y"),
                                                                     inp_FacetMapping = "fill",
                                                                     inp_FillPalette = vc_FillPalette_Subgroup)

df_Output_DistinctCodeCountCancer_Summary <- df_ADM_PatientsCancer %>%
                                                  f_GetSampleStatistics(inp_MetricFeature = DistinctCodeCountCancer,
                                                                        inp_GroupingFeature = PatientSubgroup,
                                                                        inp_na.rm = TRUE)

plot_Output_DistinctCodeCountCancer <- df_ADM_PatientsCancer %>%
                                            f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                                                                inp_Y = DistinctCodeCountCancer,
                                                                inp_OutlierAcrossAll = TRUE,
                                                                inp_OutlierQuantile = 1,
                                                                inp_LogTransform = FALSE,
                                                                inp_ShowViolinPlot = FALSE,
                                                                inp_AxisTitle_y = "Number of distinct cancer entities per patient",
                                                                inp_AxisLimits_y = c(0, NA_integer_),
                                                                inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: AD, NAD and Non-HIV-associated cancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_HIVCancerCategories <- df_ADM_PatientsCancer %>%
                                      group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory, MainCancerDiagnosisYear) %>%
                                          summarize(N = n())

plot_Output_HIVCancerCategories_OverTime <- df_Output_HIVCancerCategories %>%
                                                f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                 inp_Y = N,
                                                                 inp_GroupingFeature = PatientSubgroupHIVCancerCategory,
                                                                 inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                                                                       "HIV-associated non-AD cancer",
                                                                                       "HIV-associated AD cancer"),
                                                                 inp_GroupingPosition = position_fill(),
                                                                 inp_GroupingMapping = "alpha",
                                                                 inp_AxisType_y = "proportional",
                                                                 inp_FacetFeature = PatientSubgroup,
                                                                 inp_ls_FacetArguments = list(dir = "v"),
                                                                 inp_FacetMapping = "fill",
                                                                 inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_CIS <- df_ADM_PatientsCancer %>%
                      group_by(PatientSubgroup, MainCancerIsCarcinomaInSitu, MainCancerDiagnosisYear) %>%
                          summarize(N = n())

plot_Output_CIS_OverTime <- df_Output_CIS %>%
                                f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                 inp_Y = N,
                                                 inp_GroupingFeature = MainCancerIsCarcinomaInSitu,
                                                 inp_GroupingSpecs = c("Other" = FALSE,
                                                                       "Carcinoma in situ" = TRUE),
                                                 inp_GroupingPosition = position_fill(),
                                                 inp_GroupingMapping = "alpha",
                                                 inp_AxisType_y = "proportional",
                                                 inp_FacetFeature = PatientSubgroup,
                                                 inp_ls_FacetArguments = list(dir = "v"),
                                                 inp_FacetMapping = "fill",
                                                 inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MainCancerTopographyDetail <- df_ADM_PatientsCancer %>%
                                            group_by(PatientSubgroup, MainCancerTopographyDetail) %>%
                                                summarize(N = n()) %>%
                                            group_by(PatientSubgroup) %>%
                                                mutate(Proportion = N / sum(N)) %>%
                                            ungroup()

vc_TopographyDetailSorted <- df_Output_MainCancerTopographyDetail %>%
                                  filter(PatientSubgroup == "Cancer+/HIV+") %>%
                                  slice_max(n = 15,
                                            order_by = Proportion) %>%
                                  pull(MainCancerTopographyDetail)

# Make "pyramid plot"
plot_Output_MainCancerTopographyDetail <- df_Output_MainCancerTopographyDetail %>%
                                              mutate(Proportion = case_when(PatientSubgroup == "Cancer+/HIV-" ~ -Proportion,
                                                                            TRUE ~ Proportion)) %>%
                                              f_MakeColumnPlot(inp_X = MainCancerTopographyDetail,
                                                               inp_XSpecs = rev(vc_TopographyDetailSorted),
                                                               inp_Y = Proportion,
                                                               inp_GroupingFeature = PatientSubgroup,
                                                               inp_GroupingSpecs = c("Cancer+/HIV+", "Cancer+/HIV-"),
                                                               inp_ColumnWidth = 0.8,
                                                               inp_FillPalette = vc_FillPalette_Subgroup,
                                                               inp_AxisType_y = "proportional",
                                                               inp_CoordFlip = TRUE,
                                                               inp_TickLabelWidth_x = 40,
                                                               inp_LegendPosition = "top")

tmp_Limit <- ceiling(abs(max(df_Output_MainCancerTopographyDetail$Proportion)) * 10) / 10

# Manual modifications to comply pyramid plot needs
plot_Output_MainCancerTopographyDetail <- plot_Output_MainCancerTopographyDetail +
                                              theme(axis.text.y = element_text(margin = margin(r = 10, unit = "pt"))) +
                                              scale_y_continuous(limits = c(-tmp_Limit, tmp_Limit),
                                                                 breaks = seq(from = -tmp_Limit, to = tmp_Limit, by = 0.1),
                                                                 labels = function(x) paste(abs(round(x * 100, 0)), "%")) +
                                              scale_fill_manual(values = vc_FillPalette_Subgroup,
                                                                breaks = c("Cancer+/HIV-", "Cancer+/HIV+"),
                                                                name = NULL) +
                                              guides(fill = guide_legend(override.aes = list(alpha = 0.8)))


# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyDetail_OverTime <- df_ADM_PatientsCancer %>%
                                                      group_by(PatientSubgroup, MainCancerTopographyDetail, MainCancerDiagnosisYear) %>%
                                                          summarize(N = n()) %>%
                                                      group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                                          mutate(Proportion = N / sum(N)) %>%
                                                      ungroup()
                                                        


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MainCancerTopographyGroup <- df_ADM_PatientsCancer %>%
                                            group_by(PatientSubgroup, MainCancerTopographyGroup) %>%
                                                summarize(N = n()) %>%
                                            group_by(PatientSubgroup) %>%
                                                mutate(Proportion = N / sum(N)) %>%
                                            ungroup()

vc_TopographyGroupsSorted <- df_Output_MainCancerTopographyGroup %>%
                                  filter(PatientSubgroup == "Cancer+/HIV+") %>%
                                  arrange(desc(Proportion)) %>%
                                  pull(MainCancerTopographyGroup)

# Make "pyramid plot"
plot_Output_MainCancerTopographyGroup <- df_Output_MainCancerTopographyGroup %>%
                                              mutate(Proportion = case_when(PatientSubgroup == "Cancer+/HIV-" ~ -Proportion,
                                                                            TRUE ~ Proportion)) %>%
                                              f_MakeColumnPlot(inp_X = MainCancerTopographyGroup,
                                                               inp_XSpecs = rev(vc_TopographyGroupsSorted),
                                                               inp_Y = Proportion,
                                                               inp_GroupingFeature = PatientSubgroup,
                                                               inp_GroupingSpecs = c("Cancer+/HIV+", "Cancer+/HIV-"),
                                                               inp_ColumnWidth = 0.8,
                                                               inp_FillPalette = vc_FillPalette_Subgroup,
                                                               inp_AxisType_y = "proportional",
                                                               inp_CoordFlip = TRUE,
                                                               inp_TickLabelWidth_x = 40,
                                                               inp_LegendPosition = "top")

tmp_Limit <- ceiling(abs(max(df_Output_MainCancerTopographyGroup$Proportion)) * 10) / 10

# Manual modifications to comply pyramid plot needs
plot_Output_MainCancerTopographyGroup <- plot_Output_MainCancerTopographyGroup +
                                              theme(axis.text.y = element_text(margin = margin(r = 10, unit = "pt"))) +
                                              scale_y_continuous(limits = c(-tmp_Limit, tmp_Limit),
                                                                 breaks = seq(from = -tmp_Limit, to = tmp_Limit, by = 0.1),
                                                                 labels = function(x) paste(abs(round(x * 100, 0)), "%")) +
                                              scale_fill_manual(values = vc_FillPalette_Subgroup,
                                                                breaks = c("Cancer+/HIV-", "Cancer+/HIV+"),
                                                                name = NULL) +
                                              guides(fill = guide_legend(override.aes = list(alpha = 0.8)))

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyGroup_OverTime <- df_ADM_PatientsCancer %>%
                                                    group_by(PatientSubgroup, MainCancerTopographyGroup, MainCancerDiagnosisYear) %>%
                                                        summarize(N = n()) %>%
                                                    group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                                        mutate(Proportion = N / sum(N)) %>%
                                                    ungroup()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cancer occurrence grouped by coded entity
df_Output_CancerEntities <- df_ADM_PatientsCancer %>%
                                group_by(PatientSubgroup, MainCancerCode) %>%
                                    summarize(N = n()) %>%
                                group_by(PatientSubgroup) %>%
                                    mutate(Proportion = N / sum(N)) %>%
                                ungroup()

# Data stratified by year of cancer diagnosis
df_Output_CancerEntities_OverTime <- df_ADM_PatientsCancer %>%
                                          group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
                                              summarize(N = n()) %>%
                                          group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                              mutate(Proportion = N / sum(N)) %>%
                                          ungroup()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Metastasis occurrence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MetastasisOccurrence <- df_ADM_PatientsCancer %>%
                                      group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                      summarize(N = n(),
                                                CountMetastasis = sum(PatientHoldsMetastasisCodes == TRUE, na.rm = TRUE),
                                                ProportionMetastasis = CountMetastasis / N,
                                                CountNoMetastasis = N - CountMetastasis,
                                                ProportionNoMetastasis = CountNoMetastasis / N,
                                                CountMetastasisWithCancerDiagnosis = sum(TimeCancerToMetastasis == 0, na.rm = TRUE),
                                                ProportionMetastasisWithCancerDiagnosis = CountMetastasisWithCancerDiagnosis / CountMetastasis,
                                                CountMetastasisAfterCancerDiagnosis = CountMetastasis - CountMetastasisWithCancerDiagnosis,
                                                ProportionMetastasisAfterCancerDiagnosis = CountMetastasisAfterCancerDiagnosis / CountMetastasis)

# Column plot overall
plot_Output_MetastasisOccurrence <- df_Output_MetastasisOccurrence %>%
                                        select(PatientSubgroup,
                                               CountNoMetastasis,
                                               CountMetastasisWithCancerDiagnosis,
                                               CountMetastasisAfterCancerDiagnosis) %>%
                                        pivot_longer(cols = starts_with("Count"),
                                                     names_to = "Group",
                                                     values_to = "Count") %>% 
                                        f_MakeColumnPlot(inp_X = PatientSubgroup,
                                                         inp_XAdditionalMapping = "fill",
                                                         inp_FillPalette = vc_FillPalette_Subgroup,
                                                         inp_Y = Count,
                                                         inp_GroupingFeature = Group,
                                                         inp_GroupingSpecs = c("No Metastasis" = "CountNoMetastasis",
                                                                               "Metastasis after cancer diagnosis" = "CountMetastasisAfterCancerDiagnosis",
                                                                               "Metastasis with cancer diagnosis" = "CountMetastasisWithCancerDiagnosis"),
                                                         inp_GroupingPosition = position_fill(),
                                                         inp_GroupingMapping = "alpha",
                                                         inp_AxisType_y = "proportional",
                                                         inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                                                      inp_Theme_SizeFactorLegendLabels = 1.3))

# Column plot over time
plot_Output_MetastasisOccurrence_OverTime <- df_Output_MetastasisOccurrence %>%
                                                  select(PatientSubgroup,
                                                         MainCancerDiagnosisYear,
                                                         CountNoMetastasis,
                                                         CountMetastasisWithCancerDiagnosis,
                                                         CountMetastasisAfterCancerDiagnosis) %>%
                                                  pivot_longer(cols = starts_with("Count"),
                                                               names_to = "Group",
                                                               values_to = "Count") %>%
                                                  f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                   inp_Y = Count,
                                                                   inp_GroupingFeature = Group,
                                                                   inp_GroupingSpecs = c("No Metastasis" = "CountNoMetastasis",
                                                                                         "Metastasis after cancer diagnosis" = "CountMetastasisAfterCancerDiagnosis",
                                                                                         "Metastasis with cancer diagnosis" = "CountMetastasisWithCancerDiagnosis"),
                                                                   inp_GroupingPosition = position_fill(),
                                                                   inp_GroupingMapping = "alpha",
                                                                   inp_AxisType_y = "proportional",
                                                                   inp_FacetFeature = PatientSubgroup,
                                                                   inp_ls_FacetArguments = list(dir = "v"),
                                                                   inp_FacetMapping = "fill",
                                                                   inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time to Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_TimeCancerToMetastasis_Summary <- df_ADM_PatientsCancer %>%
                                                filter(!is.na(TimeCancerToMetastasis) & TimeCancerToMetastasis > 0) %>%   # Include only patients that presumably had no metastasis at time of cancer diagnosis
                                                f_GetSampleStatistics(inp_MetricFeature = TimeCancerToMetastasis,
                                                                      inp_GroupingFeature = PatientSubgroup,
                                                                      inp_na.rm = FALSE)

df_Output_TimeCancerToMetastasis_Quantiles <- df_ADM_PatientsCancer %>%
                                                  filter(!is.na(TimeCancerToMetastasis) & TimeCancerToMetastasis > 0) %>%
                                                  f_GetSampleQuantiles(inp_MetricFeature = TimeCancerToMetastasis,
                                                                       inp_GroupingFeature = PatientSubgroup,
                                                                       inp_na.rm = FALSE)

ls_Output_TimeCancerToMetastasis_ECDF <- df_ADM_PatientsCancer %>%
                                              filter(!is.na(TimeCancerToMetastasis) & TimeCancerToMetastasis > 0) %>%
                                              f_GetECDF(inp_MetricFeature = TimeCancerToMetastasis,
                                                        inp_GroupingFeature = PatientSubgroup,
                                                        inp_na.rm = FALSE)

plot_Output_TimeCancerToMetastasis <- ggsurvplot(fit = model_Output_TimeCancerToMetastasis,
                                                 data = df_ADM_PatientsCancer,
                                                 fun = "cumhaz",
                                                 conf.int = TRUE,
                                                 palette = c(color_CancerOnly, color_HIVCancer),
                                                 legend.title = "",
                                                 legend.labs = c("Cancer+/HIV-", "Cancer+/HIV+"))$plot +
                                          theme_CCP(inp_Theme_LegendPosition = "top") +
                                          scale_x_continuous(labels = function(value) round(value / 365, 0)) +
                                          coord_cartesian(xlim = c(0, 2000),
                                                          ylim = c(0, 5)) +
                                          labs(x = "Years after cancer diagnosis",
                                               y = "Cum. hazard of metastasis")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy modalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_TherapyModalities <- df_ADM_PatientsCancer %>%
                                    group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                        summarize(N = n(),
                                                  CountNoMajorTherapyCoded = sum(HadAnyCancerTherapy == FALSE, na.rm = TRUE),
                                                  CountAnyMajorTherapy = sum(HadAnyCancerTherapy == TRUE, na.rm = TRUE),
                                                  CountSurgery = sum(HadAnyCancerSurgery, na.rm = TRUE),
                                                  ProportionSurgery = CountSurgery / CountAnyMajorTherapy,
                                                  CountChemotherapy = sum(HadChemotherapy == TRUE, na.rm = TRUE),
                                                  ProportionChemotherapy = CountChemotherapy / CountAnyMajorTherapy,
                                                  CountImmunotherapy = sum(HadImmunotherapy == TRUE, na.rm = TRUE),
                                                  ProportionImmunotherapy = CountImmunotherapy / CountAnyMajorTherapy,
                                                  CountRadiotherapy = sum(HadRadiotherapy == TRUE, na.rm = TRUE),
                                                  ProportionRadiotherapy = CountRadiotherapy / CountAnyMajorTherapy,
                                                  CountNuclearMedicineTherapy = sum(HadNuclearmedTherapy == TRUE, na.rm = TRUE),
                                                  ProportionNuclearMedicineTherapy = CountNuclearMedicineTherapy / CountAnyMajorTherapy,
                                                  CountStemCellTherapy = sum(HadStemCellTherapy == TRUE, na.rm = TRUE),
                                                  ProportionStemCellTherapy = CountStemCellTherapy / CountAnyMajorTherapy,
                                                  CountBoneMarrowTransplant = sum(HadBoneMarrowTransplant == TRUE, na.rm = TRUE),
                                                  ProportionBoneMarrowTransplant = CountBoneMarrowTransplant / CountAnyMajorTherapy,
                                                  CountCARTCellTherapy = sum(HadPotentialCARTCellTherapy == TRUE, na.rm = TRUE),
                                                  ProportionCARTCellTherapy = CountCARTCellTherapy / CountAnyMajorTherapy)

# Column plot overall
plot_Output_AnyMajorTherapy <- df_Output_TherapyModalities %>%
                                    select(PatientSubgroup,
                                           CountNoMajorTherapyCoded,
                                           CountAnyMajorTherapy) %>%
                                    pivot_longer(cols = starts_with("Count"),
                                                 names_to = "Group",
                                                 values_to = "Count") %>%
                                    f_MakeColumnPlot(inp_X = PatientSubgroup,
                                                     inp_XAdditionalMapping = "fill",
                                                     inp_FillPalette = vc_FillPalette_Subgroup,
                                                     inp_Y = Count,
                                                     inp_GroupingFeature = Group,
                                                     inp_GroupingPosition = position_fill(),
                                                     inp_GroupingSpecs = c("No Major Therapy coded" = "CountNoMajorTherapyCoded",
                                                                           "Any Major Therapy" = "CountAnyMajorTherapy"),
                                                     inp_GroupingMapping = "alpha",
                                                     inp_AxisType_y = "proportional",
                                                     inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                                                  inp_Theme_SizeFactorLegendLabels = 1.3))

# Column plot over time
plot_Output_AnyMajorTherapy_OverTime <- df_Output_TherapyModalities %>%
                                            select(PatientSubgroup,
                                                   MainCancerDiagnosisYear,
                                                   CountNoMajorTherapyCoded,
                                                   CountAnyMajorTherapy) %>%
                                            pivot_longer(cols = starts_with("Count"),
                                                         names_to = "Group",
                                                         values_to = "Count") %>%
                                            f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                             inp_Y = Count,
                                                             inp_GroupingFeature = Group,
                                                             inp_GroupingSpecs = c("No Major Therapy coded" = "CountNoMajorTherapyCoded",
                                                                                   "Any Major Therapy" = "CountAnyMajorTherapy"),
                                                             inp_GroupingPosition = position_fill(),
                                                             inp_GroupingMapping = "alpha",
                                                             inp_AxisType_y = "proportional",
                                                             inp_FacetFeature = PatientSubgroup,
                                                             inp_ls_FacetArguments = list(dir = "v"),
                                                             inp_FacetMapping = "fill",
                                                             inp_FillPalette = vc_FillPalette_Subgroup)

# Column plot overall
plot_Output_TherapyModalities <- df_Output_TherapyModalities %>%
                                      pivot_longer(cols = starts_with("Proportion"),
                                                   names_to = "Group",
                                                   values_to = "Proportion") %>%
                                      f_MakeColumnPlot(inp_X = Group,
                                                       inp_XSpecs = c("Surgery" = "ProportionSurgery",
                                                                      "Chemo- therapy" = "ProportionChemotherapy",
                                                                      "Radio- therapy" = "ProportionRadiotherapy",
                                                                      "Immuno- therapy" = "ProportionOtherImmunotherapy",
                                                                      "Nuclear Medicine Therapy" = "ProportionNuclearMedicineTherapy",
                                                                      "Stem Cell Therapy" = "ProportionStemCellTherapy",
                                                                      "Bone Marrow Transplant" = "ProportionBoneMarrowTransplant",
                                                                      "CAR-T-Cell Therapy" = "ProportionCARTCellTherapy"),
                                                       inp_Y = Proportion,
                                                       inp_GroupingFeature = PatientSubgroup,
                                                       inp_GroupingPosition = position_dodge(),
                                                       inp_FillPalette = vc_FillPalette_Subgroup,
                                                       inp_AxisType_y = "proportional",
                                                       inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                       inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                                                    inp_Theme_SizeFactorLegendLabels = 1.3))

# Column plots over time for all therapy modalities
plot_Output_Surgery_OverTime <- df_Output_TherapyModalities %>%
                                    f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                     inp_Y = ProportionSurgery,
                                                     inp_AxisType_y = "proportional",
                                                     inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                     inp_FacetFeature = PatientSubgroup,
                                                     inp_ls_FacetArguments = list(dir = "h"),
                                                     inp_FacetMapping = "fill",
                                                     inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_Chemotherapy_OverTime <- df_Output_TherapyModalities %>%
                                          f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                           inp_Y = ProportionChemotherapy,
                                                           inp_AxisType_y = "proportional",
                                                           inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                           inp_FacetFeature = PatientSubgroup,
                                                           inp_ls_FacetArguments = list(dir = "v"),
                                                           inp_FacetMapping = "fill",
                                                           inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_Radiotherapy_OverTime <- df_Output_TherapyModalities %>%
                                          f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                           inp_Y = ProportionRadiotherapy,
                                                           inp_AxisType_y = "proportional",
                                                           inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                           inp_FacetFeature = PatientSubgroup,
                                                           inp_ls_FacetArguments = list(dir = "v"),
                                                           inp_FacetMapping = "fill",
                                                           inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_Immunotherapy_OverTime <- df_Output_TherapyModalities %>%
                                          f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                           inp_Y = ProportionImmunotherapy,
                                                           inp_AxisType_y = "proportional",
                                                           inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                           inp_FacetFeature = PatientSubgroup,
                                                           inp_ls_FacetArguments = list(dir = "v"),
                                                           inp_FacetMapping = "fill",
                                                           inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_NuclearMedicineTherapy_OverTime <- df_Output_TherapyModalities %>%
                                                    f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                     inp_Y = ProportionNuclearMedicineTherapy,
                                                                     inp_AxisType_y = "proportional",
                                                                     inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                                     inp_FacetFeature = PatientSubgroup,
                                                                     inp_ls_FacetArguments = list(dir = "v"),
                                                                     inp_FacetMapping = "fill",
                                                                     inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_StemCellTherapy_OverTime <- df_Output_TherapyModalities %>%
                                            f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                             inp_Y = ProportionStemCellTherapy,
                                                             inp_AxisType_y = "proportional",
                                                             inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                             inp_FacetFeature = PatientSubgroup,
                                                             inp_ls_FacetArguments = list(dir = "v"),
                                                             inp_FacetMapping = "fill",
                                                             inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_BoneMarrowTransplant_OverTime <- df_Output_TherapyModalities %>%
                                                  f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                   inp_Y = ProportionBoneMarrowTransplant,
                                                                   inp_AxisType_y = "proportional",
                                                                   inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                                   inp_FacetFeature = PatientSubgroup,
                                                                   inp_ls_FacetArguments = list(dir = "v"),
                                                                   inp_FacetMapping = "fill",
                                                                   inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_CARTCellTherapy_OverTime <- df_Output_TherapyModalities %>%
                                            f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                             inp_Y = ProportionCARTCellTherapy,
                                                             inp_AxisType_y = "proportional",
                                                             inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                                                             inp_FacetFeature = PatientSubgroup,
                                                             inp_ls_FacetArguments = list(dir = "v"),
                                                             inp_FacetMapping = "fill",
                                                             inp_FillPalette = vc_FillPalette_Subgroup)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy sequence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
df_Output_CancerTherapySequence <- df_ADM_PatientsCancer %>%
                                        ggsankey::make_long(TherapyOnset_1,
                                                            TherapyOnset_2,
                                                            TherapyOnset_3,
                                                            TherapyOnset_4,
                                                            TherapyOnset_5,
                                                            TherapyOnset_6) %>%
                                        filter(!is.na(node))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complications after (Chemo)therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_TherapyComplications <- df_ADM_PatientsCancer %>%
                                            group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                            summarize(N = n(),
                                                      CountAnyMajorTherapy = sum(HadAnyCancerTherapy == TRUE, na.rm = TRUE),
                                                      ProportionAnyMajorTherapy = CountAnyMajorTherapy / N,
                                                      CountChemotherapy = sum(HadChemotherapy == TRUE, na.rm = TRUE),
                                                      ProportionChemotherapy = CountChemotherapy / CountAnyMajorTherapy,
                                                      CountNoChemotherapy = CountAnyMajorTherapy - CountChemotherapy,
                                                      ProportionNoChemotherapy = CountNoChemotherapy / CountAnyMajorTherapy,
                                                      CountComplicationAfterChemotherapy = sum(HadComplicationAfterChemo == TRUE, na.rm = TRUE),
                                                      ProportionComplicationAfterChemotherapy = CountComplicationAfterChemotherapy / CountChemotherapy,
                                                      CountNoComplicationAfterChemotherapy = CountChemotherapy - CountComplicationAfterChemotherapy,
                                                      ProportionNoComplicationAfterChemotherapy = CountNoComplicationAfterChemotherapy / CountChemotherapy)

plot_Output_TherapyComplications <- df_Output_TherapyComplications %>%
                                        select(PatientSubgroup,
                                               CountNoChemotherapy,
                                               CountNoComplicationAfterChemotherapy,
                                               CountComplicationAfterChemotherapy) %>%
                                        pivot_longer(cols = starts_with("Count"),
                                                     names_to = "Group",
                                                     values_to = "Count") %>% 
                                        f_MakeColumnPlot(inp_X = PatientSubgroup,
                                                         inp_XAdditionalMapping = "fill",
                                                         inp_FillPalette = vc_FillPalette_Subgroup,
                                                         inp_Y = Count,
                                                         inp_GroupingFeature = Group,
                                                         inp_GroupingSpecs = c("No Chemotherapy" = "CountNoChemotherapy",
                                                                               "Chemotherapy without Complication" = "CountNoComplicationAfterChemotherapy",
                                                                               "Chemotherapy with Complication" = "CountComplicationAfterChemotherapy"),
                                                         inp_GroupingPosition = position_fill(),
                                                         inp_GroupingMapping = "alpha",
                                                         inp_AxisType_y = "proportional",
                                                         inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                                                      inp_Theme_SizeFactorLegendLabels = 1.3))

# Column plot over time
plot_Output_TherapyComplications_OverTime <- df_Output_TherapyComplications %>%
                                                  select(PatientSubgroup,
                                                         MainCancerDiagnosisYear,
                                                         CountNoChemotherapy,
                                                         CountNoComplicationAfterChemotherapy,
                                                         CountComplicationAfterChemotherapy) %>%
                                                  pivot_longer(cols = starts_with("Count"),
                                                               names_to = "Group",
                                                               values_to = "Count") %>%
                                                  f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                                                                   inp_Y = Count,
                                                                   inp_GroupingFeature = Group,
                                                                   inp_GroupingSpecs = c("No Chemotherapy" = "CountNoChemotherapy",
                                                                                         "Chemotherapy without Complication" = "CountNoComplicationAfterChemotherapy",
                                                                                         "Chemotherapy with Complication" = "CountComplicationAfterChemotherapy"),
                                                                   inp_GroupingPosition = position_fill(),
                                                                   inp_GroupingMapping = "alpha",
                                                                   inp_AxisType_y = "proportional",
                                                                   inp_FacetFeature = PatientSubgroup,
                                                                   inp_ls_FacetArguments = list(dir = "v"),
                                                                   inp_FacetMapping = "fill",
                                                                   inp_FillPalette = vc_FillPalette_Subgroup)

# ECDF
ls_Output_TimeChemoToComplication_ECDF <- df_ADM_PatientsCancer %>%
                                              f_GetECDF(inp_MetricFeature = TimeChemoToFirstComplication,
                                                        inp_GroupingFeature = PatientSubgroup,
                                                        inp_na.rm = TRUE)

# Kaplan-Meier-Plot
plot_Output_TimeChemoToComplication <- ggsurvplot(fit = model_Output_TimeChemoToComplication_Curve,
                                                  data = df_ADM_PatientsCancer,
                                                  fun = "event",
                                                  conf.int = TRUE,
                                                  palette = c(color_CancerOnly, color_HIVCancer),
                                                  legend.title = "",
                                                  legend.labs = c("Cancer+/HIV-", "Cancer+/HIV+"))$plot +
                                          theme_CCP(inp_Theme_LegendPosition = "top") +
                                          scale_x_continuous(labels = function(x) x) +
                                          coord_cartesian(xlim = c(0, 30),
                                                          ylim = c(0, NA)) +
                                          labs(x = "Days after corresponding chemotherapy administration",
                                               y = "Cumulative event percentage")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Last recorded discharge reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_LastRecordedDischargeCategory <- df_ADM_PatientsCancer %>%
                                                group_by(PatientSubgroup, LastRecordedDischargeCategory, MainCancerDiagnosisYear) %>%
                                                    summarize(N = n()) %>%
                                                group_by(PatientSubgroup) %>%
                                                    mutate(Proportion = N / sum(N)) %>%
                                                ungroup()

plot_Output_LastRecordedDischargeCategory <- df_Output_LastRecordedDischargeCategory %>%
                                                  f_MakeColumnPlot(inp_X = PatientSubgroup,
                                                                   inp_XAdditionalMapping = "fill",
                                                                   inp_FillPalette = vc_FillPalette_Subgroup,
                                                                   inp_Y = N,
                                                                   inp_GroupingFeature = LastRecordedDischargeCategory,
                                                                   inp_GroupingSpecs = c("Other" = "Other",
                                                                                         "Rehabilitation or Residential Care" = "Rehabilitation or Residential Care",
                                                                                         "Home" = "Home",
                                                                                         "Other Hospital" = "Other Hospital",
                                                                                         "Hospice Care" = "Hospice Care",
                                                                                         "Deceased" = "Deceased"),
                                                                   inp_GroupingPosition = "fill",
                                                                   inp_GroupingMapping = "alpha",
                                                                   inp_AxisType_y = "proportional")




################################################################################
#
#          Cancer+/HIV+
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer+/HIV+ Most commonly affected organs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Aux_TopographyDetailTop10_Total <- df_Output_MainCancerTopographyDetail_OverTime %>%
                                          filter(PatientSubgroup == "Cancer+/HIV+") %>%
                                          group_by(MainCancerTopographyDetail) %>%
                                              summarize(TotalCount = sum(N)) %>%
                                              arrange(desc(TotalCount)) %>%
                                              slice_head(n = 10)
                                    
df_Aux_TopographyDetailTop10_Recent <- df_Output_MainCancerTopographyDetail_OverTime %>%
                                            filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerDiagnosisYear %in% 2018:2022) %>%
                                            group_by(MainCancerTopographyDetail) %>%
                                                summarize(TotalCount = sum(N)) %>%
                                                arrange(desc(TotalCount)) %>%
                                                slice_head(n = 10)

df_Test <- df_Output_MainCancerTopographyDetail_OverTime %>%
                                                        filter(PatientSubgroup == "Cancer+/HIV+"
                                                                   & MainCancerTopographyDetail %in% df_Aux_TopographyDetailTop10_Total$MainCancerTopographyDetail)


plot_Output_MainCancerTopographyDetail_OverTime <- df_Output_MainCancerTopographyDetail_OverTime %>%
                                                        filter(PatientSubgroup == "Cancer+/HIV+"
                                                                   & MainCancerTopographyDetail %in% df_Aux_TopographyDetailTop10_Total$MainCancerTopographyDetail) %>%
                                                        f_MakeLinePlot(inp_X = MainCancerDiagnosisYear,
                                                                       inp_Y = Proportion,
                                                                       inp_GroupingFeature = MainCancerTopographyDetail)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIV and cancer presumed diagnosis order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_HIVCancerDiagnosisOrder <- df_ADM_PatientsHIVCancer %>%
                                          group_by(HIVCancerCategory, HIVCancerDiagnosisOrder, MainCancerDiagnosisYear) %>%
                                              summarize(N = n()) %>% 
                                          ungroup() %>%
                                          complete(HIVCancerCategory, HIVCancerDiagnosisOrder, MainCancerDiagnosisYear, fill = list(N = 0))

# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
df_Output_HIVCancerSequence <- df_ADM_PatientsHIVCancer %>%
                                    ggsankey::make_long(DiagnosisSequence_1, DiagnosisSequence_2, DiagnosisSequence_3, DiagnosisSequence_4) %>%
                                    filter(!is.na(node))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence in HIV-positive cancer patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_HIVCancerAIDS <- df_ADM_PatientsHIVCancer %>%
                                group_by(AIDSOccurrence, MainCancerDiagnosisYear) %>%
                                    summarize(N = n()) %>% 
                                ungroup() %>%
                                complete(AIDSOccurrence, MainCancerDiagnosisYear, fill = list(N = 0))




################################################################################
#
#          Cancer+/HIV+   vs.   Cancer-/HIV+ 
#
################################################################################


#---------- HIV Status ---------------------------------------------------------

#!!! Add variable in df_ADM_Patients HIV Status...



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence across all HIV-positive patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_AIDSOccurrence <- df_ADM_Patients %>%
                                filter(PatientHoldsHIVCodes == TRUE) %>%
                                group_by(PatientSubgroup, FirstRelevantAdmissionYear) %>%
                                summarize(N = n(),
                                          CountAIDS = sum(PatientHoldsAIDSCodes == TRUE, na.rm = TRUE),
                                          ProportionAIDS = CountAIDS / N,
                                          CountNoAIDS = N - CountAIDS,
                                          ProportionNoAIDS = CountNoAIDS / N,
                                          CountAIDSWithHIVDiagnosis = sum(TimeHIVToAIDS == 0, na.rm = TRUE),
                                          ProportionAIDSWithHIVDiagnosis = CountAIDSWithHIVDiagnosis / CountAIDS,
                                          CountAIDSAfterHIVDiagnosis = CountAIDS - CountAIDSWithHIVDiagnosis,
                                          ProportionAIDSAfterHIVDiagnosis = CountAIDSAfterHIVDiagnosis / CountAIDS)





################################################################################
#
#          Exporting of Plots
#
################################################################################


# f_ExportPlot(inp_Plot = plot_MatchingDistributionPlots, inp_Directory = PlotOutputPath, inp_Width = 10, inp_Height = 10)
# f_ExportPlot(inp_Plot = plot_MatchingLovePlot, inp_Directory = PlotOutputPath, inp_Width = 10, inp_Height = 10)
# f_ExportPlot(inp_Plot = plot_MatchingPropensityScorePlot, inp_Directory = PlotOutputPath, inp_Width = 10, inp_Height = 10)


f_ExportPlot(inp_Plot = plot_Output_Age, inp_Directory = PlotOutputPath, inp_Width = 10, inp_Height = 10)
f_ExportPlot(inp_Plot = plot_Output_Age_OverTime, inp_Directory = PlotOutputPath, inp_Width = 20, inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_AnyMajorTherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_BoneMarrowTransplant_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_CARTCellTherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_CaseCount,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_CaseCount_Cancer,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_CaseCount_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_Chemotherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_CIS_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_HIVCancerCategories_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_ICUTimeRelative,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_ICUTransfersRelative,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_Immunotherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_LastRecordedDischargeCategory,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_MainCancerDocumentedTimeSpan,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MainCancerTopographyDetail,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MainCancerTopographyGroup,
             inp_Directory = PlotOutputPath,
             inp_Width = 14,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MeanLengthOfStay,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MeanLengthOfStay_Cancer,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MeanLengthOfStay_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MetastasisOccurrence,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_MetastasisOccurrence_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_NuclearMedicineTherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_Radiotherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_Sex_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_StemCellTherapy_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_Surgery_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TherapyComplications,
             inp_Directory = PlotOutputPath,
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TherapyComplications_OverTime,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TherapyModalities,
             inp_Directory = PlotOutputPath,
             inp_Width = 16,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TimeCancerToMetastasis,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TimeChemoToComplication,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)






# f_ExportPlot(inp_Plot = plot_Output_CIS,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 10)
# 
# f_ExportPlot(inp_Plot = plot_Output_HIVCancerOverview,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 15)
# 
# f_ExportPlot(inp_Plot = plot_Output_MeanLengthOfStay,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 16,
#              inp_Height = 12)
# 
# f_ExportPlot(inp_Plot = plot_Output_MetastasisOccurrence,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 10)
# 
# f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 16,
#              inp_Height = 10)
# 
# f_ExportPlot(inp_Plot = plot_Output_SankeyDiagnosisProgress,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 16)
# 
# f_ExportPlot(inp_Plot = plot_Output_SankeyTherapySequence,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 80,
#              inp_Height = 50)
# 
# f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 10)
# 
# f_ExportPlot(inp_Plot = plot_Output_TherapyModalities,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 14,
#              inp_LegendPosition = c(0.8, 0.68))
# 
# f_ExportPlot(inp_Plot = plot_Output_TimeCancerToMetastasis,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 15,
#              inp_Height = 9,
#              inp_LegendPosition = c(0.3, 0.82))
# 
# f_ExportPlot(inp_Plot = plot_Output_TimeChemoToComplication,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 15,
#              inp_Height = 9,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = map_PatientPostalCodes,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 30,
#              inp_Height = 15)
# 
# 
# 
# f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_A,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_B,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_C,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# 
# f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_A,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_B,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_C,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# 
# f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_A,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_B,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
# 
# f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_C,
#              inp_Directory = PlotOutputPath,
#              inp_Width = 20,
#              inp_Height = 5,
#              inp_LegendPosition = "none")
