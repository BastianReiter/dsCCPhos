

################################################################################
#------------------------------------------------------------------------------#
#   INPUT DATA HARMONIZATION                                                   #
#------------------------------------------------------------------------------#
################################################################################



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING SETUP: Definition of Features to Monitor during Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Object Syntax: List of vectors
#     - Vector Names = Name of Feature to be monitored during Harmonization
#     - Vector Values = Set of eligible values defined in Meta Data
# If a Feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

ls_MonitorFeatures_df_IDM_Cases <- list(Sex = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "Sex")$Value,
                                        AdmissionCauseCode = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "AdmissionCauseCode")$Value,
                                        DischargeReasonCode = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "DischargeReasonCode")$Value)


ls_MonitorFeatures_df_IDM_CasesDepartment <- list(DepartmentCode = filter(df_Meta_ValueSets, Table == "CasesDepartment" & Feature == "DepartmentCode")$Value,
                                                  DepartmentAdmissionToICU = NULL)


ls_MonitorFeatures_df_IDM_CasesICD <- list(DiagnosisType = filter(df_Meta_ValueSets, Table == "CasesICD" & Feature == "DiagnosisType")$Value)



# Put all objects in one list object to make them passable to functions
ls_MonitorFeatures_All <- list(ls_MonitorFeatures_df_IDM_Cases,
                               ls_MonitorFeatures_df_IDM_CasesDepartment,
                               ls_MonitorFeatures_df_IDM_CasesICD)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values of Raw Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Raw <- list(df_IDM_Cases,
                             df_IDM_CasesDepartment,
                             df_IDM_CasesICD)


ls_Monitors_Raw <- map2(.x = ls_MonitoredData_Raw,
                        .y = ls_MonitorFeatures_All,
                        .f = function(inp_df, inp_ls_MonitorFeatures)
                             {  
                                inp_df %>%
                                    f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                         inp_ProcessingStatus = "Raw")
                             })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HARMONIZATION / Transformation Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_IDM_Cases <- df_IDM_Cases %>%
                mutate(Sex = str_to_lower(Sex),      # Convert all upper to lower letters
                       Sex = str_remove_all(Sex, " "),      # Eliminate spaces
                       Sex = f_Recode(Sex, with(filter(df_Meta_ValueSets, Table == "Cases" & Feature == "Sex"),      # Looking up Feature to transform in Meta Data Table of Eligible Values
                                                set_names(Value, OriginalValue))),      # This returns a vector of the form c("OriginalValue1" = "Value1", ...), thereby inducing replacement of original values with new ones as defined in Meta Data
                       #----------------------------------------------------
                       AdmissionCauseCode = f_Recode(AdmissionCauseCode, with(filter(df_Meta_ValueSets, Table == "Cases" & Feature == "AdmissionCauseCode"),
                                                                              set_names(Value, OriginalValue))))


df_IDM_CasesDepartment <- df_IDM_CasesDepartment %>%
                              mutate(DepartmentAdmissionToICU = case_match(DepartmentAdmissionToICU,
                                                                           "J" ~ TRUE,
                                                                           "N" ~ FALSE))

df_IDM_CasesICD <- df_IDM_CasesICD %>%
                        mutate(ICDVersion = as.integer(ICDVersion))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values after Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Transformed <- list(df_IDM_Cases,
                                     df_IDM_CasesDepartment,
                                     df_IDM_CasesICD)


ls_Monitors_Transformed <- map2(.x = ls_MonitoredData_Transformed,
                                .y = ls_MonitorFeatures_All,
                                .f = function(inp_df, inp_ls_MonitorFeatures)
                                {  
                                    inp_df %>%
                                        f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                             inp_ProcessingStatus = "Transformed")
                                })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finalize Harmonization of Data: Ineligible data (includes data that could not be harmonized) is turned into NA using factor conversion or other methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_IDM_Cases <- df_IDM_Cases %>%
                    mutate(Sex = factor(Sex, levels = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "Sex")$Value),      # Convert to factor to mark ineligible values as NA and establish level order where appropriate
                           AdmissionCauseCode = factor(AdmissionCauseCode, levels = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "AdmissionCauseCode")$Value),
                           DischargeReasonCode = factor(DischargeReasonCode, levels = filter(df_Meta_ValueSets, Table == "Cases" & Feature == "DischargeReasonCode")$Value))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Track Feature Values after Finalized Harmonization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ls_MonitoredData_Final <- list(df_IDM_Cases,
                               df_IDM_CasesDepartment,
                               df_IDM_CasesICD)


ls_Monitors_Final <- map2(.x = ls_MonitoredData_Final,
                          .y = ls_MonitorFeatures_All,
                          .f = function(inp_df, inp_ls_MonitorFeatures)
                          {  
                              inp_df %>%
                                   f_TrackFeatureValues(inp_Features = inp_ls_MonitorFeatures,
                                                       inp_ProcessingStatus = "Final")
                          })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MONITORING: Merge Monitor Objects into Coherent Summaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ls_MonitorSummaries <- list()

for (i in 1:length(ls_Monitors_Raw))
{
    df_Aux_CurrentSummary <- ls_Monitors_Raw[[i]] %>%
                                  full_join(ls_Monitors_Transformed[[i]]) %>%
                                  full_join(ls_Monitors_Final[[i]]) %>%
                                  pivot_wider(names_from = ProcessingStatus,
                                              values_from = Frequency) %>%
                                  group_by(Feature, Value, IsValueEligible) %>%
                                  summarize(Raw = sum(Raw, na.rm = TRUE),
                                            Transformed = sum(Transformed, na.rm = TRUE),
                                            Final = sum(Final, na.rm = TRUE)) %>%
                                  arrange(Feature, desc(IsValueEligible))
    
    ls_MonitorSummaries <- c(ls_MonitorSummaries, list(df_Aux_CurrentSummary))
}


names(ls_MonitorSummaries) <- c("Monitor_df_IDM_Cases",
                                "Monitor_df_IDM_CasesDepartment",
                                "Monitor_df_IDM_CasesICD")


# Save Monitor Objects to make them accessible for Reporting
save(list = "ls_MonitorSummaries",
     file = here("Reporting/Monitoring", paste0("MonitorData_", SiteName, ".RData")))


