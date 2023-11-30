

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: PROCESSED DATA VALIDATION                                         #
#------------------------------------------------------------------------------#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_ADM_Diagnoses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Column Label      |   Data Type  |  Format / Example    |   Value Restr.
# ------------------|--------------|----------------------|---------------------
# PatientPseudonym  |   character  |  "1234567890123456"  | 
# CasePseudonym     |   character  |  "1234567890123456"  | 
# AdmissionDate     |   date       |  2000-01-01          |   
# AdmissionYear     |   integer    |  2005                |
# AdmissionAge      |   integer    |  50                  |
# ...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_ADM_Patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_ADM_Patients <- df_ADM_Patients %>%
                                  create_agent() %>%
                                  col_vals_not_null(vars(PatientPseudonym,
                                                         PatientSubgroup)) %>%
                                  col_vals_between(vars(MeanLengthOfStay), left = 0, right = 1000) %>%
                                  interrogate()

# Create Validation Report as tibble
ValidationReport_df_ADM_Patients <- Validation_df_ADM_Patients %>%
                                          get_agent_report(display_table = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_ADM_PatientsCancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Column Label                |   Data Type  |  Format / Example    |   Value Restr.
# ----------------------------|--------------|----------------------|---------------------
# PresumedMainCancerDiagnosis |   character  |  "..."               |
# CancerTopographyDetail      |   character  |  "..."               |
# CancerTopographyGroup       |   character  |  "..."               |
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_ADM_PatientsCancer <- df_ADM_PatientsCancer %>%
                                        create_agent() %>%
                                        col_vals_not_null(vars(Sex,
                                                               MainCancerCodeShort,
                                                               MainCancerDiagnosisAge,
                                                               MainCancerDiagnosisYear,
                                                               MainCancerTopographyGroup,
                                                               MainCancerIsCarcinomaInSitu)) %>%
                                        interrogate()

# Create Validation Report as tibble
ValidationReport_df_ADM_PatientsCancer <- Validation_df_ADM_PatientsCancer %>%
                                              get_agent_report(display_table = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual Plausibility Check: Random patient selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RandomCancerPatientPseudonym <- sample(df_ADM_PatientsCancer$PatientPseudonym, size = 1)

df_RandomCancerPatient <- df_ADM_PatientsCancer %>%
                              filter(PatientPseudonym == RandomCancerPatientPseudonym)

