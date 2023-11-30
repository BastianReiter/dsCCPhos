

################################################################################
#------------------------------------------------------------------------------#
#   VALIDATION: HARMONIZED INPUT DATA                                          #
#------------------------------------------------------------------------------#
################################################################################


#   ==> For Validation Rules, see Project Documentation -> Harmonized Input Data Model (HIDM)

#   Required Objects:
#   ~~~~~~~~~~~~~~~~~
#   df_IDM_Cases
#   df_IDM_CasesDepartment
#   df_IDM_CasesICD
#   df_IDM_CasesOPS


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_IDM_Cases
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_IDM_Cases_Harmonized <- df_IDM_Cases %>%
                                          create_agent() %>%
                                              col_exists(vars(PatientPseudonym,
                                                              CasePseudonym,
                                                              YearOfBirth,
                                                              Sex,
                                                              PostalCode,
                                                              AdmissionDate,
                                                              AdmissionAge,
                                                              AdmissionCauseCode,
                                                              TimeInICU,
                                                              DischargeDate,
                                                              DischargeReasonCode)) %>%
                                              col_vals_not_null(vars(PatientPseudonym,
                                                                     CasePseudonym,
                                                                     YearOfBirth,
                                                                     Sex,
                                                                     PostalCode,
                                                                     AdmissionDate,
                                                                     AdmissionAge,
                                                                     AdmissionCauseCode,
                                                                     TimeInICU,
                                                                     DischargeDate,
                                                                     DischargeReasonCode)) %>%
                                              col_vals_between(vars(YearOfBirth), left = 1890, right = 2006) %>%
                                              col_vals_in_set(vars(Sex), filter(df_Meta_ValueSets, Table == "Cases" & Feature == "Sex")$Value) %>%
                                              col_vals_between(vars(AdmissionDate), left = as_date("2005-01-01"), right = as_date("2022-12-31")) %>%
                                              col_vals_between(vars(AdmissionAge), left = 18, right = 120) %>%
                                              col_vals_in_set(vars(AdmissionCauseCode), filter(df_Meta_ValueSets, Table == "Cases" & Feature == "AdmissionCauseCode")$Value) %>%
                                              col_vals_between(vars(TimeInICU), left = 0, right = 1000) %>%
                                              col_vals_between(vars(DischargeDate), left = as_date("2005-01-01"), right = as_date("2022-12-31")) %>%
                                              col_vals_in_set(vars(DischargeReasonCode), filter(df_Meta_ValueSets, Table == "Cases" & Feature == "DischargeReasonCode")$Value) %>%
                                          interrogate()

# Create Validation Report as tibble
ValidationReport_df_IDM_Cases_Harmonized <- Validation_df_IDM_Cases_Harmonized %>%
                                                get_agent_report(display_table = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_IDM_CasesDepartment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_IDM_CasesDepartment_Harmonized <- df_IDM_CasesDepartment %>%
                                                    create_agent() %>%
                                                    col_exists(vars(CasePseudonym,
                                                                    DepartmentCode,
                                                                    DepartmentAdmissionDate,
                                                                    DepartmentDischargeDate,
                                                                    DepartmentAdmissionToICU)) %>%
                                                    col_vals_not_null(vars(CasePseudonym,
                                                                           DepartmentCode,
                                                                           DepartmentAdmissionDate,
                                                                           DepartmentDischargeDate,
                                                                           DepartmentAdmissionToICU)) %>%
                                                    col_vals_in_set(vars(DepartmentCode), set = filter(df_Meta_ValueSets, Table == "CasesDepartment" & Feature == "DepartmentCode")$Value) %>%
                                                    col_vals_between(vars(DepartmentAdmissionDate), left = as_date("2005-01-01"), right = as_date("2022-12-31")) %>%
                                                    col_vals_between(vars(DepartmentDischargeDate), left = as_date("2005-01-01"), right = as_date("2022-12-31")) %>%
                                                    col_vals_in_set(vars(DepartmentAdmissionToICU), set = c(TRUE, FALSE)) %>%
                                                    interrogate()

# Create Validation Report as tibble
ValidationReport_df_IDM_CasesDepartment_Harmonized <- Validation_df_IDM_CasesDepartment_Harmonized %>%
                                                          get_agent_report(display_table = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_IDM_CasesICD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_IDM_CasesICD_Harmonized <- df_IDM_CasesICD %>%
                                              create_agent() %>%
                                              col_exists(vars(CasePseudonym,
                                                              DiagnosisType,
                                                              ICDVersion,
                                                              ICDCode,
                                                              SecondaryICDCode)) %>%
                                              col_vals_not_null(vars(CasePseudonym,
                                                                     DiagnosisType,
                                                                     ICDVersion,
                                                                     ICDCode,
                                                                     SecondaryICDCode)) %>%
                                              col_vals_in_set(vars(DiagnosisType), set = c("HD", "ND")) %>%
                                              col_vals_between(vars(ICDVersion), left = 1990, right = 2023) %>%
                                              interrogate()

# Create Validation Report as tibble
ValidationReport_df_IDM_CasesICD_Harmonized <- Validation_df_IDM_CasesICD_Harmonized %>%
                                                    get_agent_report(display_table = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of df_IDM_CasesOPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_df_IDM_CasesOPS_Harmonized <- df_IDM_CasesOPS %>%
                                              create_agent() %>%
                                              col_exists(vars(CasePseudonym,
                                                              OPSVersion,
                                                              OPSCode,
                                                              OPSDate)) %>%
                                              col_vals_not_null(vars(CasePseudonym,
                                                                     OPSVersion,
                                                                     OPSCode,
                                                                     OPSDate)) %>%
                                              col_vals_between(vars(OPSVersion), left = 1990, right = 2023) %>%
                                              col_vals_between(vars(OPSDate), left = as_date("2005-01-01"), right = as_date("2022-12-31")) %>%
                                              interrogate()

# Create Validation Report as tibble
ValidationReport_df_IDM_CasesOPS_Harmonized <- Validation_df_IDM_CasesOPS_Harmonized %>%
                                                    get_agent_report(display_table = FALSE)

