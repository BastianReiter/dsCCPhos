

################################################################################
#------------------------------------------------------------------------------#
#   INPUT DATA Freiburg: LOADING and Pre-HARMONIZATION                         #
#------------------------------------------------------------------------------#
################################################################################


# Source:
# ~~~~~~~
#     Datenintegrationszentrum Universitätsklinikum Freiburg

# Export Method:
# ~~~~~~~~~~~~~~
#     SQL-Query --> CSV-Export

# Exported files:
# ~~~~~~~~~~~~~~~
#     - Fall.csv
#     - FAB.csv
#     - ICD.csv
#     - OPS.csv

InputDataPath <- "Data/SiteInputData/Freiburg"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes on Freiburg Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   - Data only available from year 2009
#   - ICDVersion data is different, has to be recoded

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Data Model (IDM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_Cases
# df_IDM_CasesDepartment
# df_IDM_CasesICD
# df_IDM_CasesOPS

# ==> For details, see Project Documentation -> Harmonized Input Data Model


# Setting Locale Object to pass to Reading Functions
InputDataLocale <- locale(decimal_mark = ",")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_Cases: Patient- and Case-related data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_Cases <- read_csv(here(InputDataPath, "Fall.csv"),
                         locale = InputDataLocale,
                         skip = 1,                                                 # No Header Row
                         col_names = c("CasePseudonym",                            # Column Names 
                                       "YearOfBirth",
                                       "Sex",
                                       "PostalCode",
                                       "AdmissionDate",
                                       "AdmissionCauseCode",
                                       "DischargeDate",
                                       "DischargeReasonCode",
                                       "AdmissionAge",
                                       "PatientPseudonym",
                                       "TimeInICU"),
                         col_types = list(CasePseudonym = "c",                     # Column types
                                          YearOfBirth = "i",
                                          Sex = "c",
                                          PostalCode = "c",
                                          AdmissionDate = col_datetime(format = "%Y%m%d%H%M"),
                                          AdmissionCauseCode = "c",
                                          DischargeDate = col_datetime(format = "%Y%m%d%H%M"),
                                          DischargeReasonCode = "c",
                                          AdmissionAge = "i",
                                          PatientPseudonym = "c",
                                          TimeInICU = "d"))

# Small Pre-Harmonization Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_Cases <- df_IDM_Cases %>%
                    mutate(AdmissionDate = as_date(format(AdmissionDate, format = "%Y-%m-%d")),
                           DischargeDate = as_date(format(DischargeDate, format = "%Y-%m-%d")),
                           DischargeReasonCode = str_sub(DischargeReasonCode, start = 1, end = 2))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_Department: Data on Admission specific Hospital Departments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesDepartment <- read_csv(here(InputDataPath, "FAB.csv"),
                                   locale = InputDataLocale,
                                   skip = 1,
                                   col_names = c("CasePseudonym",
                                                 "DepartmentCode",
                                                 "DepartmentAdmissionDate",
                                                 "DepartmentDischargeDate",
                                                 "DepartmentAdmissionToICU"),
                                   col_types = list(CasePseudonym = "c",
                                                    DepartmentCode = "c",
                                                    DepartmentAdmissionDate = col_datetime(format = "%Y%m%d%H%M"),
                                                    DepartmentDischargeDate = col_datetime(format = "%Y%m%d%H%M"),
                                                    DepartmentAdmissionToICU = "c"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_CasesICD: Case-related ICD Codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesICD <- read_csv(here(InputDataPath, "ICD.csv"),
                            locale = InputDataLocale,
                            skip = 1,
                            col_names = c("CasePseudonym",
                                          "DiagnosisType",
                                          "ICDVersion",
                                          "ICDCode",
                                          "SecondaryICDCode"),
                            col_types = list(CasePseudonym = "c",
                                             DiagnosisType = "c",
                                             ICDVersion = "c",
                                             ICDCode = "c",
                                             SecondaryICDCode = "c"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_CasesOPS: Case-related OPS Codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesOPS <-  read_csv(here(InputDataPath, "OPS.csv"),
                             locale = InputDataLocale,
                             skip = 1,
                             col_names = c("CasePseudonym",
                                           "OPSVersion",
                                           "OPSCode",
                                           "OPSDate"),
                             col_types = list(CasePseudonym = "c",
                                              OPSVersion = "c",
                                              OPSCode = "c",
                                              OPSDate = col_datetime(format = "%Y%m%d%H%M")))

# Small Pre-Harmonization Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesOPS <- df_IDM_CasesOPS %>%
                        mutate(OPSCode = str_replace_all(OPSCode, "[^[:alnum:]]", ""))      # Remove all special characters in OPS Codes

