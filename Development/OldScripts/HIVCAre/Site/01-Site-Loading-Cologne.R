

################################################################################
#------------------------------------------------------------------------------#
#   INPUT DATA Cologne: LOADING and HARMONIZATION                              #
#------------------------------------------------------------------------------#
################################################################################


# Source:
# ~~~~~~~
#     Datenintegrationszentrum Uniklinik Köln

# Export Method:
# ~~~~~~~~~~~~~~
#     SQL-Query --> CSV-Export

# Exported files:
# ~~~~~~~~~~~~~~~
#     - data_CaseData.csv
#     - data_CasesToICDCodes.csv
#     - data_CasesToOpsCodes.csv


InputDataPath <- "./Data/SiteInputData/Cologne/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Data Model (IDM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_Cases
# df_IDM_CasesICD
# df_IDM_CasesOPS

# ==> For details, see Project Documentation -> Harmonized Input Data Model


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes on Cologne Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Missing (Relevant): - AdmissionAge
#                     

# Extra (Relevant): - CaseType
#                   - DiagnosisDate
#

# Extra (Irrelevant): - RecordDate
#


# Admission Age is missing --> Estimation using YearOfBirth and AdmissionDate



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_IDM_Cases: Patient- and Case-related data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_Cases <- read_csv(paste0(InputDataPath, "data_CaseData.csv"),
                      skip = 1,                                                 # Skip Header
                      col_names = c("PatientPseudonym",                         # Column Names 
                                    "CasePseudonym",                            
                                    "PostalCode",
                                    "Sex",
                                    "YearOfBirth",
                                    "AdmissionDate",
                                    "DischargeDate",
                                    "DischargeReason",
                                    "CaseType"),
                      col_types = list(PatientPseudonym = "c",           # Skip PatientPseudonym
                                       CasePseudonym = "c",                     
                                       PostalCode = "c",
                                       Sex = "f",
                                       YearOfBirth = "i",
                                       AdmissionDate = col_datetime(format = "%Y%m%d"),
                                       DischargeDate = col_datetime(format = "%Y%m%d"),
                                       DischargeReason = "f",
                                       CaseType = "c"))

# Harmonization operations
# ------------------------
# - Filter out non-inpatient cases (unfortunately)
# - If redundant cases with same DischargeDate, keep only first one (arrange & distinct)
# - Add column of missing AdmissionAge
# - Estimate Admission Age based on YearOfBirth and AdmissionDate
df_IDM_Cases <- df_IDM_Cases %>%
                filter(CaseType == "Inpatient") %>%
                arrange(PatientPseudonym, CasePseudonym, DischargeDate, AdmissionDate) %>%
                distinct(CasePseudonym, .keep_all = TRUE) %>%
                mutate(AdmissionAge = case_when(month(AdmissionDate) <= 6 ~ year(AdmissionDate) - YearOfBirth - 1,      # If latest month of Admission Date is June, presume that patient did not have birthday yet
                                                month(AdmissionDate) > 6 ~ year(AdmissionDate) - YearOfBirth)) %>%      # If month of Admission Date is after June, presume that patient already passed birthday
                select(-CaseType)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case-related ICD Codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesICD <- read_csv(paste0(InputDataPath, "data_CasesToICDCodes.csv"),
                         skip = 1,
                         col_names = c("CasePseudonym",
                                       "DiagnosisType", 
                                       "ICDVersion",
                                       "ICDCode",
                                       "SecondaryICDCode",
                                       "DiagnosisDate",
                                       "RecordDate"),
                         col_types = list(CasePseudonym = "c",
                                          DiagnosisType = "f",
                                          ICDVersion = "c",
                                          ICDCode = "c",
                                          SecondaryICDCode = "c",
                                          DiagnosisDate = col_skip(),
                                          RecordDate = col_skip()))

# Harmonization operations
# ------------------------
# - Modify ICDVersion value format and data type
# - If ICDVersion value is missing, assign value "2022"
df_IDM_CasesICD <- df_IDM_CasesICD %>%
                    mutate(ICDVersion = as.integer(str_remove(ICDVersion, "ICD10-"))) %>%
                    mutate(ICDVersion = replace_na(ICDVersion, 2022))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case-related OPS Codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_IDM_CasesOPS <-  read_csv(paste0(InputDataPath, "data_CasesToOPSCodes.csv"),
                          skip = 1,
                          col_names = c("CasePseudonym",
                                        "OPSVersion",
                                        "OPSCode",
                                        "OPSDate"),
                          col_types = list(CasePseudonym = "c",
                                           OPSVersion = "f",
                                           OPSCode = "c",
                                           OPSDate = col_datetime(format = "%Y%m%d")))

# Harmonization operations
# ------------------------
# - Modify OPSVersion value format and data type
# - If OPSVersion value is missing, assign value "2022"
# - Remove all special symbols in OPSCode values
df_IDM_CasesOPS <- df_IDM_CasesOPS %>%
                    mutate(OPSVersion = as.integer(str_remove(OPSVersion, "OPS-")),
                           OPSCode = str_replace_all(OPSCode, "[^[:alnum:]]", "")) %>%
                    mutate(OPSVersion = case_when(is.na(OPSVersion) ~ 2022))
                    




