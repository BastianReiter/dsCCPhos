

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: LOAD INPUT DATA  -  Munich (LMU)                                  #
#------------------------------------------------------------------------------#
################################################################################


# Source: Zentrum für Medizinische Datenintegration und -analyse (MeDICLMU)
# Method: SQL-Query --> CSV-Export


InputDataPath <- "./Data/SiteInputData/MunichLMU"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes on Munich (LMU) Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variables containing Missing Data - Alter-in-Jahren-am-Aufnahmetag
#                                   - Verweildauer-Intensiv
#                                   - KennungIntensivbett


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


#===============================================================================
# Initiating IDM objects with data from first year
#===============================================================================

df_IDM_Cases <- read_csv2(here(InputDataPath, "2114AFALL.CSV"),
                          locale = InputDataLocale) %>%
                    select("Patientennummer",
                           "KH-internes-Kennzeichen",
                           "Geburtsjahr",
                           "Geschlecht",
                           "PLZ",
                           "Aufnahmedatum",
                           "Aufnahmeanlass",
                           "Entlassungsdatum",
                           "Entlassungsgrund",
                           "Alter-in-Jahren-am-Aufnahmetag")


df_IDM_CasesDepartment <- read_csv2(here(InputDataPath, "2014FAB.CSV"),
                                    locale = InputDataLocale) %>%
                              select("KH-internes-Kennzeichen",
                                     "Fachabteilung",
                                     "FAB-Aufnahmedatum",
                                     "FAB-Entlassungsdatum")


df_IDM_CasesICD <- read_csv2(here(InputDataPath, "2114AICD.CSV"),
                            locale = InputDataLocale) %>%
                        select("KH-internes-Kennzeichen",
                               "Diagnoseart",
                               "ICD-Version",
                               "ICD-Kode",
                               "Sekundaer-Kode")


df_IDM_CasesOPS <- read_csv2(here(InputDataPath, "2114AOPS.CSV"),
                     locale = InputDataLocale) %>%
                select("KH-internes-Kennzeichen",
                       "OPS-Version",
                       "OPS-Kode",
                       "OPS-Datum")



#===============================================================================
# Assembling complete IDM objects by attaching data from all following years
#===============================================================================

for(year in 2015:2021)
{
    # Assembling df_IDM_Cases
    # --------------------------------------------------------------------------
    df_CasesCurrent <- read_csv2(here(InputDataPath, paste0(year, "FALL.CSV")),
                                 locale = InputDataLocale) %>%
                            select(colnames(df_IDM_Cases))
                        
    df_IDM_Cases <- bind_rows(df_IDM_Cases, df_CasesCurrent)
    
    # Assembling df_IDM_CasesDepartment
    # --------------------------------------------------------------------------
    df_CasesDepartmentCurrent <- read_csv2(here(InputDataPath, paste0(year, "FAB.CSV")),
                                           locale = InputDataLocale) %>%
                                      select(colnames(df_IDM_CasesDepartment))
    
    df_IDM_CasesDepartment <- bind_rows(df_IDM_CasesDepartment, df_CasesDepartmentCurrent)
    
    # Assembling df_IDM_CasesICD
    # --------------------------------------------------------------------------
    df_CasesICDCurrent <- read_csv2(here(InputDataPath, paste0(year, "ICD.CSV")),
                                    locale = InputDataLocale) %>%
                              select(colnames(df_IDM_CasesICD))
    
    df_IDM_CasesICD <- bind_rows(df_IDM_CasesICD, df_CasesICDCurrent)
    
    # Assembling df_IDM_CasesOPS
    # --------------------------------------------------------------------------
    df_CasesOPSCurrent <- read_csv2(here(InputDataPath, paste0(year, "OPS.CSV")),
                                    locale = InputDataLocale) %>%
                              select(colnames(df_IDM_CasesOPS))
    
    df_IDM_CasesOPS <- bind_rows(df_IDM_CasesOPS, df_CasesOPSCurrent)
}



#===============================================================================
# Assigning new column names
#===============================================================================

colnames(df_IDM_Cases) <- c("PatientPseudonym",
                            "CasePseudonym",
                            "YearOfBirth",
                            "Sex",
                            "PostalCode",
                            "AdmissionDate",
                            "AdmissionCauseCode",
                            "DischargeDate",
                            "DischargeReasonCode",
                            "AdmissionAge")


colnames(df_IDM_CasesDepartment) <- c("CasePseudonym",
                                      "DepartmentCode",
                                      "DepartmentAdmissionDate",
                                      "DepartmentDischargeDate")


colnames(df_IDM_CasesICD) <- c("CasePseudonym",
                               "DiagnosisType",
                               "ICDVersion",
                               "ICDCode",
                               "SecondaryICDCode")


colnames(df_IDM_CasesOPS) <- c("CasePseudonym",
                               "OPSVersion",
                               "OPSCode",
                               "OPSDate")



#===============================================================================
# Assigning column data types and formats
#===============================================================================


df_IDM_Cases <- df_IDM_Cases %>%
                    mutate(YearOfBirth = as.integer(YearOfBirth),
                           AdmissionDate = parse_date_time(AdmissionDate, orders="%Y%m%d%h%M"),
                           AdmissionDate = as_date(format(AdmissionDate, format = "%Y-%m-%d")),
                           DischargeDate = parse_date_time(DischargeDate, orders="%Y%m%d%h%M"),
                           DischargeDate = as_date(format(DischargeDate, format = "%Y-%m-%d")),
                           DischargeReasonCode = str_sub(DischargeReasonCode, start = 1, end = 2),
                           AdmissionAge = ifelse(is.na(AdmissionAge),
                                                 as.integer(year(AdmissionDate) + 1 - YearOfBirth),      # If missing, AdmissionAge is roughly (over-)estimated 
                                                 as.integer(AdmissionAge)),
                           TimeInICU = NA_integer_)


df_IDM_CasesDepartment <- df_IDM_CasesDepartment %>%
                              mutate(DepartmentAdmissionDate = parse_date_time(DepartmentAdmissionDate, orders="%Y%m%d%h%M"),
                                     DepartmentDischargeDate = parse_date_time(DepartmentDischargeDate, orders="%Y%m%d%h%M"),
                                     DepartmentAdmissionToICU = NA_character_)


df_IDM_CasesOPS <- df_IDM_CasesOPS %>%
                        mutate(OPSCode = str_replace_all(OPSCode, "[^[:alnum:]]", ""),      # Remove all special characters in OPS Codes
                               OPSDate = parse_date_time(OPSDate, orders="%Y%m%d%h%M"))
                           




                    




