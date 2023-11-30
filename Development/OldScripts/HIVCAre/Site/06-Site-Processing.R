

#==============================================================================#
#------------------------------------------------------------------------------#
#   HIVCAre: PROCESS / WRANGLE DATA (SITE)                                     #
#------------------------------------------------------------------------------#
#==============================================================================#


#   Required Objects:
#   ~~~~~~~~~~~~~~~~~
#   df_IDM_Cases
#   df_IDM_CasesDepartment
#   df_IDM_CasesICD
#   df_IDM_CasesOPS


#===============================================================================
# Analysis Data Model (ADM)
#===============================================================================

#       __________________         ___________________
#      / df_ADM_Diagnoses \       / df_ADM_Procedures \
#      \__________________/       \___________________/
#     
#                      _______________
#                     / df_ADM_Events \
#                     \_______________/
#     
#                      _________________
#                     / df_ADM_CaseInfo \
#                     \_________________/
#     
#                     _________________
#                    / df_ADM_Patients \
#                    \_________________/



#===============================================================================
# Initialize Progress Bar
#===============================================================================

try(ls_Progress_Processing <- f_InitProgressBar(inp_Title = "Processing Analysis Data Model",
                                                inp_ProgressTotalSteps = 30),      # Needs to be adjusted manually (How many times is f_UpdateProgressBar being called?)
    silent = TRUE)


#===============================================================================
# Initiate Data Frame containing Case data and corresponding (Transfer-related) Events
#===============================================================================
# Add case-specific variables:
#     - CaseNumber
#     - AdmissionYear: Year of admission for subgrouping in reports
#     - LengthOfStay: Length of stay per case in days
#-------------------------------------------------------------------------------
# Join df_IDM_Cases with df_IDM_CasesDepartment
#-------------------------------------------------------------------------------
# Look up labels of coded variables via join()
#     - AdmissionCause
#     - DischargeReason
#     - Department
#-------------------------------------------------------------------------------

df_ADM_Events <- df_IDM_Cases %>%
                      group_by(PatientPseudonym) %>%
                          mutate(CaseNumber = row_number(),
                                 AdmissionYear = year(AdmissionDate),
                                 LengthOfStay = ceiling(as.numeric(difftime(DischargeDate, AdmissionDate, units = "days") + 1))) %>%
                      ungroup() %>% 
                      full_join(df_IDM_CasesDepartment, by = join_by(CasePseudonym)) %>%
                      filter(is.na(PatientPseudonym) == FALSE) %>%
                      left_join(df_Meta_AdmissionCauses, by = join_by(AdmissionCauseCode)) %>%
                      left_join(df_Meta_DischargeReasons, by = join_by(DischargeReasonCode)) %>%
                      left_join(df_Meta_Departments, by = join_by(DepartmentCode)) %>%
                      arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                      select(PatientPseudonym,
                             CasePseudonym,
                             CaseNumber,
                             YearOfBirth,
                             Sex,
                             PostalCode,
                             AdmissionDate,
                             AdmissionYear,
                             AdmissionAge,
                             AdmissionCauseCode,
                             AdmissionCause,
                             DepartmentCode,
                             Department,
                             OperatingSpecialty,
                             DepartmentAdmissionToICU,
                             DepartmentAdmissionDate,
                             DepartmentDischargeDate,
                             TimeInICU,
                             DischargeDate,
                             DischargeCategory,
                             LengthOfStay,
                             #--- Deselected Features --- # Listed for clarity
                             -DischargeReasonCode,
                             -DischargeReasonOriginalLabel,
                             -DepartmentOriginalLabel,
                             -Subspecialty)
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing, inp_StepDescription = "Initiating df_ADM_Events"), silent = TRUE)


df_ADM_Events <- df_ADM_Events %>%
                      arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                      mutate(DepartmentIsPseudo = (Department == "Pseudo")) %>%
                      group_by(PatientPseudonym, CasePseudonym) %>%
                          add_tally(name = "CountTransfersWithinCase") %>%
                          add_tally(DepartmentAdmissionToICU,
                                    name = "CountTransfersICU") %>%
                          add_tally(DepartmentIsPseudo,
                                    name = "CountTransfersPseudo")
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)


df_ADM_Events <- df_ADM_Events %>%
                      ungroup() %>%
                      arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                      #--- If Department is "Pseudo" and Transfer Time is neighboring Admission to non-Pseudo-Department, turn it into "real" Department
                      mutate(DepartmentCode = if_else(Department == "Pseudo" & (DepartmentAdmissionDate == tidytable::lag(DepartmentDischargeDate)),
                                                      tidytable::lag(DepartmentCode),
                                                      DepartmentCode)) %>%
                      group_by(across(c(-Department,
                                        -OperatingSpecialty,
                                        -DepartmentIsPseudo,
                                        -DepartmentAdmissionDate,
                                        -DepartmentDischargeDate))) %>%
                          mutate(HasStayedWithoutPause = (DepartmentAdmissionDate == tidytable::lag(DepartmentDischargeDate, default = as_date(0))),      # Check: DepartmentAdmissionDate == Previous DepartmentDischargeDate ?
                                 WillStayWithoutPause = (DepartmentDischargeDate == tidytable::lead(DepartmentAdmissionDate, default = as_date(0))))      # Check: DepartmentDischargeDate == Next DepartmentAdmissionDate ?
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing, inp_StepDescription = "Processing df_ADM_Events: Discrimination of stays"), silent = TRUE)

                          
df_ADM_Events <- df_ADM_Events %>%
                      filter(HasStayedWithoutPause == FALSE | WillStayWithoutPause == FALSE) %>%
                      mutate(DepartmentDischargeDate = if_else(WillStayWithoutPause == TRUE,
                                                               tidytable::lead(DepartmentDischargeDate),
                                                               DepartmentDischargeDate),
                             DepartmentAdmissionDate = if_else(HasStayedWithoutPause == TRUE,
                                                               tidytable::lag(DepartmentAdmissionDate),
                                                               DepartmentAdmissionDate)) %>%
                      ungroup() %>%
                      select(-HasStayedWithoutPause,
                             -WillStayWithoutPause) %>%
                      distinct(PatientPseudonym,
                               CasePseudonym,
                               DepartmentCode,
                               DepartmentAdmissionToICU,
                               DepartmentAdmissionDate,
                               DepartmentDischargeDate,
                               .keep_all = TRUE)
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing, inp_StepDescription = "Processing df_ADM_Events: Sorting out real admission and discharge dates"), silent = TRUE)


df_ADM_Events <- df_ADM_Events %>%
                      arrange(PatientPseudonym, AdmissionDate, DepartmentAdmissionDate) %>%
                      group_by(PatientPseudonym, CasePseudonym) %>%
                          mutate(TransferNumber = row_number(),
                                 EventDate = DepartmentAdmissionDate,
                                 EventClass = "Administration",
                                 EventSubclass = case_when(TransferNumber == 1 ~ "Admission",
                                                           TransferNumber > 1 ~ "Transfer"),
                                 EventSpecification_A = case_when(TransferNumber == 1 ~ AdmissionCause,
                                                                TransferNumber > 1 ~ Department),
                                 EventSpecification_B = case_when(TransferNumber == 1 ~ AdmissionCauseCode,
                                                         TransferNumber > 1 ~ NA),
                                 EventSpecification_C = NA,
                                 EventLevelOfCare = case_when(DepartmentAdmissionToICU == TRUE ~ "ICU",
                                                              DepartmentAdmissionToICU == FALSE ~ "NonICU",
                                                              is.na(DepartmentAdmissionToICU) ~ "Unknown"),
                                 AdmittingDepartmentCode = first(DepartmentCode),
                                 AdmittingDepartment = first(Department),
                                 TransferLengthOfStay = round(as.numeric(difftime(DepartmentDischargeDate, DepartmentAdmissionDate, units = "days")), 2),
                                 CalculatedTimeInICU = sum(TransferLengthOfStay[DepartmentAdmissionToICU == TRUE]))
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# Initiate Data Frame of consolidated Case Data
#===============================================================================

df_ADM_CaseInfo <- df_ADM_Events %>%
                    group_by(PatientPseudonym, CasePseudonym) %>%
                        filter(TransferNumber == 1) %>%
                        select(PatientPseudonym,
                               CasePseudonym,
                               CaseNumber,
                               YearOfBirth,
                               Sex,
                               PostalCode,
                               AdmissionDate,
                               AdmissionYear,
                               AdmissionAge,
                               AdmissionCauseCode,
                               AdmissionCause,
                               AdmittingDepartmentCode,
                               AdmittingDepartment,
                               TimeInICU,
                               CalculatedTimeInICU,
                               CountTransfersWithinCase,
                               CountTransfersICU,
                               CountTransfersPseudo,
                               DischargeDate,
                               DischargeCategory,
                               LengthOfStay,
                               #--- Deselected Features ---   # Only listed for clarity
                               -TransferNumber,
                               -EventDate,
                               -EventClass,
                               -EventSubclass,
                               -EventSpecification_A,
                               -EventSpecification_B,
                               -EventSpecification_C,
                               -EventLevelOfCare,
                               -DepartmentCode,
                               -Department,
                               -OperatingSpecialty,
                               -DepartmentAdmissionDate,
                               -DepartmentDischargeDate,
                               -DepartmentAdmissionToICU,
                               -DepartmentIsPseudo,
                               -TransferLengthOfStay)
                    #=== Update Progress Bar ===
                    try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# Obtain Discharge Events and add them to df_ADM_Events
#===============================================================================

df_Aux_DischargeEvents <- df_ADM_Events %>%
                              group_by(PatientPseudonym, CasePseudonym) %>%
                                  filter(TransferNumber == max(TransferNumber)) %>%
                                  mutate(EventDate = DepartmentDischargeDate,
                                         EventClass = "Administration",
                                         EventSubclass = "Discharge",
                                         EventSpecification_A = DischargeCategory,
                                         EventSpecification_B = NA,
                                         EventSpecification_C = NA,
                                         EventLevelOfCare = case_when(DepartmentAdmissionToICU == TRUE ~ "ICU",
                                                                      DepartmentAdmissionToICU == FALSE ~ "NonICU",
                                                                      is.na(DepartmentAdmissionToICU) ~ "Unknown"))
                              #=== Update Progress Bar ===
                              try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)

                              
df_ADM_Events <- df_ADM_Events %>%
                      ungroup() %>%
                      add_row(df_Aux_DischargeEvents) %>%
                      arrange(PatientPseudonym, CasePseudonym, EventDate)
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# Reconfigure df_ADM_Events
#===============================================================================

df_ADM_Events <- df_ADM_Events %>%
                      select(PatientPseudonym,
                             YearOfBirth,
                             Sex,
                             CaseNumber,
                             CasePseudonym,
                             AdmissionAge,
                             DepartmentCode,
                             Department,
                             OperatingSpecialty,
                             EventDate,
                             EventClass,
                             EventSubclass,
                             EventSpecification_A,
                             EventSpecification_B,
                             EventSpecification_C,
                             EventLevelOfCare)



#===============================================================================
# df_ADM_Diagnoses: Diagnosis-oriented Data Frame
#===============================================================================
# Create data frame of Case-related ICD codes
# 1) Attach Case-related ICD Codes
# 2) Use join to identify HIV Status codes from meta data
# 3) Use join to identify HIV Disease codes from meta data
#-------------------------------------------------------------------------------

df_ADM_Diagnoses <- df_ADM_CaseInfo %>%
                        left_join(df_IDM_CasesICD, by = join_by(CasePseudonym)) %>%
                        left_join(df_Meta_ICD10HIVStatus, by = join_by(ICDCode == PrimaryICDCode,
                                                                       SecondaryICDCode == SecondaryICDCode)) %>%
                        left_join(df_Meta_ICD10HIVDiseases, by = join_by(ICDCode == PrimaryICDCode,
                                                                         SecondaryICDCode == SecondaryICDCode))
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_ADM_Diagnoses
#===============================================================================
# Add code-specific variables:
#     - ICDCodeShort: Short form of full ICD Code to get more general (cancer) diagnoses for easier subgrouping
#     - Is...Code: Identify ICD codes to later construct subgroups of interest
#     - IsCancerCode: True, if code is a cancer code, but not a metastasis code
#     - IsPotentialMainCancer: True, if the cancer code is a "main diagnosis" ("HD") and no code of anamnestic or follow-up character (Z08, Z85, Z92.6)
#     - IsADCode: True, if coded disease is considered AIDS defining. Not equivalent to AIDS code!
#     - IsChronicRenalFailure
#     - IsAIDSCode: True, if code is an AIDS code by itself or if it codes for an AIDS defining disease that occurred after an HIV code
#-------------------------------------------------------------------------------

df_ADM_Diagnoses <- df_ADM_Diagnoses %>%
                        mutate(ICDCodeShort = str_sub(ICDCode, end = 3),      # Get first 3 chars of full ICD code to get more general diagnostic entity
                               IsCancerCode = if_else(str_starts(ICDCode, paste(vc_MetastasisCodes, collapse = "|")),
                                                      FALSE,
                                                      str_starts(ICDCode, paste(vc_CancerCodes, collapse = "|"))),
                               IsPotentialMainCancer = (IsCancerCode == TRUE &
                                                          DiagnosisType == "HD" &
                                                          str_starts(ICDCode, "Z08|Z85|Z92.6") == FALSE),
                               PotentialMainCancerCode = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                ICDCodeShort[IsPotentialMainCancer == TRUE],
                                                                NA),
                               IsMetastasisCode = str_starts(ICDCode, paste(vc_MetastasisCodes, collapse = "|")),
                               IsHIVCode = str_starts(ICDCode, paste(vc_HIVCodes, collapse = "|")),
                               IsADCode = case_when(HIVAssociation == "AIDS defining" ~ TRUE,
                                                    TRUE ~ FALSE),
                               IsADCodeCancer = case_when((HIVAssociation == "AIDS defining" & HIVDiseaseClass == "Cancer") ~ TRUE,
                                                          TRUE ~ FALSE),
                               IsADCodeNonCancerous = case_when((HIVAssociation == "AIDS defining" & HIVDiseaseClass != "Cancer") ~ TRUE,
                                                                TRUE ~ FALSE),
                               IsHIVNonADCodeCancer = case_when((HIVAssociation == "HIV associated" & HIVDiseaseClass == "Cancer") ~ TRUE,
                                                                TRUE ~ FALSE),
                               IsChronicRenalFailure = str_starts(ICDCode, "N18")) %>%
                        arrange(PatientPseudonym, AdmissionDate, IsADCode) %>%      # HIV codes have to be listed before AIDS defining codes if occurring on the same date
                        group_by(PatientPseudonym) %>% 
                            mutate(IsAIDSCode = case_when(HIVStatusInterpretation == "AIDS" ~ TRUE,
                                                          (cumsum(IsHIVCode) > 0 & IsADCode == TRUE) ~ TRUE,      # Check if HIV code already occurred. If so and if the code stands for an AIDS defining disease, interpret as AIDS code.
                                                          TRUE ~ FALSE),      # In "case_when" logic "TRUE ~ " is equivalent to "else"-statement
                                   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   # HIV- and non-cancerous AD and AIDS codes should not be shortened in order to be distinguishable later on
                                   ICDCodeShort = case_when(IsHIVCode == TRUE ~ ICDCode,
                                                            IsADCode == TRUE & IsADCodeCancer == FALSE ~ ICDCode,
                                                            IsAIDSCode == TRUE & IsADCodeCancer == FALSE  ~ ICDCode,
                                                            TRUE ~ ICDCodeShort))      # Only evaluated if code is neither HIV nor AD nor AIDS code
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)


                            
#===============================================================================
# df_ADM_Diagnoses
#===============================================================================
# Obtain coded diagnosis labels in two forms:
#     - DiagnosisGeneral: More general diagnostic entity (except for AIDS codes)
#     - DiagnosisDetail: Detailed diagnostic entity
#-------------------------------------------------------------------------------
# Obtain additional Cancer Classification Information from Meta Data
#-------------------------------------------------------------------------------

df_ADM_Diagnoses <- df_ADM_Diagnoses %>%
                        left_join(df_Meta_ICD10Catalog, by = join_by(ICDCodeShort == ICDCode)) %>%
                        left_join(df_Meta_ICD10Catalog, by = join_by(ICDCode == ICDCode)) %>%
                        rename(DiagnosisGeneral = Diagnosis.x,
                               DiagnosisDetail = Diagnosis.y) %>%
                        mutate(DiagnosisGeneral = coalesce(DiagnosisGeneral, DiagnosisDetail)) %>%      # If value in DiagnosisGeneral is NA, take value from DiagnosisDetail
                        left_join(df_Meta_ICD10CancerGrouping, by = join_by(ICDCodeShort)) %>%
                        select(-CancerTopographyDetail_German,
                               -CancerTopographyGroup_German,
                               -CancerTopographySpecification_German,
                               -CancerSpecification_German) 
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)

                            

#===============================================================================
# df_ADM_Diagnoses
#===============================================================================
# Reorder columns
# Omit unneeded variables:
#     - ICD Version
#-------------------------------------------------------------------------------
# Sort by patient pseudonym and admission date
#-------------------------------------------------------------------------------

df_ADM_Diagnoses <- df_ADM_Diagnoses %>%
                        select(#--- Patient-specific data ------------------
                               PatientPseudonym,
                               YearOfBirth,
                               Sex,
                               #--- Case-specific data -------------------------
                               CasePseudonym,
                               CaseNumber,
                               PostalCode,
                               AdmissionDate,
                               AdmissionYear,
                               AdmissionAge,
                               AdmissionCauseCode,
                               AdmissionCause,
                               AdmittingDepartmentCode,
                               AdmittingDepartment,
                               TimeInICU,
                               CalculatedTimeInICU,
                               CountTransfersWithinCase,
                               CountTransfersICU,
                               CountTransfersPseudo,
                               DischargeDate,
                               DischargeCategory,
                               LengthOfStay,
                               #--- Code-specific data -------------------------
                               DiagnosisType,
                               ICDCodeShort,
                               DiagnosisGeneral,
                               ICDCode,
                               SecondaryICDCode,
                               DiagnosisDetail,
                               IsCancerCode,
                               IsPotentialMainCancer,
                               IsMetastasisCode,
                               IsHIVCode,
                               IsAIDSCode,
                               IsADCode,
                               IsADCodeCancer,
                               IsADCodeNonCancerous,
                               IsHIVNonADCodeCancer,
                               IsChronicRenalFailure,
                               HIVDiseaseGerman,
                               HIVDiseaseClass,
                               HIVInformationClass,
                               HIVInformationValue,
                               HIVCodingPlausibility,
                               HIVStatusInterpretation,
                               HIVStadiumClinical,
                               HIVStadiumCellcount,
                               HIVAssociation,
                               CancerTopographyGroup_ICD10,
                               CancerTopographyDetail_ICD10,
                               CancerTopographyGroup_ZFKD,
                               CancerTopographySpecification,
                               CancerSpecification,
                               CancerIsLikelyToMetastasize,
                               CancerIsCarcinomaInSitu,
                               CancerIsNeoplasmOfUncertainBehavior) %>%
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        arrange(PatientPseudonym, AdmissionDate, DiagnosisType)



#===============================================================================
# df_ADM_Diagnoses
#===============================================================================
# Define main cancer disease of patients
#     - IsPresumedMainCancer
# If the context of the code meets certain criteria, presume that it is the first diagnosis of this cancer
#     - IsPresumedMainCancerFirstDiagnosis: Only presumed if there is a potential main cancer code (as defined in df_ADM_CaseDiagnoses) and no other Cancer Code (especially Z85 / Z92.6 or "ND" diagnoses) occurred before or at the same time
# Add information about patient-specific first occurrences of disease codes
#     - IsFirstMetastasisCode: Only presumed if Cancer code occurred before or at the same time
#     - IsFirstHIVCode
#     - IsFirstAIDSCode: Only presumed if HIV code occurred before or at the same time
#-------------------------------------------------------------------------------

df_ADM_Diagnoses <- df_ADM_Diagnoses %>%
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%      # Sort all coded diagnoses of each patient by admission date
                        #--- Presumed Cancer Diagnosis -------------------
                        # Step 1: Identify first occurring potential main cancer
                        group_by(PatientPseudonym, IsPotentialMainCancer) %>%
                            mutate(IsPresumedMainCancer = (IsPotentialMainCancer == TRUE & row_number() == 1)) %>%
                        # Step 2: If there is no other Cancer Code that occurred before or at the same time (especially Z85 / Z92.6 or "ND" diagnoses), assume First Diagnosis of Cancer for code that is TRUE in IsPresumedMainCancer
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%
                            mutate(IsPresumedMainCancerFirstDiagnosis = IsPresumedMainCancer == TRUE &
                                                                          (any(AdmissionDate[IsCancerCode == TRUE & IsPotentialMainCancer == FALSE] <= AdmissionDate[IsPresumedMainCancer == TRUE]) == FALSE)) %>% 
                        #--- Presumed Metastasis Diagnosis ---------------
                        group_by(PatientPseudonym) %>%
                            arrange(AdmissionDate, IsMetastasisCode, .by_group = TRUE) %>%      # Primary Cancer codes have to be listed before Metastasis codes if occurring on the same date. Here FALSE values for IsMetastasisCode appear on top of sorted "list".
                            mutate(IsFirstMetastasisCode = (cumsum(IsPresumedMainCancer) > 0 & IsMetastasisCode == TRUE)) %>%      # Check if Primary Cancer code already occurred
                        group_by(IsMetastasisCode, .add = TRUE) %>%
                            mutate(IsFirstMetastasisCode = (IsFirstMetastasisCode == TRUE & row_number() == 1)) %>%
                        #--- First HIV Code ----------------------
                        group_by(PatientPseudonym, IsHIVCode) %>%
                            arrange(AdmissionDate, .by_group = TRUE) %>%
                            mutate(IsFirstHIVCode = (IsHIVCode == TRUE & row_number() == 1)) %>%
                        #--- First AIDS Code ---------------------
                        group_by(PatientPseudonym, IsAIDSCode) %>%
                            mutate(IsFirstAIDSCode = (IsAIDSCode == TRUE & row_number() == 1)) %>%
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        ungroup()
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# Obtain Patient-specific diagnostic events and add them to df_ADM_Events
#===============================================================================

df_Aux_DiagnosisEvents <- df_ADM_Diagnoses %>%
                              group_by(PatientPseudonym) %>%
                                  mutate(EventDate = AdmissionDate,
                                         #EventDate = AdmissionDate + days(LengthOfStay / 2),
                                         EventClass = "Diagnosis",
                                         EventSubclass = case_when(IsPresumedMainCancer == TRUE | IsFirstMetastasisCode == TRUE ~ "Cancer-related Diagnosis",
                                                                   IsFirstHIVCode == TRUE | IsFirstAIDSCode == TRUE ~ "HIV-related Diagnosis",
                                                                   IsChronicRenalFailure == TRUE ~ "Special Comorbidity Diagnosis",
                                                                   TRUE ~ NA),
                                         EventSpecification_A = case_when(IsPresumedMainCancer == TRUE ~ "Main Cancer Diagnosis",
                                                                          IsFirstMetastasisCode == TRUE ~ "Metastasis Diagnosis",
                                                                          IsFirstHIVCode == TRUE ~ "HIV Diagnosis",
                                                                          IsFirstAIDSCode == TRUE ~ "AIDS Diagnosis",
                                                                          IsChronicRenalFailure == TRUE ~ "Chronic Renal Failure",
                                                                          TRUE ~ NA),
                                         EventSpecification_B = paste0(ICDCodeShort, ": ", DiagnosisGeneral),
                                         EventSpecification_C = ifelse(IsPresumedMainCancerFirstDiagnosis == TRUE,
                                                                       "Likely First Diagnosis",
                                                                       NA),
                                         EventLevelOfCare = NA) %>%
                                  filter(is.na(EventSpecification_A) == FALSE) %>%
                                  select(PatientPseudonym,
                                         CasePseudonym,
                                         EventDate,
                                         EventClass,
                                         EventSubclass,
                                         EventSpecification_A,
                                         EventSpecification_B,
                                         EventSpecification_C,
                                         EventLevelOfCare) %>%
                              ungroup()
                              #=== Update Progress Bar ===
                              try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)


df_ADM_Events <- df_ADM_Events %>%
                      ungroup() %>%
                      add_row(df_Aux_DiagnosisEvents) %>%
                      group_by(PatientPseudonym, CasePseudonym) %>%
                          fill(c(YearOfBirth,
                                 Sex,
                                 AdmissionAge,
                                 DepartmentCode,
                                 Department,
                                 OperatingSpecialty)) %>%
                          mutate(EventDate = as_datetime(ifelse(EventClass == "Diagnosis",
                                                                EventDate[EventSubclass == "Admission"],
                                                                EventDate))) %>%
                          arrange(PatientPseudonym, CasePseudonym, EventDate)
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_ADM_CaseInfo
#===============================================================================
# Obtain Presumed Main Cancer Diagnosis and all Potential Main Cancer Diagnoses for every case and add that information to df_ADM_CaseInfo
#-------------------------------------------------------------------------------

df_Aux_CaseSummaries_Diagnoses <- df_ADM_Diagnoses %>%
                                      group_by(PatientPseudonym, CasePseudonym) %>%
                                          summarize(CaseHoldsCancerCodes = any(IsCancerCode == TRUE),
                                                    CaseHoldsMetastasisCodes = any(IsFirstMetastasisCode == TRUE),
                                                    CaseHoldsHIVCodes = any(IsHIVCode == TRUE),
                                                    CaseHoldsAIDSCodes = any(IsFirstAIDSCode == TRUE),
                                                    #---------------------------
                                                    AllPotentialMainCancers_Case = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                                          paste(unique(ICDCodeShort[IsPotentialMainCancer == TRUE]), collapse = ", "),
                                                                                          NA),
                                                    CancerousComorbidities_Case = ifelse(any(IsCancerCode == TRUE),
                                                                                         paste(unique(ICDCodeShort[IsCancerCode == TRUE & DiagnosisType == "ND"]), collapse = ", "),
                                                                                         NA),
                                                    AllDistinctCancerCodes_Case = ifelse(any(IsCancerCode == TRUE),
                                                                                         paste(unique(ICDCodeShort[IsCancerCode == TRUE]), collapse = ", "),
                                                                                         NA),
                                                    SpecialComorbidities_Case = ifelse(any(IsChronicRenalFailure == TRUE),
                                                                                       ICDCodeShort[IsChronicRenalFailure == TRUE],
                                                                                       NA),
                                                    #---------------------------
                                                    CaseHoldsMainCancerDiagnosis = any(IsPresumedMainCancer == TRUE),
                                                    CaseHoldsMainCancerFirstDiagnosis = any(IsPresumedMainCancerFirstDiagnosis == TRUE),
                                                    #---------------------------
                                                    HIVStatus_Case = NA) %>%
                                      ungroup()
                                      #=== Update Progress Bar ===
                                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#   - DistinctCodeCount: Count of all distinct codes
#   - DistinctCodeCountCancer: Count of all "real" cancer codes (no anamnestic codes)
#   - DistinctCodeCountMainCancer: Count of all distinct MAIN cancer codes
#   - DistinctCodeCountHIV: Count of all distinct HIV codes
#   - DistinctCodeCountAIDS: Count of all distinct AIDS codes

df_Aux_PatientSummaries_Diagnoses <- df_ADM_Diagnoses %>%
                                          group_by(PatientPseudonym) %>%
                                              summarize(DistinctCodeCount = n_distinct(ICDCodeShort),
                                                        DistinctCodeCountCancer = n_distinct(ICDCodeShort[IsCancerCode == TRUE & str_starts(ICDCode, "Z08|Z85|Z92.6") == FALSE]),      # Only count "real" cancer codes
                                                        DistinctCodeCountMainCancer = n_distinct(ICDCodeShort[IsPotentialMainCancer == TRUE]),
                                                        DistinctCodeCountHIV = n_distinct(ICDCodeShort[IsHIVCode == TRUE]),
                                                        DistinctCodeCountAIDS = n_distinct(ICDCodeShort[IsAIDSCode == TRUE]),
                                                        #-----------------------
                                                        PatientHoldsCancerCodes = any(IsCancerCode == TRUE),
                                                        PatientHoldsMainCancerDiagnosis = any(IsPresumedMainCancer == TRUE),
                                                        PatientHoldsMainCancerFirstDiagnosis = any(IsPresumedMainCancerFirstDiagnosis == TRUE),
                                                        PatientHoldsMetastasisCodes = any(IsFirstMetastasisCode == TRUE),
                                                        PatientHoldsHIVCodes = any(IsHIVCode == TRUE),
                                                        PatientHoldsAIDSCodes = any(IsFirstAIDSCode == TRUE),
                                                        #-----------------------
                                                        MainCancerCode = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                ICDCode[IsPresumedMainCancer == TRUE],
                                                                                NA),
                                                        MainCancerDiagnosis = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                     DiagnosisDetail[IsPresumedMainCancer == TRUE],
                                                                                     NA),
                                                        MainCancerCodeShort = case_when(str_starts(MainCancerCode, "C") == TRUE ~ str_sub(MainCancerCode, end = 3),
                                                                                        str_starts(MainCancerCode, "D") == TRUE ~ MainCancerCode,
                                                                                        TRUE ~ NA),
                                                        MainCancerTopographyGroup = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                           CancerTopographyGroup_ICD10[IsPresumedMainCancer == TRUE],
                                                                                           NA),
                                                        MainCancerTopographyDetail = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                            CancerTopographyDetail_ICD10[IsPresumedMainCancer == TRUE],
                                                                                            NA),
                                                        MainCancerIsLikelyToMetastasize = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                                 CancerIsLikelyToMetastasize[IsPresumedMainCancer == TRUE],
                                                                                                 NA),
                                                        MainCancerIsCarcinomaInSitu = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                             CancerIsCarcinomaInSitu[IsPresumedMainCancer == TRUE],
                                                                                             NA),
                                                        MainCancerIsNeoplasmOfUncertainBehavior = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                                         CancerIsNeoplasmOfUncertainBehavior[IsPresumedMainCancer == TRUE],
                                                                                                         NA),
                                                        MainCancerIsAD = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                IsADCodeCancer[IsPresumedMainCancer == TRUE],
                                                                                NA),
                                                        MainCancerIsHIVNonAD = ifelse(any(IsPresumedMainCancer == TRUE),
                                                                                      IsHIVNonADCodeCancer[IsPresumedMainCancer == TRUE],
                                                                                      NA),
                                                        #-----------------------
                                                        Patient_AllPotentialMainCancers = ifelse(any(IsPotentialMainCancer == TRUE),
                                                                                                 paste(unique(ICDCodeShort[IsPotentialMainCancer == TRUE]), collapse = ", "),
                                                                                                 NA),
                                                        Patient_CancerousComorbidities = ifelse(any(IsCancerCode == TRUE),
                                                                                                paste(unique(ICDCodeShort[IsCancerCode == TRUE & DiagnosisType == "ND"]), collapse = ", "),
                                                                                                NA),
                                                        Patient_AllDistinctCancerCodes = ifelse(any(IsCancerCode == TRUE),
                                                                                                paste(unique(ICDCodeShort[IsCancerCode == TRUE]), collapse = ", "),
                                                                                                NA),
                                                        Patient_SpecialComorbidities = ifelse(any(IsChronicRenalFailure == TRUE),
                                                                                              ICDCodeShort[IsChronicRenalFailure == TRUE],
                                                                                              NA)) %>%
                                          ungroup()
                                          #=== Update Progress Bar ===
                                          try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)

                                          

# Add Information about Potential Main Cancer Diagnoses to Case Data
df_ADM_CaseInfo <- df_ADM_CaseInfo %>%
                        left_join(df_Aux_PatientSummaries_Diagnoses, by = join_by(PatientPseudonym)) %>%
                        left_join(df_Aux_CaseSummaries_Diagnoses, by = join_by(PatientPseudonym, CasePseudonym))
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# Calculation of Comorbidity Scores (Charlson and Elixhauser) for every Case
#===============================================================================
# Using weights put forth by Quan et al. for Charlson and Van Walraven et al. for Elixhauser
#-------------------------------------------------------------------------------

df_Aux_ComorbiditiesCharlson <- df_ADM_Diagnoses %>%
                                    group_by(CasePseudonym) %>%
                                        distinct(ICDCode) %>%
                                        comorbidity(id = "CasePseudonym",
                                                    code = "ICDCode",
                                                    map = "charlson_icd10_quan",
                                                    assign0 = TRUE) %>%
                                        select(-canc,
                                               -metacanc,
                                               -aids)

df_Aux_ComorbiditiesElixhauser <- df_ADM_Diagnoses %>%
                                      group_by(CasePseudonym) %>%
                                          distinct(ICDCode) %>%
                                          comorbidity(id = "CasePseudonym",
                                                      code = "ICDCode",
                                                      map = "elixhauser_icd10_quan",
                                                      assign0 = TRUE)

# Calculation of score using Van Walraven weights for Elixhauser comorbidity groups
# AIDS/HIV has weight 0, so we don't have to exclude this comorbidity prior to avoid induced bias in later matching
# Cancerous comorbidity are included for now
df_Aux_ScoreElixhauser <- df_Aux_ComorbiditiesElixhauser %>%
                              select(CasePseudonym) %>%
                              add_column(ComorbidityScore = score(df_Aux_ComorbiditiesElixhauser,
                                                                  weights = "vw",
                                                                  assign0 = TRUE))

# Add Comorbidity Score to Case Data
df_ADM_CaseInfo <- df_ADM_CaseInfo %>%
                        left_join(df_Aux_ScoreElixhauser, by = join_by(CasePseudonym))
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_ADM_CaseProcedures: Data Frame containing information about Procedures coded for each case
#===============================================================================

df_Aux_OPSCoding <- df_Meta_OPSCoding %>%
                        select(OPSCode, Procedure)


df_ADM_Procedures <- df_ADM_CaseInfo %>%
                          left_join(df_IDM_CasesOPS, by = join_by(CasePseudonym)) %>%
                          mutate(OPSCodeShort = str_sub(OPSCode, end = 4)) %>%
                          left_join(df_Aux_OPSCoding, by = join_by(OPSCodeShort == OPSCode)) %>%
                          mutate(ProcedureType = case_when(str_starts(OPSCodeShort, "5") & OPSCodeShort %notin% c("5411", "5936") ~ "Surgery",
                                                           str_starts(OPSCodeShort, "854") & OPSCodeShort %notin% c("8547", "8548") ~ "Chemotherapy",
                                                           OPSCodeShort == "8547" ~ "Immunotherapy",
                                                           str_starts(OPSCodeShort, "852") ~ "Radiotherapy",
                                                           str_starts(OPSCodeShort, "853") ~ "Nuclear Medicine Therapy",
                                                           OPSCodeShort == "8805" ~ "Stem Cell Therapy",
                                                           OPSCodeShort == "5411" ~ "Bone Marrow Transplant",
                                                           OPSCodeShort == "5936" ~ "Potential CAR-T-Cell Therapy",
                                                           OPSCodeShort == "8548" ~ "Antiretroviral Therapy",
                                                           OPSCodeShort %in% c("8701", "8704", "8706", "8712", "8713", "8714") ~ "Ventilation",
                                                           OPSCodeShort %in% c("8853", "8854", "8855", "8857") ~ "Dialysis")) %>%
                          left_join(df_Meta_CancerSurgery, by = join_by(MainCancerCodeShort == ICDCodeShort,
                                                                        OPSCodeShort == OPSCodeShort))
                          #=== Update Progress Bar ===
                          try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



df_Aux_ProcedureEvents <- df_ADM_Procedures %>%
                              group_by(PatientPseudonym) %>%
                                  mutate(EventDate = OPSDate,
                                         EventClass = "Procedure",
                                         EventSubclass = ProcedureType,
                                         EventSpecification_A = case_when(ProcedureType == "Surgery" & IsLikelyCurativeIntention_Primary == TRUE ~ "CancerSurgery_CurativeIntention_Primary",
                                                                        ProcedureType == "Surgery" & IsLikelyCurativeIntention_Primary == TRUE ~ "CancerSurgery_CurativeIntention_Secondary",
                                                                        ProcedureType == "Surgery" & IsLikelySupportive_Direct == TRUE ~ "CancerSurgery_Supportive",
                                                                        ProcedureType == "Surgery" & IsLikelyCancerRelated == TRUE ~ "CancerSurgery_Other",
                                                                        ProcedureType == "Surgery" & is.na(IsLikelyCancerRelated) ~ "OtherSurgery",
                                                                        TRUE ~ ProcedureType),
                                         EventSpecification_B = Procedure,
                                         EventSpecification_C = NA,
                                         EventLevelOfCare = NA) %>%
                                  filter(is.na(EventSubclass) == FALSE) %>%
                                  select(PatientPseudonym,
                                         CasePseudonym,
                                         EventDate,
                                         EventClass,
                                         EventSubclass,
                                         EventSpecification_A,
                                         EventSpecification_B,
                                         EventLevelOfCare) %>%
                                  distinct() %>%
                              ungroup()
                              #=== Update Progress Bar ===
                              try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)
                              

df_ADM_Events <- df_ADM_Events %>%
                      ungroup() %>%
                      add_row(df_Aux_ProcedureEvents) %>%
                      filter(EventSpecification_A != "OtherSurgery") %>%
                      group_by(PatientPseudonym, CasePseudonym) %>%
                          fill(c(YearOfBirth,
                                 Sex,
                                 AdmissionAge,
                                 DepartmentCode,
                                 Department,
                                 OperatingSpecialty)) %>%
                          arrange(PatientPseudonym, CasePseudonym, EventDate) %>%
                          fill(CaseNumber,
                               EventLevelOfCare) %>%
                          mutate(EventClass = factor(EventClass, levels = c("Administration", "Diagnosis", "Procedure")),      # Factorize EventClass to establish order of Admission and Diagnosis events if they have the same EventDate
                                 EventLevelOfCare = ifelse(EventClass == "Diagnosis",
                                                           "Not Specified",
                                                           EventLevelOfCare),
                                 EventSubclass = case_when(EventSubclass == "Transfer" & EventLevelOfCare == "ICU" ~ "TransferICU",
                                                           EventSubclass == "Transfer" & EventLevelOfCare == "NonICU" ~ "TransferNonICU",
                                                           EventSubclass == "Transfer" & EventLevelOfCare == "Unknown" ~ "TransferNonICU",
                                                           TRUE ~ EventSubclass),
                                 EventSpecification_A = case_when(EventSubclass == "Chemotherapy" & EventLevelOfCare == "ICU" ~ "ChemotherapyICU",
                                                                EventSubclass == "Chemotherapy" & EventLevelOfCare == "NonICU" ~ "ChemotherapyNonICU",
                                                                EventSubclass == "Chemotherapy" & EventLevelOfCare == "Unknown" ~ "ChemotherapyUnknown",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "ICU" ~ "DialysisICU",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "NonICU" ~ "DialysisNonICU",
                                                                EventSubclass == "Dialysis" & EventLevelOfCare == "Unknown" ~ "DialysisUnknown",
                                                                TRUE ~ EventSpecification_A)) %>%
                      group_by(CasePseudonym, EventSpecification_A) %>%
                          arrange(EventDate, .by_group = TRUE) %>%
                          mutate(IsFirstOfProcedure_WithinCase = (EventClass == "Procedure" & row_number() == 1)) %>%
                      group_by(PatientPseudonym, EventSpecification_A) %>%
                          arrange(EventDate, .by_group = TRUE) %>%
                          mutate(IsFirstOfProcedure_AcrossCases = (EventClass == "Procedure" & row_number() == 1)) %>%
                      ungroup() %>%
                      arrange(PatientPseudonym, EventDate, EventClass)
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)
                              


df_ADM_Events <- df_ADM_Events %>%
                      group_by(PatientPseudonym) %>%
                          arrange(EventDate, EventClass, .by_group = TRUE) %>%
                          mutate(LastSurgeryDate = as_datetime(ifelse(EventSubclass == "Surgery",
                                                                      EventDate,
                                                                      NA)),
                                 LastChemotherapyDate = as_datetime(ifelse(EventSubclass == "Chemotherapy",
                                                                           EventDate,
                                                                           NA)),
                                 LastChemotherapyDate_NonICU = as_datetime(ifelse(EventSpecification_A %in% c("ChemotherapyNonICU", "ChemotherapyUnknown"),
                                                                                  EventDate,
                                                                                  NA))) %>%
                          fill(LastSurgeryDate,
                               LastChemotherapyDate,
                               LastChemotherapyDate_NonICU) %>%
                          mutate(DaysSinceLastSurgery = round(as.numeric(difftime(EventDate, LastSurgeryDate, units = "days")), digits = 1),
                                 DaysSinceLastChemotherapy_NonICU = round(as.numeric(difftime(EventDate, LastChemotherapyDate_NonICU, units = "days")), digits = 1),
                                 IsPotentialComplication_AcrossCases = (is.na(DaysSinceLastChemotherapy_NonICU) == FALSE & between(DaysSinceLastChemotherapy_NonICU, 0, 30))      # Check if there has been a (Non-ICU-) Chemotherapy event in the past 20 days
                                                                          & (is.na(DaysSinceLastSurgery) == TRUE | DaysSinceLastSurgery > 7)      # Ensure that there has not been a preceding Surgery event in the 7 days before potential complication (across cases)
                                                                          & EventSubclass %in% c("TransferICU",
                                                                                                 "Dialysis",
                                                                                                 "Ventilation")) %>%      # Admission to ICU, Dialysis or Ventilation event after Chemotherapy without recent surgery is assumed as potential complication occurrence
                      group_by(PatientPseudonym, IsPotentialComplication_AcrossCases) %>%                                   
                          mutate(IsFirstPotentialComplication_AcrossCases = (IsPotentialComplication_AcrossCases == TRUE & row_number() == 1)) %>%
                      group_by(PatientPseudonym, CasePseudonym) %>%
                          arrange(EventDate, EventClass, .by_group = TRUE) %>%
                          mutate(IsPotentialComplication_WithinCase = cumsum(EventSpecification_A == "ChemotherapyNonICU") > 0      # Check if there has been a preceding (Non-ICU-) Chemotherapy event
                                                                        & (is.na(DaysSinceLastSurgery) == TRUE | DaysSinceLastSurgery > 7)      # Ensure that there has not been a Surgery event during the 7 days prior
                                                                        & EventSubclass %in% c("TransferICU",
                                                                                               "Dialysis",
                                                                                               "Ventilation")) %>%      # Admission to ICU, Dialysis or Ventilation event after Chemotherapy without recent surgery is assumed as potential complication occurrence
                      group_by(PatientPseudonym, CasePseudonym, IsPotentialComplication_WithinCase) %>%                                   
                          mutate(IsFirstPotentialComplication_WithinCase = (IsPotentialComplication_WithinCase == TRUE & row_number() == 1)) %>%
                      group_by(PatientPseudonym) %>%
                          arrange(EventDate, EventClass, .by_group = TRUE) %>%
                          mutate(IsPotentialComplication_AcrossCases = ifelse(IsPotentialComplication_AcrossCases == TRUE
                                                                                & EventSubclass == "Dialysis"
                                                                                & cumsum(EventSpecification_A == "Chronic Renal Failure") > 0,
                                                                              FALSE,
                                                                              IsPotentialComplication_AcrossCases),
                                 IsPotentialComplication_WithinCase = ifelse(IsPotentialComplication_WithinCase == TRUE
                                                                                & EventSubclass == "Dialysis"
                                                                                & cumsum(EventSpecification_A == "Chronic Renal Failure") > 0,
                                                                              FALSE,
                                                                              IsPotentialComplication_WithinCase),
                                 TimeChemoToComplication_AcrossCases = ifelse(IsPotentialComplication_AcrossCases == TRUE,
                                                                              DaysSinceLastChemotherapy_NonICU[IsPotentialComplication_AcrossCases == TRUE],
                                                                              NA),
                                 TimeChemoToComplication_WithinCase = ifelse(IsPotentialComplication_WithinCase == TRUE,
                                                                              DaysSinceLastChemotherapy_NonICU[IsPotentialComplication_WithinCase == TRUE],
                                                                              NA)) %>%
                      ungroup()
                      #=== Update Progress Bar ===
                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#   - Presumed...DiagnosisDate
#   - FirstMainAdmissionDate: Date of main cancer diagnosis or HIV diagnosis, whichever is earlier. If there is no main cancer or HIV diagnosis, take earliest admission date.
#   - FirstMainAdmissionAge: Age at first main admission
#   - LastRecordedDischargeDate
#   - LastRecordedDischargeCategory: Taken as rough momentary "outcome" of medical care

df_Aux_PatientSummaries_Events <- df_ADM_Events %>%
                                      group_by(PatientPseudonym) %>%
                                      summarize(PresumedMainCancerDiagnosisDate = as_datetime(ifelse(any(EventSpecification_A == "Main Cancer Diagnosis"),
                                                                                                     EventDate[EventSpecification_A == "Main Cancer Diagnosis"],
                                                                                                     NA)),
                                                PresumedMainCancerIsLikelyFirstDiagnosis = any(EventSpecification_C == "Likely First Diagnosis"),
                                                PresumedMainCancerFirstDiagnosisDate = as_datetime(ifelse(any(EventSpecification_C == "Likely First Diagnosis"),
                                                                                                          EventDate[EventSpecification_C == "Likely First Diagnosis"],
                                                                                                          NA)),
                                                PresumedMetastasisDiagnosisDate = as_datetime(ifelse(any(EventSpecification_A == "Metastasis Diagnosis"),
                                                                                                     EventDate[EventSpecification_A == "Metastasis Diagnosis"],
                                                                                                     NA)),
                                                PresumedHIVDiagnosisDate = as_datetime(ifelse(any(EventSpecification_A == "HIV Diagnosis"),
                                                                                              EventDate[EventSpecification_A == "HIV Diagnosis"],
                                                                                              NA)),
                                                PresumedAIDSDiagnosisDate = as_datetime(ifelse(any(EventSpecification_A == "AIDS Diagnosis"),
                                                                                               EventDate[EventSpecification_A == "AIDS Diagnosis"],
                                                                                               NA)),
                                                #-------------------------------
                                                FirstRelevantAdmissionDate = as_datetime(ifelse(sum(EventSpecification_A == "Main Cancer Diagnosis") == 0 &
                                                                                                  sum(EventSpecification_A == "HIV Diagnosis") == 0,      # If patient holds no main cancer or HIV diagnosis
                                                                                                min(EventDate[EventSubclass == "Admission"], na.rm = TRUE),      # Then take the first admission date as value
                                                                                                min(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate, na.rm = TRUE))),
                                                FirstRelevantAdmissionYear = year(FirstRelevantAdmissionDate),
                                                FirstRelevantAdmissionAge = first(AdmissionAge[EventDate == FirstRelevantAdmissionDate]),
                                                #-------------------------------
                                                MainCancerDiagnosisYear = year(PresumedMainCancerDiagnosisDate),
                                                MainCancerDiagnosisAge = first(AdmissionAge[EventDate == PresumedMainCancerDiagnosisDate]),
                                                MainCancerDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), PresumedMainCancerDiagnosisDate, units = "days")) + 1),
                                                #-------------------------------
                                                TotalDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), min(EventDate), units = "days")) + 1),
                                                RelevantDocumentedTimeSpan = ceiling(as.numeric(difftime(max(EventDate), min(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate), units = "days")) + 1),
                                                #-------------------------------
                                                MainCancerFirstStay_TimeToFirstNonSurgicalTherapy = round(min(c(100000, as.numeric(difftime(as_datetime(EventDate[EventSubclass %in% c("Chemotherapy",
                                                                                                                                                                                       "Immunotherapy",
                                                                                                                                                                                       "Radiotherapy",
                                                                                                                                                                                       "Nuclear Medicine Therapy",
                                                                                                                                                                                       "Stem Cell Therapy",
                                                                                                                                                                                       "Bone Marrow Transplant",
                                                                                                                                                                                       "Potential CAR-T-Cell Therapy")]),
                                                                                                                                        PresumedMainCancerDiagnosisDate,
                                                                                                                                        units = "days"))), na.rm = TRUE), 1),
                                                MainCancerFirstStay_TimeToFirstNonSurgicalTherapy = ifelse(MainCancerFirstStay_TimeToFirstNonSurgicalTherapy == 100000,
                                                                                                           NA,
                                                                                                           MainCancerFirstStay_TimeToFirstNonSurgicalTherapy),
                                                #--- Calculate time intervals between significant disease diagnosis / progress
                                                TimeHIVToCancer = ceiling(as.numeric(difftime(PresumedMainCancerDiagnosisDate, PresumedHIVDiagnosisDate, units = "days"))),
                                                TimeHIVToAIDS = ceiling(as.numeric(difftime(PresumedAIDSDiagnosisDate, PresumedHIVDiagnosisDate, units = "days"))),
                                                TimeAIDSToCancer = ceiling(as.numeric(difftime(PresumedMainCancerDiagnosisDate, PresumedAIDSDiagnosisDate, units = "days"))),
                                                TimeCancerToMetastasis = ceiling(as.numeric(difftime(PresumedMetastasisDiagnosisDate, PresumedMainCancerDiagnosisDate, units = "days"))),
                                                #--- Replace invalid time intervals with NA ---------
                                                TimeHIVToCancer = replace(TimeHIVToCancer, which(TimeHIVToCancer < 0), NA),
                                                TimeHIVToAIDS = replace(TimeHIVToAIDS, which(TimeHIVToAIDS < 0), NA),
                                                TimeAIDSToCancer = replace(TimeAIDSToCancer, which(TimeAIDSToCancer < 0), NA),
                                                TimeCancerToMetastasis = replace(TimeCancerToMetastasis, which(TimeCancerToMetastasis < 0), NA),
                                                #-------------------------------
                                                LastRecordedDischargeDate = max(EventDate[EventSubclass == "Discharge"], na.rm = TRUE),
                                                LastRecordedDischargeCategory = EventSpecification_A[EventSubclass == "Discharge" & EventDate == LastRecordedDischargeDate]) %>%
                                      ungroup()
                                      #=== Update Progress Bar ===
                                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)

                                    

#===============================================================================
# df_Aux_PatientSummaries_CaseInfo
#===============================================================================

#   - PrimaryPostalCode: Postal code taken from first admission
#   - CaseCount: Number of cases per patient
#   - MeanLengthOfStay: Per patient mean length of stay across cases
#         - Calculate mean length of stay per patient based on age at admission (so only cases of the corresponding age group are included in mean calculation)

df_Aux_PatientSummaries_CaseInfo <- df_ADM_CaseInfo %>%
                                        group_by(PatientPseudonym) %>%
                                            arrange(AdmissionDate, .by_group = TRUE) %>%
                                            summarize(YearOfBirth = first(YearOfBirth),
                                                      Sex = first(Sex),
                                                      PrimaryPostalCode = first(PostalCode),
                                                      #-------------------------
                                                      CaseCount = n_distinct(CasePseudonym),
                                                      LengthOfStayTotal = sum(LengthOfStay),
                                                      #-------------------------
                                                      ICUTransfersAbsolute = sum(CountTransfersICU, na.rm = TRUE),
                                                      ICUTransfersRelative = round(ICUTransfersAbsolute / CaseCount, 2),
                                                      ICUTimeAbsolute = sum(CalculatedTimeInICU, na.rm = TRUE),
                                                      ICUTimeRelative = round(ICUTimeAbsolute / LengthOfStayTotal, 2),
                                                      #-------------------------
                                                      MeanLengthOfStay = round(mean(LengthOfStay, na.rm = TRUE), 2),
                                                      MeanLengthOfStay_Age0To19 = round(mean(LengthOfStay[AdmissionAge < 20], na.rm = TRUE), 2),
                                                      MeanLengthOfStay_Age20To39 = round(mean(LengthOfStay[between(AdmissionAge, 20, 39)], na.rm = TRUE), 2),
                                                      MeanLengthOfStay_Age40To59 = round(mean(LengthOfStay[between(AdmissionAge, 40, 59)], na.rm = TRUE), 2),
                                                      MeanLengthOfStay_Age60To79 = round(mean(LengthOfStay[between(AdmissionAge, 60, 79)], na.rm = TRUE), 2),
                                                      MeanLengthOfStay_Age80AndAbove = round(mean(LengthOfStay[AdmissionAge >= 80], na.rm = TRUE), 2),
                                                      #-------------------------
                                                      MainCancerFirstStay_Length = ifelse(any(CaseHoldsMainCancerDiagnosis == TRUE),
                                                                                              LengthOfStay[CaseHoldsMainCancerDiagnosis == TRUE],
                                                                                              NA),
                                                      ComorbidityScoreAtMainCancerDiagnosis = ifelse(any(CaseHoldsMainCancerDiagnosis == TRUE),
                                                                                                     ComorbidityScore[CaseHoldsMainCancerDiagnosis == TRUE],
                                                                                                     NA)) %>%
                                            mutate(across(starts_with("MeanLengthOfStay"), ~ ifelse(is.nan(.x), NA, .x))) %>%
                                        ungroup()
                                        #=== Update Progress Bar ===
                                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)

                                        

#===============================================================================
# df_ADM_Patients: Consolidate patient-specific data
#===============================================================================
#   - Join all previously created Patient Summaries
#-------------------------------------------------------------------------------

df_ADM_Patients <- df_Aux_PatientSummaries_CaseInfo %>%
                        left_join(df_Aux_PatientSummaries_Diagnoses, by = join_by(PatientPseudonym)) %>%
                        left_join(df_Aux_PatientSummaries_Events, by = join_by(PatientPseudonym))



#===============================================================================     
# PATIENT SELECTION
#===============================================================================
#   - Keep only patients with ICD codes for cancer and/or HIV
#   - Keep only patients with main documentation starting at adult age
#-------------------------------------------------------------------------------

df_ADM_Patients <- df_ADM_Patients %>%
                        filter(PatientHoldsCancerCodes == TRUE | PatientHoldsHIVCodes == TRUE) %>%
                        filter(FirstRelevantAdmissionAge >= 18)


# Update Attrition Tracker
# ------------------------------------------------------------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_Patients$PatientPseudonym),
                                                         inp_InclusionComment = "Patients with ICD codes for cancer and/or HIV")



#===============================================================================
# PATIENT SELECTION df_ADM_Patients
#===============================================================================
#   - Among cancer coded patients, keep only ones with a plausible presumed cancer diagnosis, so with least one "main cancer code"
#-------------------------------------------------------------------------------
# Define main patient subgroups:
#     - Patients with presumed cancer only
#     - Patients with presumed HIV only
#     - Patients with presumed HIV infection and cancer
# Take group names and labeling from global variable
#-------------------------------------------------------------------------------

df_ADM_Patients <- df_ADM_Patients %>%
                        filter((PatientHoldsCancerCodes == TRUE & PatientHoldsMainCancerDiagnosis == TRUE) |
                                  (PatientHoldsCancerCodes == FALSE & PatientHoldsHIVCodes == TRUE)) %>%
                        #--- Add Subgroup variable ---------------------------------
                        mutate(PatientSubgroup = case_when(PatientHoldsCancerCodes == TRUE & PatientHoldsHIVCodes == FALSE ~ "Cancer+/HIV-",
                                                           PatientHoldsCancerCodes == TRUE & PatientHoldsHIVCodes == TRUE ~ "Cancer+/HIV+",
                                                           PatientHoldsCancerCodes == FALSE & PatientHoldsHIVCodes == TRUE ~ "Cancer-/HIV+"),
                               PatientSubgroup = factor(PatientSubgroup,
                                                        levels = vc_MainSubgroups,
                                                        labels = names(vc_MainSubgroups)),
                                    .after = PatientPseudonym)
                        #=== Update Progress Bar ===
                        try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)


# Update Attrition Tracker
# ------------------------------------------------------------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_Patients$PatientPseudonym),
                                                                     inp_InclusionComment = "Patients with plausibly coded cancer diagnosis")



#===============================================================================
# PATIENT SELECTION df_ADM_PatientsCancer: Only patients with cancer
#===============================================================================

df_ADM_PatientsCancer <- df_ADM_Patients %>%
                              filter(PatientHoldsMainCancerDiagnosis == TRUE)


# Update Attrition Tracker
# ------------------------------------------------------------------------------
df_Mon_AttritionTracker_CancerPatients <- df_Mon_AttritionTracker_CancerPatients %>%
                                              f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_PatientsCancer$PatientPseudonym),
                                                                       inp_InclusionComment = "Initial Sample Size")



#===============================================================================
# Create auxiliary data frames with summarizing information about cancer patients
#===============================================================================

df_Aux_CancerPatientSummaries_Progress <- df_ADM_Events %>%
                                              filter(PatientPseudonym %in% df_ADM_PatientsCancer$PatientPseudonym) %>%
                                              group_by(PatientPseudonym) %>%
                                                  summarize(HadAnyCancerTherapy = any(str_starts(EventSpecification_A, "CancerSurgery")) |
                                                                                    any(EventSubclass %in% c("Chemotherapy",
                                                                                                             "Immunotherapy",
                                                                                                             "Radiotherapy",
                                                                                                             "Nuclear Medicine Therapy",
                                                                                                             "Stem Cell Therapy",
                                                                                                             "Bone Marrow Transplant",
                                                                                                             "Potential CAR-T-Cell Therapy")),
                                                            #-------------------
                                                            HadAnyCancerSurgery = any(str_starts(EventSpecification_A, "CancerSurgery")),
                                                            HadCancerSurgery_CurativeIntention = any(EventSpecification_A %in% c("CancerSurgery_CurativeIntention_Primary",
                                                                                                                               "CancerSurgery_CurativeIntention_Secondary")),
                                                            HadCancerSurgery_Supportive = any(EventSpecification_A == "CancerSurgery_Supportive"),
                                                            HadCancerSurgery_Other = any(EventSpecification_A == "CancerSurgery_Other"),
                                                            HadChemotherapy = any(EventSubclass == "Chemotherapy"),
                                                            HadImmunotherapy = any(EventSubclass == "Immunotherapy"),
                                                            HadRadiotherapy = any(EventSubclass == "Radiotherapy"),
                                                            HadNuclearmedTherapy = any(EventSubclass == "Nuclear Medicine Therapy"),
                                                            HadStemCellTherapy = any(EventSubclass == "Stem Cell Therapy"),
                                                            HadBoneMarrowTransplant = any(EventSubclass == "Bone Marrow Transplant"),
                                                            HadPotentialCARTCellTherapy = any(EventSubclass == "Potential CAR-T-Cell Therapy"),
                                                            #-------------------
                                                            DateFirstCancerSurgery = as.Date(as_datetime(ifelse(any(EventSpecification_A %in% c("CancerSurgery_CurativeIntention_Primary",
                                                                                                                          "CancerSurgery_CurativeIntention_Secondary",
                                                                                                                          "CancerSurger_Supportive")),
                                                                                                         min(EventDate[str_starts(EventSpecification_A, "CancerSurgery")], na.rm = TRUE),
                                                                                                         NA))),
                                                            DateFirstChemotherapy = as.Date(as_datetime(ifelse(HadChemotherapy == TRUE,
                                                                                                        min(EventDate[EventSubclass == "Chemotherapy"], na.rm = TRUE),
                                                                                                        NA))),
                                                            DateFirstImmunotherapy = as.Date(as_datetime(ifelse(HadImmunotherapy == TRUE,
                                                                                                         min(EventDate[EventSubclass == "Immunotherapy"], na.rm = TRUE),
                                                                                                         NA))),
                                                            DateFirstRadiotherapy = as.Date(as_datetime(ifelse(HadRadiotherapy == TRUE,
                                                                                                        min(EventDate[EventSubclass == "Radiotherapy"], na.rm = TRUE),
                                                                                                        NA))),
                                                            DateFirstNuclearmedTherapy = as.Date(as_datetime(ifelse(HadNuclearmedTherapy == TRUE,
                                                                                                             min(EventDate[EventSubclass == "Nuclear Medicine Therapy"], na.rm = TRUE),
                                                                                                             NA))),
                                                            DateFirstStemCellTherapy = as.Date(as_datetime(ifelse(HadStemCellTherapy == TRUE,
                                                                                                           min(EventDate[EventSubclass == "Stem Cell Therapy"], na.rm = TRUE),
                                                                                                           NA))),
                                                            DateFirstBoneMarrowTransplant = as.Date(as_datetime(ifelse(HadBoneMarrowTransplant == TRUE,
                                                                                                                     min(EventDate[EventSubclass == "Bone Marrow Transplant"], na.rm = TRUE),
                                                                                                                     NA))),
                                                            DateFirstPotentialCARTCellTherapy = as.Date(as_datetime(ifelse(HadPotentialCARTCellTherapy == TRUE,
                                                                                                                    min(EventDate[EventSubclass == "Potential CAR-T-Cell Therapy"], na.rm = TRUE),
                                                                                                                    NA))),
                                                            #-------------------
                                                            HadComplicationAfterChemo = any(IsPotentialComplication_WithinCase == TRUE) | any(IsPotentialComplication_AcrossCases == TRUE),
                                                            HadComplication_Ventilation = any(IsPotentialComplication_AcrossCases == TRUE & EventSubclass == "Ventilation"),
                                                            HadComplication_Dialysis = any(IsPotentialComplication_AcrossCases == TRUE & EventSubclass == "Dialysis"),
                                                            HadComplication_TransferICU = any(IsPotentialComplication_AcrossCases == TRUE & EventSubclass == "TransferICU"),
                                                            DateFirstComplicationAfterChemo = as.Date(as_datetime(ifelse(any(IsFirstPotentialComplication_AcrossCases == TRUE),
                                                                                                                         EventDate[IsFirstPotentialComplication_AcrossCases == TRUE],
                                                                                                                         NA))),
                                                            TimeChemoToFirstComplication = ifelse(HadComplicationAfterChemo == TRUE,
                                                                                                  TimeChemoToComplication_AcrossCases[IsFirstPotentialComplication_AcrossCases],
                                                                                                  NA),
                                                            #-------------------
                                                            HadMetastasis = any(EventSpecification_A == "Metastasis Diagnosis")) %>%
                                              ungroup()
                                              #=== Update Progress Bar ===
                                              try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_Aux_CancerPatientSummaries_TherapySequence
#===============================================================================
# Data frame containing information about sequence of:
#     - Presumed onset of different therapy modalities
#     - Presumed first complication after chemotherapy
#-------------------------------------------------------------------------------

df_Aux_CancerPatientSummaries_TherapySequence <- df_Aux_CancerPatientSummaries_Progress %>%
                                                      select(PatientPseudonym,
                                                             DateFirstCancerSurgery,
                                                             DateFirstChemotherapy,
                                                             DateFirstImmunotherapy,
                                                             DateFirstRadiotherapy,
                                                             DateFirstNuclearmedTherapy,
                                                             DateFirstStemCellTherapy,
                                                             DateFirstBoneMarrowTransplant,
                                                             DateFirstPotentialCARTCellTherapy,
                                                             DateFirstComplicationAfterChemo) %>%
                                                      filter(!if_all(c(everything(), -PatientPseudonym), ~ is.na(.x))) %>%
                                                      group_by(PatientPseudonym) %>%
                                                          pivot_longer(c(everything(), -PatientPseudonym),
                                                                       names_to = "Event",
                                                                       values_to = "TherapyOnsetDate") %>%
                                                          filter(!is.na(TherapyOnsetDate)) %>%
                                                          arrange(PatientPseudonym, TherapyOnsetDate) %>%
                                                      group_by(PatientPseudonym, TherapyOnsetDate) %>%
                                                          summarize(ColSurgery = ifelse(any(Event == "DateFirstCancerSurgery"), "Surgery", NA),
                                                                    ColChemotherapy = ifelse(any(Event == "DateFirstChemotherapy"), "Chemotherapy", NA),
                                                                    ColImmunotherapy = ifelse(any(Event == "DateFirstImmunotherapy"), "Immunotherapy", NA),
                                                                    ColRadiotherapy = ifelse(any(Event == "DateFirstRadiotherapy"), "Radiotherapy", NA),
                                                                    ColNuclearmedTherapy = ifelse(any(Event == "DateFirstNuclearmedTherapy"), "Nuclear Medicine Therapy", NA),
                                                                    ColStemCellTherapy = ifelse(any(Event == "DateFirstStemCellTherapy"), "Nuclear Medicine Therapy", NA),
                                                                    ColBoneMarrowTransplant = ifelse(any(Event == "DateFirstBoneMarrowTransplant"), "Bone Marrow Transplant", NA),
                                                                    ColCARTCellTherapy = ifelse(any(Event == "DateFirstPotentialCARTCellTherapy"), "CAR-T-Cell Therapy", NA),
                                                                    ColComplication = ifelse(any(Event == "DateFirstComplicationAfterChemo"), "Complication", NA)) %>%
                                                          unite(col = "TherapyOnset",
                                                                c("ColSurgery",
                                                                  "ColChemotherapy",
                                                                  "ColImmunotherapy",
                                                                  "ColRadiotherapy",
                                                                  "ColNuclearmedTherapy",
                                                                  "ColStemCellTherapy",
                                                                  "ColBoneMarrowTransplant",
                                                                  "ColCARTCellTherapy",
                                                                  "ColComplication"),
                                                                remove = TRUE, na.rm = TRUE, sep = " & ") %>%
                                                          mutate(Stage = row_number(), .after = PatientPseudonym) %>%
                                                      group_by(PatientPseudonym) %>%
                                                      pivot_wider(names_from = Stage, values_from = c(TherapyOnset, TherapyOnsetDate)) %>%
                                                      ungroup()
                                                      #=== Update Progress Bar ===
                                                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_ADM_PatientsCancer
#===============================================================================
#   - Join all previously created Patient Summaries
#   - Add Subgrouping of relation between cancer entity and HIV, regardless of actual HIV status
#-------------------------------------------------------------------------------

df_ADM_PatientsCancer <- df_ADM_PatientsCancer %>%
                              left_join(df_Aux_CancerPatientSummaries_Progress, by = join_by(PatientPseudonym)) %>%
                              left_join(df_Aux_CancerPatientSummaries_TherapySequence, by = join_by(PatientPseudonym)) %>%
                              mutate(PatientSubgroupHIVCancerCategory = case_when(MainCancerIsAD == TRUE ~ "HIV-associated AD cancer",
                                                                                  MainCancerIsHIVNonAD == TRUE ~ "HIV-associated non-AD cancer",
                                                                                  (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE) ~ "Non-HIV-associated cancer"))



#===============================================================================
# SUBSET: Patients with presumed HIV and cancer
#===============================================================================
# Add variable regarding order of presumed HIV and cancer diagnosis:
#     - HIV before cancer
#     - Diagnosed simultaneously
#     - Cancer before HIV
#-------------------------------------------------------------------------------
# Add variable regarding cancer categories in patients with presumed HIV infection and cancer:
#     - Patients with AIDS-defining cancer, occurring in manifested AIDS
#     - Patients with AIDS-defining cancer, occurring before presumed HIV diagnosis, so can not be considered manifested AIDS
#     - Patients with HIV-associated, non-AIDS-defining cancer, occurring at or after presumed HIV diagnosis
#     - Patients with HIV-associated, non-AIDS-defining cancer, occurring before presumed HIV diagnosis
#     - Patients with Non-HIV-associated cancer, occurring at or after presumed HIV diagnosis
#     - Patients with Non-HIV-associated cancer, occurring before presumed HIV diagnosis
#-------------------------------------------------------------------------------
# Add variable regarding AIDS occurrence in patients with presumed HIV infection and cancer 
#     - Patients with presumed AIDS diagnosis at or before presumed cancer diagnosis
#     - Patients with presumed AIDS diagnosis after presumed cancer diagnosis
#     - Patients with cancer without AIDS
#-------------------------------------------------------------------------------

df_ADM_PatientsHIVCancer <- df_ADM_PatientsCancer %>%
                                filter(PatientHoldsHIVCodes == TRUE) %>%
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                mutate(HIVCancerDiagnosisOrder = case_when(PresumedHIVDiagnosisDate < PresumedMainCancerDiagnosisDate ~ "HIV before cancer",
                                                                           PresumedHIVDiagnosisDate == PresumedMainCancerDiagnosisDate ~ "Diagnosed simultaneously",
                                                                           PresumedHIVDiagnosisDate > PresumedMainCancerDiagnosisDate ~ "Cancer before HIV"),
                                       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                       HIVCancerCategory = case_when((MainCancerIsAD == TRUE &
                                                                        PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "HIV-associated AD cancer",
                                                                     (MainCancerIsAD == TRUE &
                                                                        PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "HIV-associated AD cancer before presumed HIV diagnosis",
                                                                     (MainCancerIsHIVNonAD == TRUE &
                                                                        PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "HIV-associated non-AD cancer",
                                                                     (MainCancerIsHIVNonAD == TRUE &
                                                                        PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "HIV-associated non-AD cancer before presumed HIV diagnosis",
                                                                     (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE &
                                                                        PresumedMainCancerDiagnosisDate >= PresumedHIVDiagnosisDate) ~ "Non-HIV-associated cancer",
                                                                     (MainCancerIsAD == FALSE & MainCancerIsHIVNonAD == FALSE &
                                                                        PresumedMainCancerDiagnosisDate < PresumedHIVDiagnosisDate) ~ "Non-HIV-associated cancer before presumed HIV diagnosis"),
                                       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                       AIDSOccurrence = case_when((PatientHoldsAIDSCodes == TRUE &
                                                                      PresumedAIDSDiagnosisDate <= PresumedMainCancerDiagnosisDate) ~ "AIDS at or before cancer diagnosis",
                                                                  (PatientHoldsAIDSCodes == TRUE &
                                                                      PresumedAIDSDiagnosisDate > PresumedMainCancerDiagnosisDate) ~ "AIDS after cancer diagnosis",
                                                                   PatientHoldsAIDSCodes == FALSE ~ "Cancer and HIV without AIDS")) %>%      # Most general case is evaluated last, following case_when syntax
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                ungroup()
                                #=== Update Progress Bar ===
                                try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)


vc_HIVCancerCategory <- unique(df_ADM_PatientsHIVCancer$HIVCancerCategory)
names(vc_HIVCancerCategory) <- vc_HIVCancerCategory
                                


#===============================================================================
# df_Aux_HIVCancerPatientSummaries_HIVCancerSequence
#===============================================================================
# Data frame containing information about sequence of:
#     - Cancer diagnosis
#     - Metastasis diagnosis
#     - HIV diagnosis
#     - AIDS diagnosis
# ... and all combinations
#-------------------------------------------------------------------------------

df_Aux_HIVCancerPatientSummaries_HIVCancerSequence <- df_ADM_PatientsHIVCancer %>%
                                                          select(PatientPseudonym,
                                                                 PresumedMainCancerDiagnosisDate,
                                                                 PresumedMetastasisDiagnosisDate,
                                                                 PresumedHIVDiagnosisDate,
                                                                 PresumedAIDSDiagnosisDate) %>%
                                                          filter(!if_all(c(everything(), -PatientPseudonym), ~ is.na(.x))) %>%
                                                          group_by(PatientPseudonym) %>%
                                                          pivot_longer(c(everything(), -PatientPseudonym),
                                                                       names_to = "Event",
                                                                       values_to = "DiagnosisSequenceDate") %>%
                                                          filter(!is.na(DiagnosisSequenceDate)) %>%
                                                          arrange(PatientPseudonym, DiagnosisSequenceDate) %>%
                                                      group_by(PatientPseudonym, DiagnosisSequenceDate) %>%
                                                          summarize(ColCancer = ifelse(any(Event == "PresumedMainCancerDiagnosisDate"), "Cancer", NA),
                                                                    ColMetastasis = ifelse(any(Event == "PresumedMetastasisDiagnosisDate"), "Metastasis", NA),
                                                                    ColHIV = ifelse(any(Event == "PresumedHIVDiagnosisDate"), "HIV", NA),
                                                                    ColAIDS = ifelse(any(Event == "PresumedAIDSDiagnosisDate"), "AIDS", NA)) %>%
                                                          unite(col = "DiagnosisSequence",
                                                                c("ColCancer",
                                                                  "ColMetastasis",
                                                                  "ColHIV",
                                                                  "ColAIDS"),
                                                                remove = TRUE, na.rm = TRUE, sep = " & ") %>%
                                                          mutate(Stage = row_number(), .after = PatientPseudonym) %>%
                                                      group_by(PatientPseudonym) %>%
                                                      pivot_wider(names_from = Stage, values_from = c(DiagnosisSequence, DiagnosisSequenceDate)) %>%
                                                      ungroup()
                                                      #=== Update Progress Bar ===
                                                      try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# df_ADM_PatientsHIVCancer
#===============================================================================
#   - Add Diagnosis Sequence
#-------------------------------------------------------------------------------

df_ADM_PatientsHIVCancer <- df_ADM_PatientsHIVCancer %>%
                                left_join(df_Aux_HIVCancerPatientSummaries_HIVCancerSequence, by = join_by(PatientPseudonym)) %>%
                                ungroup()
                                #=== Update Progress Bar ===
                                try(ls_Progress_Processing$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Processing), silent = TRUE)



#===============================================================================
# PATIENT SELECTION df_ADM_PatientsCancer_Strict / df_ADM_PatientsHIVCancer_Strict
#===============================================================================
#   - Among cancer coded patients, keep only ones with strictly plausible cancer diagnosis onset documentation
#-------------------------------------------------------------------------------

# df_ADM_PatientsCancer_Strict <- df_ADM_PatientsCancer %>%
#                                     filter(MainCancerFirstStay_Length > 4
#                                             & MainCancerFirstStay_TimeToFirstNonSurgicalTherapy > 2)
# 
# 
# df_ADM_PatientsHIVCancer_Strict <- df_ADM_PatientsHIVCancer %>%
#                                         filter(MainCancerFirstStay_Length > 4
#                                                 & MainCancerFirstStay_TimeToFirstNonSurgicalTherapy > 2)


# Update Attrition Tracker
# ------------------------------------------------------------------------------
df_Mon_AttritionTracker_CancerPatients <- df_Mon_AttritionTracker_CancerPatients %>%
                                              f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_ADM_PatientsCancer$PatientPseudonym),
                                                                       inp_InclusionComment = "No short first stays and no non-surgical therapy right after admission")
                                        


#===============================================================================
# Close Progress Bar
#===============================================================================

Sys.sleep(3)
try(close(ls_Progress_Processing$ProgressBar), silent = TRUE)
