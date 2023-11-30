

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: DATA EXCLUSION                                                    #
#------------------------------------------------------------------------------#
################################################################################


#   ==> For Exclusion Criteria, see Project Documentation -> Data Exclusion

#   Required Objects:
#   ~~~~~~~~~~~~~~~~~
#   df_IDM_Cases
#   df_IDM_CasesDepartment
#   df_IDM_CasesICD
#   df_IDM_CasesOPS


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude patients with inconsistent redundant CasePseudonyms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Find PatientPseudonyms belonging to inconsistently redundantly documented cases
vc_IneligiblePatients_1 <- df_IDM_Cases %>%
                                group_by(PatientPseudonym, CasePseudonym) %>%
                                summarize(N = n()) %>%
                                filter(N > 1) %>%
                                distinct(PatientPseudonym) %>%
                                pull(PatientPseudonym)

# Filter out Patients
df_IDM_Cases <- df_IDM_Cases %>%
                    filter(PatientPseudonym %in% vc_IneligiblePatients_1 == FALSE)


# Update Attrition Tracker
# ------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_IDM_Cases$PatientPseudonym),
                                                                     inp_ExclusionComment = "Patients with inconsistent redundant Cases")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude Patients having Cases that contain missing PatientPseudonym or CasePseudonym
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vc_IneligiblePatients_2 <- df_IDM_Cases %>%
                                filter(is.na(PatientPseudonym) | is.na(CasePseudonym)) %>%
                                distinct(PatientPseudonym) %>%
                                pull(PatientPseudonym)

# Filter out Patients with redundant CasePseudonyms
df_IDM_Cases <- df_IDM_Cases %>%
                    filter(PatientPseudonym %in% vc_IneligiblePatients_2 == FALSE)


# Update Attrition Tracker
# ------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_IDM_Cases$PatientPseudonym),
                                                                     inp_ExclusionComment = "Patients with Cases that contain missing Patient or Case Pseudonyms")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude patients with incomplete documentation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Find PatientPseudonyms belonging to incompletely documented cases
vc_IneligiblePatients_3 <- df_IDM_Cases %>%
                                left_join(df_IDM_CasesICD, by = "CasePseudonym") %>%      # Join with Diagnosis Data to broaden scope of incompletion search
                                select(-PostalCode,      # Here, Features are listed that are allowed to have missing values
                                       -TimeInICU,
                                       -ICDVersion,
                                       -SecondaryICDCode) %>%
                                filter(if_any(everything(), ~ is.na(.x))) %>%
                                distinct(PatientPseudonym) %>%
                                pull(PatientPseudonym)

# Filter out Patients with incompletely documented cases
df_IDM_Cases <- df_IDM_Cases %>%
                    filter(PatientPseudonym %in% vc_IneligiblePatients_3 == FALSE)


# Update Attrition Tracker
# ------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_IDM_Cases$PatientPseudonym),
                                                                     inp_ExclusionComment = "Patients with incompletely documented cases")

