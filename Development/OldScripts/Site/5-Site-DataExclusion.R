

################################################################################
#------------------------------------------------------------------------------#
#   DATA EXCLUSION                                                             #
#------------------------------------------------------------------------------#
################################################################################


#   ==> For Exclusion Criteria, see Project Documentation -> Data Exclusion

#   Required Objects:
#   ~~~~~~~~~~~~~~~~~
#    - df_SDM_Patients
#    - df_SDM_Diagnosis
#    - df_SDM_Tumor
#    - df_SDM_Histology
#    - df_SDM_TNM
#    - df_SDM_Metastasis
#    - df_SDM_Progress
#    - df_SDM_Surgery
#    - df_SDM_SystemicTherapy
#    - df_SDM_Radiotherapy
#    - df_SDM_BioSampling
#    - df_SDM_MolecularDiagnostics



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Attrition Tracker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_AttritionTracker <- df_AttritionTracker %>%
                          f_UpdateAttritionTracker(inp_NewSampleSize = ...,
                                                   inp_ExclusionComment = "Reason for Exclusion")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exclude more data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ...


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Attrition Tracker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_AttritionTracker <- df_AttritionTracker %>%
                            f_UpdateAttritionTracker(inp_NewSampleSize = ...,
                                                     inp_ExclusionComment = "Reason for Exclusion")

