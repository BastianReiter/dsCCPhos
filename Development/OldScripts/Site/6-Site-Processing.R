
################################################################################
#------------------------------------------------------------------------------#
#   PROCESS DATA (SITE)                                                        #                                                                              #
#------------------------------------------------------------------------------#
################################################################################



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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Work_Patients <- df_SDM_Patients %>%
                        select(-PatientID_Site) %>%      # Deselect unneeded PatientID_Site
                        left_join(df_SDM_Diagnosis, by = join_by(PatientID)) %>%
                        group_by(PatientID) %>%
                            mutate(CountDifferentDiagnoses = n_distinct(DiagnosisID),
                                   CountDifferentCancers = n_distinct(ICD10Code)) %>%      # Get Counts of different diagnoses and different cancer entities (by ICD-10-Code). These counts should be equal in majority of cases.
                        ungroup() %>%
                        left_join(df_SDM_Tumor, by = join_by(PatientID, DiagnosisID, ICD10Code))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df_WorkTumor <- df_Tumor %>%
#                     left_join(df_TNM, by = join_by(PatientID, TumorID, ICD10Code)) %>%
#                     group_by(TumorID) %>%
#                     arrange(DateTNMDocumentation) %>%
#                     mutate(IsTNMDocumentationAvailable = !is.na(TnmID),
#                            IndexStaging = ifelse(IsTNMDocumentationAvailable == TRUE, row_number(), NA),
#                            DateTumorFirstDiagnosis = ifelse(IsTNMDocumentationAvailable == TRUE, DateTNMDocumentation[IndexStaging == 1], NA),
#                            DiseaseEvent = case_when(IndexStaging == 1 ~ "Initial Staging",
#                                                     IndexStaging > 1 ~ "Re-Staging",
#                                                     TRUE ~ NA),
#                            DiseaseEventDetail = case_when()
#
#
# df_WorkDiseaseEvents <- df_Tumor %>%
#                             group_by(TumorID)






df_Work_Tumor <- df_SDM_Tumor %>%
                      left_join(df_SDM_Histology, by = join_by(PatientID, DiagnosisID)) %>%
                      group_by(across(!c(HistologyID, ICDO_Morphology, Grading))) %>%      # Group all columns but HistologyID, ICDO_Morphology and Grading ...
                          mutate(HistologiesPerTumor = n(),      # ... to obtain number of Histology Reports per Tumor, regardless of differences in ICDO-Morphology-Code
                                 DifferentHistologiesPerTumor = n_distinct(ICDO_Morphology, Grading)) %>%      # ... and number of DIFFERENT ICDO-Morphology-Codes or Grading per Tumor
                          arrange(HistologyID) %>%      # Sort by HistologyID, presuming that reports with higher ID are more recent
                          filter(row_number() == n()) %>%      # Keep only the last row of each tumor (should be the most recent Histology report)
                      ungroup() %>%
                      left_join(df_SDM_Staging, by = join_by(PatientID, TumorID, ICD10Code))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual Plausibility Check: Random patient selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_RandomTumor <- df_WorkTumor %>%
                      filter(TumorID == sample(df_WorkTumor$TumorID, size = 1))

View(df_RandomTumor)



