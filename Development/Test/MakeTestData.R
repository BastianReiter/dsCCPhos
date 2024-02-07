
library(dplyr)
library(readr)
library(stringr)



df_BioSampling <- read_csv(file = "./Development/Data/TestData/CSV/sample/data.csv",
                           col_types = cols(.default = "c")) %>%
                      rename("sample-id" = entity_id)

df_Diagnosis <- read_csv(file = "./Development/Data/TestData/CSV/diagnosis/data.csv",
                         col_types = cols(.default = "c")) %>%
                    rename("diagnosis-id" = entity_id)

df_Histology <- read_csv(file = "./Development/Data/TestData/CSV/histology/data.csv",
                         col_types = cols(.default = "c")) %>%
                    rename("histology-id" = entity_id)

df_Metastasis <- read_csv(file = "./Development/Data/TestData/CSV/metastasis/data.csv",
                          col_types = cols(.default = "c")) %>%
                      rename("metastasis-id" = entity_id)

df_MolecularDiagnostics <- read_csv(file = "./Development/Data/TestData/CSV/molecular-marker/data.csv",
                                    col_types = cols(.default = "c")) %>%
                              rename("mol-marker-id" = entity_id)

df_Patient <- read_csv(file = "./Development/Data/TestData/CSV/patient/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("patient-id" = entity_id)

df_Progress <- read_csv(file = "./Development/Data/TestData/CSV/progress/data.csv",
                        col_types = cols(.default = "c")) %>%
                    rename("progress-id" = entity_id)

df_RadiationTherapy <- read_csv(file = "./Development/Data/TestData/CSV/radiation-therapy/data.csv",
                                col_types = cols(.default = "c")) %>%
                            rename("radiation-therapy-id" = entity_id)

df_Staging <- read_csv(file = "./Development/Data/TestData/CSV/tnm/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("tnm-id" = entity_id)

df_Surgery <- read_csv(file = "./Development/Data/TestData/CSV/surgery/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("surgery-id" = entity_id)

df_SystemicTherapy <- read_csv(file = "./Development/Data/TestData/CSV/system-therapy/data.csv",
                               col_types = cols(.default = "c")) %>%
                          rename("systemic-therapy-id" = entity_id)


CCPTestData_Total <- list(BioSampling = as.data.frame(df_BioSampling),
                          Diagnosis = as.data.frame(df_Diagnosis),
                          Histology = as.data.frame(df_Histology),
                          Metastasis = as.data.frame(df_Metastasis),
                          MolecularDiagnostics = as.data.frame(df_MolecularDiagnostics),
                          Patient = as.data.frame(df_Patient),
                          Progress = as.data.frame(df_Progress),
                          RadiationTherapy = as.data.frame(df_RadiationTherapy),
                          Staging = as.data.frame(df_Staging),
                          Surgery = as.data.frame(df_Surgery),
                          SystemicTherapy = as.data.frame(df_SystemicTherapy))


PatientsPerSite <- floor(nrow(df_Patient) / 3)

vc_PatientIDs_A <- sample(df_Patient$"patient-id", PatientsPerSite)
df_Patient_Rest <- filter(df_Patient, !(df_Patient$"patient-id" %in% vc_PatientIDs_A))
vc_PatientIDs_B <- sample(df_Patient_Rest$"patient-id", PatientsPerSite)
df_Patient_Rest <- filter(df_Patient_Rest, !(df_Patient_Rest$"patient-id" %in% vc_PatientIDs_B))
vc_PatientIDs_C <- sample(df_Patient_Rest$"patient-id", PatientsPerSite)



CCPTestData_A <- list(BioSampling = as.data.frame(filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_A)),
                      Diagnosis = as.data.frame(filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_A)),
                      Histology = as.data.frame(filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_A)),
                      Metastasis = as.data.frame(filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_A)),
                      MolecularDiagnostics = as.data.frame(filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_A)),
                      Patient = as.data.frame(filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_A)),
                      Progress = as.data.frame(filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_A)),
                      RadiationTherapy = as.data.frame(filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_A)),
                      Staging = as.data.frame(filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_A)),
                      Surgery = as.data.frame(filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_A)),
                      SystemicTherapy = as.data.frame(filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_A)))


CCPTestData_B <- list(BioSampling = as.data.frame(filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_B)),
                      Diagnosis = as.data.frame(filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_B)),
                      Histology = as.data.frame(filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_B)),
                      Metastasis = as.data.frame(filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_B)),
                      MolecularDiagnostics = as.data.frame(filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_B)),
                      Patient = as.data.frame(filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_B)),
                      Progress = as.data.frame(filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_B)),
                      RadiationTherapy = as.data.frame(filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_B)),
                      Staging = as.data.frame(filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_B)),
                      Surgery = as.data.frame(filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_B)),
                      SystemicTherapy = as.data.frame(filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_B)))


CCPTestData_C <- list(BioSampling = as.data.frame(filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_C)),
                      Diagnosis = as.data.frame(filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_C)),
                      Histology = as.data.frame(filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_C)),
                      Metastasis = as.data.frame(filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_C)),
                      MolecularDiagnostics = as.data.frame(filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_C)),
                      Patient = as.data.frame(filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_C)),
                      Progress = as.data.frame(filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_C)),
                      RadiationTherapy = as.data.frame(filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_C)),
                      Staging = as.data.frame(filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_C)),
                      Surgery = as.data.frame(filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_C)),
                      SystemicTherapy = as.data.frame(filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_C)))


save(CCPTestData_Total, file = "./Development/Data/TestData/CCPTestData_Total.Rdata")
save(CCPTestData_A, file = "./Development/Data/TestData/CCPTestData_A.Rdata")
save(CCPTestData_B, file = "./Development/Data/TestData/CCPTestData_B.Rdata")
save(CCPTestData_C, file = "./Development/Data/TestData/CCPTestData_C.Rdata")



