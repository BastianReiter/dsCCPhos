
library(dplyr)
library(readr)


df_BioSampling <- read_csv(file = "./Development/Data/TestData/CSV/sample.csv",
                           col_types = cols(entnahmedatum = col_date(),
                                            status = col_character(),
                                            projektbezeichnung = col_character(),
                                            aliquot = col_character())) %>%
                      rename("sample-id" = entity_id)

df_Diagnosis <- read_csv(file = "./Development/Data/TestData/CSV/diagnosis.csv",
                         col_types = cols("seitenlokalisation_nach_adt-gekid" = col_character())) %>%
                    rename("diagnosis-id" = entity_id)

df_Histology <- read_csv(file = "./Development/Data/TestData/CSV/histology.csv",
                         col_types = cols(grading = col_character())) %>%
                    rename("histology-id" = entity_id)

df_Metastasis <- read_csv(file = "./Development/Data/TestData/CSV/metastasis.csv") %>%
                      rename("metastasis-id" = entity_id)

df_MolecularDiagnostics <- read_csv(file = "./Development/Data/TestData/CSV/molecular-marker.csv",
                                    col_types = cols(zusaetzliche_alternative_dokumentation = col_character())) %>%
                              rename("mol-marker-id" = entity_id)

df_Patient <- read_csv(file = "./Development/Data/TestData/CSV/patient.csv",
                       col_types = cols("dktk-id-global" = col_character(),
                                        "dktk-id-lokal" = col_character(),
                                        todesursachen = col_character())) %>%
                  rename("patient-id" = entity_id)

df_Progress <- read_csv(file = "./Development/Data/TestData/CSV/progress.csv") %>%
                    rename("progress-id" = entity_id)

df_RadiationTherapy <- read_csv(file = "./Development/Data/TestData/CSV/radiation-therapy.csv",
                                col_types = cols(strahlentherapie_stellung_zu_operativer_therapie = col_character(),
                                                 intention_strahlentherapie = col_character())) %>%
                            rename("radiation-therapy-id" = entity_id)

df_Staging <- read_csv(file = "./Development/Data/TestData/CSV/tnm.csv") %>%
                  rename("tnm-id" = entity_id)

df_Surgery <- read_csv(file = "./Development/Data/TestData/CSV/surgery.csv",
                       col_types = cols("ops-code" = col_character(),
                                        "OPS-Version" = col_character())) %>%
                  rename("surgery-id" = entity_id)

df_SystemicTherapy <- read_csv(file = "./Development/Data/TestData/CSV/system-therapy.csv")




CCPTestData_Total <- list(BioSampling = df_BioSampling,
                          Diagnosis = df_Diagnosis,
                          Histology = df_Histology,
                          Metastasis = df_Metastasis,
                          MolecularDiagnostics = df_MolecularDiagnostics,
                          Patient = df_Patient,
                          Progress = df_Progress,
                          RadiationTherapy = df_RadiationTherapy,
                          Staging = df_Staging,
                          Surgery = df_Surgery,
                          SystemicTherapy = df_SystemicTherapy)


PatientsPerSite <- floor(nrow(df_Patient) / 4)

vc_PatientIDs_A <- sample(df_Patient$"patient-id", PatientsPerSite)
df_Patient_Rest <- filter(df_Patient, !(df_Patient$"patient-id" %in% vc_PatientIDs_A))
vc_PatientIDs_B <- sample(df_Patient_Rest$"patient-id", PatientsPerSite)
df_Patient_Rest <- filter(df_Patient_Rest, !(df_Patient_Rest$"patient-id" %in% vc_PatientIDs_B))
vc_PatientIDs_C <- sample(df_Patient_Rest$"patient-id", PatientsPerSite)
df_Patient_Rest <- filter(df_Patient_Rest, !(df_Patient_Rest$"patient-id" %in% vc_PatientIDs_C))
vc_PatientIDs_D <- sample(df_Patient_Rest$"patient-id", PatientsPerSite)



CCPTestData_A <- list(BioSampling = filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_A),
                      Diagnosis = filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_A),
                      Histology = filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_A),
                      Metastasis = filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_A),
                      MolecularDiagnostics = filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_A),
                      Patient = filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_A),
                      Progress = filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_A),
                      RadiationTherapy = filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_A),
                      Staging = filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_A),
                      Surgery = filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_A),
                      SystemicTherapy = filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_A))


CCPTestData_B <- list(BioSampling = filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_B),
                      Diagnosis = filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_B),
                      Histology = filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_B),
                      Metastasis = filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_B),
                      MolecularDiagnostics = filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_B),
                      Patient = filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_B),
                      Progress = filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_B),
                      RadiationTherapy = filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_B),
                      Staging = filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_B),
                      Surgery = filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_B),
                      SystemicTherapy = filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_B))


CCPTestData_C <- list(BioSampling = filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_C),
                      Diagnosis = filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_C),
                      Histology = filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_C),
                      Metastasis = filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_C),
                      MolecularDiagnostics = filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_C),
                      Patient = filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_C),
                      Progress = filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_C),
                      RadiationTherapy = filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_C),
                      Staging = filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_C),
                      Surgery = filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_C),
                      SystemicTherapy = filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_C))


CCPTestData_D <- list(BioSampling = filter(df_BioSampling, df_BioSampling$"patient-id" %in% vc_PatientIDs_D),
                      Diagnosis = filter(df_Diagnosis, df_Diagnosis$"patient-id" %in% vc_PatientIDs_D),
                      Histology = filter(df_Histology, df_Histology$"patient-id" %in% vc_PatientIDs_D),
                      Metastasis = filter(df_Metastasis, df_Metastasis$"patient-id" %in% vc_PatientIDs_D),
                      MolecularDiagnostics = filter(df_MolecularDiagnostics, df_MolecularDiagnostics$"patient-id" %in% vc_PatientIDs_D),
                      Patient = filter(df_Patient, df_Patient$"patient-id" %in% vc_PatientIDs_D),
                      Progress = filter(df_Progress, df_Progress$"patient-id" %in% vc_PatientIDs_D),
                      RadiationTherapy = filter(df_RadiationTherapy, df_RadiationTherapy$"patient-id" %in% vc_PatientIDs_D),
                      Staging = filter(df_Staging, df_Staging$"patient-id" %in% vc_PatientIDs_D),
                      Surgery = filter(df_Surgery, df_Surgery$"patient-id" %in% vc_PatientIDs_D),
                      SystemicTherapy = filter(df_SystemicTherapy, df_SystemicTherapy$"patient-id" %in% vc_PatientIDs_D))



save(CCPTestData_Total, file = "./Development/Data/TestData/CCPTestData_Total.Rdata")
save(CCPTestData_A, file = "./Development/Data/TestData/CCPTestData_A.Rdata")
save(CCPTestData_B, file = "./Development/Data/TestData/CCPTestData_B.Rdata")
save(CCPTestData_C, file = "./Development/Data/TestData/CCPTestData_C.Rdata")
save(CCPTestData_D, file = "./Development/Data/TestData/CCPTestData_D.Rdata")



