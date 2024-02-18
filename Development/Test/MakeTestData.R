
library(dplyr)
library(readr)
library(stringr)



df_BioSampling <- read_csv(file = "./Development/Data/TestData/CSV/sample/data.csv",
                           col_types = cols(.default = "c")) %>%
                      rename("_id" = entity_id)

df_Diagnosis <- read_csv(file = "./Development/Data/TestData/CSV/diagnosis/data.csv",
                         col_types = cols(.default = "c")) %>%
                    rename("_id" = entity_id)

df_Histology <- read_csv(file = "./Development/Data/TestData/CSV/histology/data.csv",
                         col_types = cols(.default = "c")) %>%
                    rename("_id" = entity_id)

df_Metastasis <- read_csv(file = "./Development/Data/TestData/CSV/metastasis/data.csv",
                          col_types = cols(.default = "c")) %>%
                      rename("_id" = entity_id)

df_MolecularDiagnostics <- read_csv(file = "./Development/Data/TestData/CSV/molecular-marker/data.csv",
                                    col_types = cols(.default = "c")) %>%
                              rename("_id" = entity_id)

df_Patient <- read_csv(file = "./Development/Data/TestData/CSV/patient/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("_id" = entity_id)

df_Progress <- read_csv(file = "./Development/Data/TestData/CSV/progress/data.csv",
                        col_types = cols(.default = "c")) %>%
                    rename("_id" = entity_id)

df_RadiationTherapy <- read_csv(file = "./Development/Data/TestData/CSV/radiation-therapy/data.csv",
                                col_types = cols(.default = "c")) %>%
                            rename("_id" = entity_id)

df_Staging <- read_csv(file = "./Development/Data/TestData/CSV/tnm/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("_id" = entity_id)

df_Surgery <- read_csv(file = "./Development/Data/TestData/CSV/surgery/data.csv",
                       col_types = cols(.default = "c")) %>%
                  rename("_id" = entity_id)

df_SystemicTherapy <- read_csv(file = "./Development/Data/TestData/CSV/system-therapy/data.csv",
                               col_types = cols(.default = "c")) %>%
                          rename("_id" = entity_id)


CCPTestData <- list(sample = as.data.frame(df_BioSampling),
                    diagnosis = as.data.frame(df_Diagnosis),
                    histology = as.data.frame(df_Histology),
                    metastasis = as.data.frame(df_Metastasis),
                    "molecular-marker" = as.data.frame(df_MolecularDiagnostics),
                    patient = as.data.frame(df_Patient),
                    progress = as.data.frame(df_Progress),
                    "radiation-therapy" = as.data.frame(df_RadiationTherapy),
                    tnm = as.data.frame(df_Staging),
                    surgery = as.data.frame(df_Surgery),
                    "system-therapy" = as.data.frame(df_SystemicTherapy))


saveRDS(CCPTestData, file = "./Development/Data/TestData/CCPTestData.rds")


