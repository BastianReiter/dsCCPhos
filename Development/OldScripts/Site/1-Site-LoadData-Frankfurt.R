

################################################################################
#------------------------------------------------------------------------------#
#   SEED DATA Frankfurt: LOADING                                               #
#------------------------------------------------------------------------------#
################################################################################


# Source:
# ~~~~~~~
#     CCP-Bridgehead Frankfurt (UCT)

# Export Method:
# ~~~~~~~~~~~~~~
#     SQL-Query --> XLSX-Export

# Exported files:
# ~~~~~~~~~~~~~~~
#     - Data_CCP_Frankfurt.xlsx


InputDataPath <- here::here("Data/SiteInputData/Frankfurt")

InputFileName <- "Data_CCP_Frankfurt.xlsx"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Seed Data Model (SDM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    - df_SDM_Patients
#    - df_SDM_Diagnosis
#    - df_SDM_Tumor
#    - df_SDM_Histology
#    - df_SDM_Staging
#    - df_SDM_Metastasis
#    - df_SDM_Progress
#    - df_SDM_Surgery
#    - df_SDM_SystemicTherapy
#    - df_SDM_Radiotherapy
#    - df_SDM_BioSampling
#    - df_SDM_MolecularDiagnostics

# ==> For details, see Project Documentation -> Seed Data Model



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Patients: Patient-specific data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Patients <- read_excel(path = here::here(InputDataPath, InputFileName),
                              sheet = "Patient",                                    
                              skip = 1,                                         # No Header
                              col_names = c("PatientID",                        # Column Names 
                                            "PatientID_Site",
                                            "DateFirstProgressDoc",
                                            "YearFirstCancerDiagnosis",
                                            "YearOfBirth",
                                            "DateLastVitalStatus",
                                            "Sex",
                                            "LastVitalStatus"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Diagnosis: Data on cancer diagnoses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Diagnosis <- read_excel(path = here::here(InputDataPath, InputFileName),
                               sheet = "Diagnosis",                                    
                               skip = 1,
                               col_names = c("DiagnosisID",
                                             "PatientID",
                                             "YearCancerDiagnosis",
                                             "AgeCancerDiagnosis",
                                             "ICD10Code",
                                             "ICDVersion"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Tumor: Data on ICD-O coding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Tumor <- read_excel(path = here::here(InputDataPath, InputFileName),
                           sheet = "Tumour",                                    
                           skip = 1,
                           col_names = c("TumorID",
                                         "DiagnosisID",
                                         "PatientID",
                                         "ICD10Code",
                                         "ICDO_Topography",
                                         "ICDO_VersionTopography",
                                         "LocalizationSide"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Histology: Data on cancer histology
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Histology <- read_excel(path = here::here(InputDataPath, InputFileName),
                               sheet = "Histology",                                    
                               skip = 1,
                               col_names = c("HistologyID",
                                             "TumorID",
                                             "PatientID",
                                             "ICD10Code",
                                             "ICDO_Morphology",
                                             "ICDO_VersionMorphology",
                                             "Grading"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Staging: Data on TNM Status
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Staging <- read_excel(path = here::here(InputDataPath, InputFileName),
                             sheet = "Tnm",                                    
                             skip = 1,
                             col_names = c("TnmID",
                                           "TumorID",
                                           "PatientID",
                                           "ICD10Code",
                                           "DateTNMDocumentation",
                                           "UICCStage",
                                           "TNM_T",
                                           "TNM_mSymbol",
                                           "TNM_N",
                                           "TNM_M",
                                           "TNM_T_Prefix",
                                           "TNM_N_Prefix",
                                           "TNM_M_Prefix",
                                           "TNM_ySymbol",
                                           "TNM_rSymbol",
                                           "TNMVersion"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Metastasis: Data on Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Metastasis <- read_excel(path = here::here(InputDataPath, InputFileName),
                                sheet = "Metastasis",                                    
                                skip = 1,
                                col_names = c("MetastasisID",
                                              "TumorID",
                                              "PatientID",
                                              "ICD10Code",
                                              "DateMetastasisDiagnosis",
                                              "IsDistantMetastasis",
                                              "DistantMetastasisLocalization"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Progress: Data on disease and therapy progress
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Progress <- read_excel(path = here::here(InputDataPath, InputFileName),
                              sheet = "Progress",                                    
                              skip = 1,
                              col_names = c("ProgressID",
                                            "TumorID",
                                            "PatientID",
                                            "ICD10Code",
                                            "DateOfProgress",
                                            "IsSurgery",
                                            "SurgeryIntention",
                                            "IsRadiotherapy",
                                            "RadiotherapyIntention",
                                            "RadiotherapyRelationToSurgery",
                                            "IsChemotherapy",
                                            "ChemotherapyIntention",
                                            "ChemotherapyRelationToSurgery",
                                            "IsImmunotherapy",
                                            "IsHormonetherapy",
                                            "IsBonemarrowtransplant",
                                            "TherapeuticResponse",
                                            "LocalRelapse",
                                            "LymphnodalRelapse",
                                            "DistantMetastasis",
                                            "IsSyntheticProgressEntry"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Surgery: Data on Surgery
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Surgery <- read_excel(path = here::here(InputDataPath, InputFileName),
                             sheet = "Surgery",                                    
                             skip = 1,
                             col_names = c("SurgeryID",
                                           "ProgressID",
                                           "PatientID",
                                           "ICD10Code",
                                           "ResidualAssessmentLocal",
                                           "ResidualAssessmentTotal"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_SystemicTherapy: Data on Systemic Therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_SystemicTherapy <- read_excel(path = here::here(InputDataPath, InputFileName),
                                     sheet = "SystemTherapy",                                    
                                     skip = 1,
                                     col_names = c("SystemicTherapyID",
                                                   "ProgressID",
                                                   "PatientID",
                                                   "ICD10Code",
                                                   "DateSystemicTherapyStart",
                                                   "DateSystemicTherapyEnd",
                                                   "SystemicTherapyProtocol",
                                                   "SystemicTherapySubstances"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_Radiotherapy: Data on Radiotherapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_Radiotherapy <- read_excel(path = here::here(InputDataPath, InputFileName),
                                  sheet = "RadiationTherapy",                                    
                                  skip = 1,
                                  col_names = c("RadiotherapyID",
                                                "ProgressID",
                                                "PatientID",
                                                "ICD10Code",
                                                "DateRadiotherapyStart",
                                                "DateRadiotherapyEnd"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_BioSampling: Data on Taking of Samples from Patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_BioSampling <- read_excel(path = here::here(InputDataPath, InputFileName),
                                 sheet = "Sample",                                    
                                 skip = 1,
                                 col_names = c("SampleID",
                                               "PatientID",
                                               "DateSampleTaking",
                                               "PatientWithSample",
                                               "SampleType",
                                               "SampleTissue",
                                               "FixationType"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_SDM_MolecularDiagnostics: Data on Molecular Diagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SDM_MolecularDiagnostics <- read_excel(path = here::here(InputDataPath, InputFileName),
                                          sheet = "MolecularMarker",                                    
                                          skip = 1,
                                          col_names = c("MolecularDiagnosticsID",
                                                        "TumorID",
                                                        "PatientID",
                                                        "ICD10Code",
                                                        "DateMolecularDiagnostics",
                                                        "MolecularMarker",
                                                        "MolecularMarkerStatus",
                                                        "MolecularMarkerComment"))


