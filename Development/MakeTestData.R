
library(dplyr)
library(stringr)

#rm(list = ls())

load(file = "./Development/Data/RealData/SDM_CCPFrankfurt.RData")


CCPDataModel <- CCPhos::XMLToDataModel(XMLSchemaFilePath = "./Development/MetaData/SchemaOpalDB.xml")
TestDB <- CCPhos::DataModelToDB(DataModel = CCPDataModel,
                                DBPath = ":memory:")

DBI::dbListTables(TestDB)

# [1] "diagnosis"         "histology"         "metastasis"        "molecular-marker"  "patient"           "progress"
# [7] "radiation-therapy" "sample"            "surgery"           "system-therapy"    "tnm"



df_Patient <- DBI::dbReadTable(TestDB, name = "patient", check.names = FALSE)

df_Patient <- df_Patient %>%
                    bind_rows(data.frame("patient-id" = df_SDM_Patient$PatientID,
                                         "dktk-id-lokal" = df_SDM_Patient$PatientID_Site,
                                         geburtsdatum = as.Date(df_SDM_Patient$YearOfBirth, format = "%d.%m.%Y"),
                                         geschlecht = df_SDM_Patient$Sex,
                                         datum_des_letztbekannten_vitalstatus = as.Date(paste0("01.", df_SDM_Patient$DateLastVitalStatus), format = "%d.%m.%Y"),
                                         vitalstatus = df_SDM_Patient$LastVitalStatus,
                                         check.names = FALSE))



df_Diagnosis <- DBI::dbReadTable(TestDB, name = "diagnosis", check.names = FALSE)

df_SDM_Tumor <- df_SDM_Tumor %>%
                        left_join(df_SDM_Diagnosis, by = join_by(PatientID, DiagnosisID))

df_Diagnosis <- df_Diagnosis %>%
                    bind_rows(data.frame("diagnosis-id" = df_SDM_Tumor$TumorID,
                                         "patient-id" = df_SDM_Tumor$PatientID,
                                         primaerdiagnose = df_SDM_Tumor$ICD10Code.y,
                                         tumor_diagnosedatum = as.Date(paste(df_SDM_Tumor$YearCancerDiagnosis, 6, 15, sep = "-")),
                                         "version_des_icd-10_katalogs" = as.integer(str_extract(df_SDM_Tumor$ICDVersion, "\\d+")),
                                         lokalisation = df_SDM_Tumor$ICDO_Topography,
                                         "icd-o_katalog_topographie_version" = df_SDM_Tumor$ICDO_VersionTopography,
                                         "seitenlokalisation_nach_adt-gekid" = df_SDM_Tumor$LocalizationSide,
                                         check.names = FALSE))



df_Histology <- DBI::dbReadTable(TestDB, name = "histology", check.names = FALSE)

df_Histology <- df_Histology %>%
                    bind_rows(data.frame("histology-id" = as.character(df_SDM_Histology$HistologyID),
                                         "diagnosis-id" = df_SDM_Histology$TumorID,
                                         "patient-id" = df_SDM_Histology$PatientID,
                                         "icd-o_katalog_morphologie_version" = df_SDM_Histology$ICDO_VersionMorphology,
                                         morphologie = df_SDM_Histology$ICDO_Morphology,
                                         grading = df_SDM_Histology$Grading,
                                         check.names = FALSE))



df_Metastasis <- DBI::dbReadTable(TestDB, name = "metastasis", check.names = FALSE)

df_Metastasis <- df_Metastasis %>%
                    bind_rows(data.frame("metastasis-id" = df_SDM_Metastasis$MetastasisID,
                                         "diagnosis-id" = df_SDM_Metastasis$TumorID,
                                         "patient-id" = df_SDM_Metastasis$PatientID,
                                         datum_fernmetastasen = as.Date(df_SDM_Metastasis$DateMetastasisDiagnosis, format = "%d.%m.%Y"),
                                         fernmetastasen_vorhanden = case_match(df_SDM_Metastasis$IsDistantMetastasis,
                                                                                "ja" ~ TRUE,
                                                                                "nein" ~ FALSE),
                                         lokalisation_fernmetastasen = df_SDM_Metastasis$DistantMetastasisLocalization,
                                         check.names = FALSE))



df_MolecularDiagnostics <- DBI::dbReadTable(TestDB, name = "molecular-marker", check.names = FALSE)

df_MolecularDiagnostics <- df_MolecularDiagnostics %>%
                                bind_rows(data.frame("mol-marker-id" = df_SDM_MolecularDiagnostics$MolecularDiagnosticsID,
                                                     "diagnosis-id" = df_SDM_MolecularDiagnostics$TumorID,
                                                     "patient-id" = df_SDM_MolecularDiagnostics$PatientID,
                                                     marker_datum = as.Date(df_SDM_MolecularDiagnostics$DateMolecularDiagnostics, format = "%d.%m.%Y"),
                                                     marker_name = df_SDM_MolecularDiagnostics$MolecularMarker,
                                                     marker_status = df_SDM_MolecularDiagnostics$MolecularMarkerStatus,
                                                     zusaetzliche_alternative_dokumentation = df_SDM_MolecularDiagnostics$MolecularMarkerComment,
                                                     check.names = FALSE))



df_Progress <- DBI::dbReadTable(TestDB, name = "progress", check.names = FALSE)

df_Progress <- df_Progress %>%
                    bind_rows(data.frame("progress-id" = df_SDM_Progress$ProgressID,
                                         "diagnosis-id" = df_SDM_Progress$TumorID,
                                         "patient-id" = df_SDM_Progress$PatientID,
                                         "untersuchungs-_befunddatum_im_verlauf" = as.Date(df_SDM_Progress$DateOfProgress, format = "%d.%m.%Y"),
                                         gesamtbeurteilung_tumorstatus = df_SDM_Progress$TherapeuticResponse,
                                         lokales_oder_regionaeres_rezidiv = df_SDM_Progress$LocalRelapse,
                                         "lymphknoten-rezidiv" = df_SDM_Progress$LymphnodalRelapse,
                                         fernmetastasen = df_SDM_Progress$DistantMetastasis,
                                         check.names = FALSE))



df_RadiationTherapy <- DBI::dbReadTable(TestDB, name = "radiation-therapy", check.names = FALSE)

df_SDM_Radiotherapy <- df_SDM_Progress %>%
                            select(ProgressID,
                                   TumorID,
                                   DateOfProgress,
                                   RadiotherapyIntention,
                                   RadiotherapyRelationToSurgery) %>%
                            right_join(df_SDM_Radiotherapy, by = join_by(ProgressID))

df_RadiationTherapy <- df_RadiationTherapy %>%
                            bind_rows(data.frame("radiation-therapy-id" = df_SDM_Radiotherapy$RadiotherapyID,
                                                 "diagnosis-id" = df_SDM_Radiotherapy$TumorID,
                                                 "patient-id" = df_SDM_Radiotherapy$PatientID,
                                                 strahlentherapie_stellung_zu_operativer_therapie = df_SDM_Radiotherapy$RadiotherapyRelationToSurgery,
                                                 intention_strahlentherapie = df_SDM_Radiotherapy$RadiotherapyIntention,
                                                 strahlentherapie_beginn = as.Date(df_SDM_Radiotherapy$DateRadiotherapyStart, format = "%d.%m.%Y"),
                                                 strahlentherapie_ende = as.Date(df_SDM_Radiotherapy$DateRadiotherapyEnd, format = "%d.%m.%Y"),
                                                 check.names = FALSE))



df_BioSampling <- DBI::dbReadTable(TestDB, name = "sample", check.names = FALSE)

df_BioSampling <- df_BioSampling %>%
                        bind_rows(data.frame("sample-id" = df_SDM_BioSampling$SampleID,
                                             "patient-id" = df_SDM_BioSampling$PatientID,
                                             entnahmedatum = as.Date(df_SDM_BioSampling$DateSampleTaking, format = "%d.%m.%Y"),
                                             probentyp = df_SDM_BioSampling$SampleType,
                                             check.names = FALSE))



df_Surgery <- DBI::dbReadTable(TestDB, name = "surgery", check.names = FALSE)

df_SDM_Surgery <- df_SDM_Progress %>%
                      select(ProgressID,
                             DateOfProgress,
                             TumorID,
                             SurgeryIntention) %>%
                      right_join(df_SDM_Surgery, by = join_by(ProgressID))

df_Surgery <- df_Surgery %>%
                  bind_rows(data.frame("surgery-id" = as.character(df_SDM_Surgery$SurgeryID),
                                       "diagnosis-id" = df_SDM_Surgery$TumorID,
                                       "patient-id" = df_SDM_Surgery$PatientID,
                                       datum_der_op = as.Date(df_SDM_Surgery$DateOfProgress, format = "%d.%m.%Y"),
                                       intention_op = df_SDM_Surgery$SurgeryIntention,
                                       lokale_beurteilung_resttumor = df_SDM_Surgery$ResidualAssessmentLocal,
                                       gesamtbeurteilung_resttumor = df_SDM_Surgery$ResidualAssessmentTotal,
                                       check.names = FALSE))



df_SystemicTherapy <- DBI::dbReadTable(TestDB, name = "system-therapy", check.names = FALSE)

df_SDM_SystemicTherapy <- df_SDM_Progress %>%
                              select(ProgressID,
                                     DateOfProgress,
                                     TumorID,
                                     ChemotherapyIntention,
                                     ChemotherapyRelationToSurgery,
                                     IsChemotherapy,
                                     IsImmunotherapy,
                                     IsHormonetherapy,
                                     IsBonemarrowtransplant) %>%
                              right_join(df_SDM_SystemicTherapy, by = join_by(ProgressID))

df_SystemicTherapy <- df_SystemicTherapy %>%
                          bind_rows(data.frame("system-therapy-id" = df_SDM_SystemicTherapy$SystemicTherapyID,
                                               "diagnosis-id" = df_SDM_SystemicTherapy$TumorID,
                                               "patient-id" = df_SDM_SystemicTherapy$PatientID,
                                               systemische_therapie_stellung_zu_operativer_therapie = df_SDM_SystemicTherapy$ChemotherapyRelationToSurgery,
                                               intention_chemotherapie = df_SDM_SystemicTherapy$ChemotherapyIntention,
                                               systemische_therapie_beginn = as.Date(df_SDM_SystemicTherapy$DateSystemicTherapyStart, format = "%d.%m.%Y"),
                                               systemische_therapie_ende = as.Date(df_SDM_SystemicTherapy$DateSystemicTherapyEnd, format = "%d.%m.%Y"),
                                               systemische_therapie_protokoll = df_SDM_SystemicTherapy$SystemicTherapyProtocol,
                                               systemische_therapie_substanzen = df_SDM_SystemicTherapy$SystemicTherapySubstances,
                                               chemotherapie = as.logical(df_SDM_SystemicTherapy$IsChemotherapy),
                                               hormontherapie = as.logical(df_SDM_SystemicTherapy$IsHormonetherapy),
                                               immuntherapie = as.logical(df_SDM_SystemicTherapy$IsImmunotherapy),
                                               knochenmarktransplantation = as.logical(df_SDM_SystemicTherapy$IsBonemarrowtransplant),
                                               check.names = FALSE))



df_Staging <- DBI::dbReadTable(TestDB, name = "tnm", check.names = FALSE)

df_Staging <- df_Staging %>%
                  bind_rows(data.frame("tnm-id" = df_SDM_Staging$TnmID,
                                       "diagnosis-id" = df_SDM_Staging$TumorID,
                                       "patient-id" = df_SDM_Staging$PatientID,
                                       datum_der_tnm_dokumentation_datum_befund = as.Date(df_SDM_Staging$DateTNMDocumentation, format = "%d.%m.%Y"),
                                       uicc_stadium = df_SDM_Staging$UICCStage,
                                       "tnm-t" = df_SDM_Staging$TNM_T,
                                       "tnm-n" = df_SDM_Staging$TNM_N,
                                       "tnm-m" = df_SDM_Staging$TNM_M,
                                       c_p_u_preefix_t = df_SDM_Staging$TNM_T_Prefix,
                                       c_p_u_preefix_n = df_SDM_Staging$TNM_N_Prefix,
                                       c_p_u_preefix_m = df_SDM_Staging$TNM_M_Prefix,
                                       "tnm-y-symbol" = df_SDM_Staging$TNM_ySymbol,
                                       "tnm-r-symbol" = df_SDM_Staging$TNM_rSymbol,
                                       "tnm-m-symbol" = df_SDM_Staging$TNM_mSymbol,
                                       "tnm-version" = df_SDM_Staging$TNMVersion,
                                       check.names = FALSE))


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


vc_PatientIDs_A <- sample(df_Patient$"patient-id", 3000)
df_Patient_Rest <- filter(df_Patient, !(df_Patient$"patient-id" %in% vc_PatientIDs_A))
vc_PatientIDs_B <- sample(df_Patient_Rest$"patient-id", 3000)
df_Patient_Rest <- filter(df_Patient_Rest, !(df_Patient_Rest$"patient-id" %in% vc_PatientIDs_B))
vc_PatientIDs_C <- sample(df_Patient_Rest$"patient-id", 3000)



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



save(CCPTestData_Total, file = "./Development/Data/RealData/CCPTestData_Total.Rdata")
save(CCPTestData_A, file = "./Development/Data/RealData/CCPTestData_A.Rdata")
save(CCPTestData_B, file = "./Development/Data/RealData/CCPTestData_B.Rdata")
save(CCPTestData_C, file = "./Development/Data/RealData/CCPTestData_C.Rdata")


