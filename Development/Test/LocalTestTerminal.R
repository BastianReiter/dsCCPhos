
library(dplyr)
library(dsFreda)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CCP test data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RawDataSet <- readRDS(file = "./Development/Data/RealData/CCPRealData_Frankfurt.rds")
#OldTestData <- readRDS(file = "./Development/Data/TestData/OldTestData/CCPTestData.rds")
CCP.RawDataSet <- readRDS(file = "./Development/Data/TestData/CCPTestData.rds")
#RawDataSet <- readRDS(file = "./Development/Data/TestData/CCPTestData_WithMissingTables.rds")


# names(RawDataSet) <- c("patient",
#                        "diagnosis",
#                        "progress",
#                        "histology",
#                        "metastasis",
#                        "tnm",
#                        "system-therapy",
#                        "surgery",
#                        "radiation-therapy",
#                        "molecular-marker",
#                        "sample")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename tables of RawDataSet (the names are also changed when tables are being loaded into R server sessions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vc_Lookup <- dsCCPhos::Meta.Tables$TableName.Curated
names(vc_Lookup) <- dsCCPhos::Meta.Tables$TableName.Raw

names(CCP.RawDataSet) <- sapply(names(CCP.RawDataSet),
                            function(TableName) { vc_Lookup[TableName] })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform preparatory operations prior to curation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSPreparation <- PrepareRawDataDS(RawDataSetName.S = "CCP.RawDataSet",
                                   Module.S = "CCP",
                                   CurateFeatureNames.S = TRUE)

CCP.RawDataSet <- RDSPreparation$RawDataSet


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- dsFreda::GetDataSetCheckDS(DataSetName.S = "CCP.RawDataSet",
                                            Module.S = "CCP",
                                            Stage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RDSValidationReport <- GetRDSValidationReportDS("CCP.RawDataSet")

# summary(RDSValidationReport$RDS_BioSampling)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Draw sample from Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CCP.RawDataSet <- DrawSampleDS(RawDataSetName.S = "CCP.RawDataSet",
#                                SampleSize.S = 1000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- CurateDataDS(RawDataSetName.S = "CCP.RawDataSet",
                               Settings.S = list(DataHarmonization = list(Run = TRUE,
                                                                          Profile = "Default"),
                                                 FeatureObligations = list(Profile = "Default"),
                                                 FeatureTracking = list(Profile = "Default"),
                                                 TableCleaning = list(Run = TRUE)))


# CurationOutput$CurationReport$EntryCounts
# View(CurationOutput$CurationReport$Transformation$Monitors$RadiationTherapy)
# View(CurationOutput$CurationReport$Transformation$Monitors$Staging)
# View(CurationOutput$CurationReport$Transformation$EligibilityOverviews$Staging)
# View(CurationOutput$CurationReport$Transformation$ValueSetOverviews$Staging$Harmonized)


CCP.CuratedDataSet <- CurationOutput$CuratedDataSet

CDSTableCheck <- dsFreda::GetDataSetCheckDS(DataSetName.S = "CCP.CuratedDataSet",
                                            Module.S = "CCP",
                                            Stage.S = "Curated")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AugmentationOutput <- AugmentDataDS(CuratedDataSetName.S = "CCP.CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS",
                                   Module.S = "CCP",
                                   Stage.S = "Augmented")



# saveRDS(ADS, file = "TestADS.rds")


ADS.Patient <- ADS$Patient
ADS.Diagnosis <- ADS$Diagnosis


ADS.Patient <- ADS_Patient %>%
                    filter(CountDiagnoses == 1)


Analysis <- JoinTablesDS(TableNameA.S = "ADS.Patient",
                         TableNameB.S = "ADS.Diagnosis",
                         ByStatement.S = "PatientID")

#
#
# SampleStatistics <- GetSampleStatisticsDS(TableName.S = "ADS$Patients",
#                                           FeatureName.S = "LastVitalStatus")
# SampleStatistics$Statistics





Test <- Analysis %>%
            filter(str_starts(ICD10Code, "C34") == TRUE)



str_starts(Analysis$ICD10Code, "C50")








