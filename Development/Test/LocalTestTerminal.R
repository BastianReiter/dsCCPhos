

library(dsCCPhos)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CCP test data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RawDataSet <- readRDS(file = "./Development/Data/RealData/CCPRealData_Frankfurt.rds")
#OldTestData <- readRDS(file = "./Development/Data/TestData/OldTestData/CCPTestData.rds")
RawDataSet <- readRDS(file = "./Development/Data/TestData/CCPTestData.rds")
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



# - - OPTIONAL START - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Sub-sample test data for easier testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NumberOfPatients <- 1000

# Get a random sample of PatientIDs
SamplePatientIDs <- sample(RawDataSet$patient$"_id",
                           size = NumberOfPatients)

# Get data subsets that relate to sampled PatientIDs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Special tables 'sample' and 'molecularmarker' might be empty
sample <- data.frame()
if (nrow(RawDataSet$sample) > 0)
{
    sample <- as.data.frame(filter(RawDataSet$sample, RawDataSet$sample$"patient-id" %in% SamplePatientIDs))
}

molecularmarker <- data.frame()
if (nrow(RawDataSet$`molecular-marker`) > 0)
{
    molecularmarker <- as.data.frame(filter(RawDataSet$"molecular-marker", RawDataSet$"molecular-marker"$"patient-id" %in% SamplePatientIDs))
}

# Compile list 'RawDataSet'
#~~~~~~~~~~~~~~~~~~~~~~~~~~
RawDataSet <- list(sample = sample,
                   diagnosis = as.data.frame(filter(RawDataSet$diagnosis, RawDataSet$diagnosis$"patient-id" %in% SamplePatientIDs)),
                   histology = as.data.frame(filter(RawDataSet$histology, RawDataSet$histology$"patient-id" %in% SamplePatientIDs)),
                   metastasis = as.data.frame(filter(RawDataSet$metastasis, RawDataSet$metastasis$"patient-id" %in% SamplePatientIDs)),
                   "molecular-marker" = molecularmarker,
                   patient = as.data.frame(filter(RawDataSet$patient, RawDataSet$patient$"_id" %in% SamplePatientIDs)),
                   progress = as.data.frame(filter(RawDataSet$progress, RawDataSet$progress$"patient-id" %in% SamplePatientIDs)),
                   "radiation-therapy" = as.data.frame(filter(RawDataSet$"radiation-therapy", RawDataSet$"radiation-therapy"$"patient-id" %in% SamplePatientIDs)),
                   tnm = as.data.frame(filter(RawDataSet$tnm, RawDataSet$tnm$"patient-id" %in% SamplePatientIDs)),
                   surgery = as.data.frame(filter(RawDataSet$surgery, RawDataSet$surgery$"patient-id" %in% SamplePatientIDs)),
                   "system-therapy" = as.data.frame(filter(RawDataSet$"system-therapy", RawDataSet$"system-therapy"$"patient-id" %in% SamplePatientIDs)))

# - - OPTIONAL END - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename tables of RawDataSet (the names are also changed when tables are being loaded into R server sessions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vc_Lookup <- paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated)
names(vc_Lookup) <- dsCCPhos::Meta_Tables$TableName_Raw

names(RawDataSet) <- sapply(names(RawDataSet),
                            function(TableName) { vc_Lookup[TableName] })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "RawDataSet",
                                   AssumeCCPDataSet.S = TRUE)
                                   # RequiredTableNames.S = paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated),
                                   # RequiredFeatureNames.S = RequiredTableNames.S %>%
                                   #                              map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
                                   #                              set_names(RequiredTableNames.S))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RDSValidationReport <- GetRDSValidationReportDS("RawDataSet")

# summary(RDSValidationReport$RDS_BioSampling)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPTIONAL: Draw sample from Raw Data Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RawDataSet <- DrawSampleDS(RawDataSetName.S = "RawDataSet",
#                            SampleSize.S = "1000")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- CurateDataDS(RawDataSetName.S = "RawDataSet",
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


CuratedDataSet <- CurationOutput$CuratedDataSet

CDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "CuratedDataSet",
                                AssumeCCPDataSet.S = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AugmentationOutput <- AugmentDataDS(CuratedDataSetName.S = "CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS")



ADS_Patient <- ADS$Patient
ADS_Diagnosis <- ADS$Diagnosis


ADS_Patient <- ADS_Patient %>%
                    filter(CountDiagnoses == 1)


Analysis <- JoinTablesDS(TableNameA.S = "ADS_Patient",
                         TableNameB.S = "ADS_Diagnosis",
                         ByStatement.S = "PatientID")

#
#
# SampleStatistics <- GetSampleStatisticsDS(TableName.S = "ADS$Patients",
#                                           FeatureName.S = "LastVitalStatus")
# SampleStatistics$Statistics





Test <- Analysis %>%
            filter(str_starts(ICD10Code, "C34") == TRUE)



str_starts(Analysis$ICD10Code, "C50")








