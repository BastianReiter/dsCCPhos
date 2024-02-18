

library(dsCCPhos)


# Load CCP test data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RawDataSet <- readRDS(file = "./Development/Data/TestData/CCPTestData.rds")


# Make Test data set smaller for easier testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NumberOfPatients <- 1000

# Get a random sample of PatientIDs
SamplePatientIDs <- sample(RawDataSet$patient$"_id",
                           size = NumberOfPatients)

# Get data subsets that relate to sampled PatientIDs
RawDataSet <- list(sample = as.data.frame(filter(RawDataSet$sample, RawDataSet$sample$"patient-id" %in% SamplePatientIDs)),
                   diagnosis = as.data.frame(filter(RawDataSet$diagnosis, RawDataSet$diagnosis$"patient-id" %in% SamplePatientIDs)),
                   histology = as.data.frame(filter(RawDataSet$histology, RawDataSet$histology$"patient-id" %in% SamplePatientIDs)),
                   metastasis = as.data.frame(filter(RawDataSet$metastasis, RawDataSet$metastasis$"patient-id" %in% SamplePatientIDs)),
                   "molecular-marker" = as.data.frame(filter(RawDataSet$"molecular-marker", RawDataSet$"molecular-marker"$"patient-id" %in% SamplePatientIDs)),
                   patient = as.data.frame(filter(RawDataSet$patient, RawDataSet$patient$"_id" %in% SamplePatientIDs)),
                   progress = as.data.frame(filter(RawDataSet$progress, RawDataSet$progress$"patient-id" %in% SamplePatientIDs)),
                   "radiation-therapy" = as.data.frame(filter(RawDataSet$"radiation-therapy", RawDataSet$"radiation-therapy"$"patient-id" %in% SamplePatientIDs)),
                   tnm = as.data.frame(filter(RawDataSet$tnm, RawDataSet$tnm$"patient-id" %in% SamplePatientIDs)),
                   surgery = as.data.frame(filter(RawDataSet$surgery, RawDataSet$surgery$"patient-id" %in% SamplePatientIDs)),
                   "system-therapy" = as.data.frame(filter(RawDataSet$"system-therapy", RawDataSet$"system-therapy"$"patient-id" %in% SamplePatientIDs)))

# Rename tables of RawDataSet (the names are also changed when tables are being loaded into R server sessions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vc_Lookup <- paste0("RDS_", dsCCPhos::Meta_TableNames$TableName_Curated)
names(vc_Lookup) <- dsCCPhos::Meta_TableNames$TableName_Raw

names(RawDataSet) <- sapply(names(RawDataSet),
                            function(TableName) { vc_Lookup[TableName] })


# Set CurateDataDS argument values manually if needed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RuleProfile_RawDataTransformation = "Default"
RuleProfile_DiagnosisRedundancy = "Default"
RuleProfile_DiagnosisAssociation = "Default"


# Curate data
CurationOutput <- dsCCPhos::CurateDataDS(Name_RawDataSet = "RawDataSet",
                                         RuleProfile_RawDataTransformation = "Default",
                                         RuleProfile_DiagnosisRedundancy = "Default",
                                         RuleProfile_DiagnosisAssociation = "Default")

View(CurationOutput$CurationReport$Monitor_Diagnosis)

View(CurationOutput$CuratedDataSet$Diagnosis)

# Save curated data set for testing purposes
# save(CurationOutput, file = "./Development/Data/TestData/CCPCurationOutput.Rdata")

# Augment data based on curation output
AugmentationOutput <- dsCCPhos::AugmentDataDS("CurationOutput")

View(AugmentationOutput$Patients)





