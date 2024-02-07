

library(dsCCPhos)

# Load test data into WD
load("./Development/Data/TestData/CCPTestData_B.RData")

# Create virtual data base with test data
DBConnection <- MakeTestDB(CCPTestData_B)

# Load raw test data from data base into WD
RawDataSet <- LoadRawData(DBConnection)

View(RawDataSet$MolecularDiagnostics)

RuleProfile_RawDataTransformation = "Default"
RuleProfile_DiagnosisRedundancy = "Default"
RuleProfile_DiagnosisAssociation = "Default"

# Curate data
CurationOutput <- dsCCPhos::CurateDataDS(Name_RawDataSet = "RawDataSet",
                                         RuleProfile_RawDataTransformation = "Default",
                                         RuleProfile_DiagnosisRedundancy = "Default",
                                         RuleProfile_DiagnosisAssociation = "Default")

View(CurationOutput$CurationReport$Monitor_Staging)

View(CurationOutput$CuratedDataSet$Diagnosis)

# Save curated data set for testing purposes
#save(CurationOutput, file = "./Development/Data/RealData/CCPCurationOutput.Rdata")

# Augment data based on curation output
AugmentationOutput <- dsCCPhos::AugmentDataDS("CurationOutput")

View(AugmentationOutput$ADS_Events)

View(tidyr::unnest(AugmentationOutput$ADS_Events, cols = c(EventDetails)))

View(AugmentationOutput$ADS_Diagnosis)

View(AugmentationOutput$ADS_Patient)








# Missings <- purrr::map(CCPTestData_Total, function(df) {
#                                               MissingColumns <- names(df)[which(colSums(is.na(df)) == nrow(df))]
#
#
# })
#
#
# Missings <- unlist(Missings)
#
# names(Missings) <- NULL
