

library(dsCCPhos)

# Load test data into WD
load("./Development/Data/RealData/CCPTestData_Total.RData")

# Create virtual data base with test data
DBConnection <- MakeTestDB(CCPTestData_Total)

# Load raw test data from data base into WD
RawDataSet <- LoadRawData(DBConnection)

RuleProfile_DiagnosisRedundancy = "Default"
RuleProfile_DiagnosisAssociation = "Default"

# Curate data
CurationOutput <- dsCCPhos::CurateDataDS(Name_RawDataSet = "RawDataSet",
                                         RuleProfile_DiagnosisAssociation = "Default",
                                         RuleProfile_DiagnosisRedundancy = "Default")

# Save curated data set for testing purposes
#save(CurationOutput, file = "./Development/Data/RealData/CCPCurationOutput.Rdata")

# Augment data based on curation output
AugmentationOutput <- dsCCPhos::AugmentDataDS("CurationOutput")

View(AugmentationOutput$ADS_Events)






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
