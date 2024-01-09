

library(dsCCPhos)

# Load test data into WD
load("./Development/Data/RealData/CCPTestData_Total.RData")

# Create virtual data base with test data
DBConnection <- MakeTestDB(CCPTestData_Total)

# Load raw test data from data base into WD
RawData <- LoadRawData(DBConnection)

# Curate data
CurationOutput <- dsCCPhos::CurateDataDS("RawData")

# Save curated data set for test purposes
#save(CuratedData, file = "./Development/Data/RealData/CCPCuratedData.Rdata")

# Augment data based on curation output
AugmentationOutput <- dsCCPhos::AugmentDataDS("CurationOutput")

View(AugmentationOutput$ADM_Events)



