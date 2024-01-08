

library(dsCCPhos)

# Load test data into WD
load("./Development/Data/RealData/CCPTestData_Total.RData")

# Create virtual data base with test data
DBConnection <- MakeTestDB(CCPTestData_Total)

# Load raw test data from data base into WD
RawData <- LoadRawData(DBConnection)

# Curate data
CuratedData <- dsCCPhos::CurateDataDS("RawData")$CuratedData

# Save curated data set for test purposes
#save(CuratedData, file = "./Development/Data/RealData/CCPCuratedData.Rdata")


#TestConn <- dsCCPhos::ConnectToSites()




