
# Install own dataSHIELD packages
#devtools::install_github(repo = "BastianReiter/dsCCPhos")
#devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
#devtools::install_github(repo = "BastianReiter/TinkerLab")

#devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
#devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)

# Load needed packages
library(dsBase)
library(dsBaseClient)
library(dsCCPhos)
library(dsCCPhosClient)
library(DSLite)
library(resourcer)



load("./Development/Data/TestData/CCPTestData_A.RData")
load("./Development/Data/TestData/CCPTestData_B.RData")
load("./Development/Data/TestData/CCPTestData_C.RData")
load("./Development/Data/TestData/CCPTestData_D.RData")


load("./Development/Data/RealData/CCPTestData_Total.RData")
load("./Development/Data/RealData/CCPTestData_A.RData")
load("./Development/Data/RealData/CCPTestData_B.RData")
load("./Development/Data/RealData/CCPTestData_C.RData")



Server_SiteTotal <- newDSLiteServer(tables = CCPTestData_Total,
                                    config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                        "dsCCPhos",
                                                                                        "dsSynthetic")))


Server_SiteA <- newDSLiteServer(tables = CCPTestData_A,
                                config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                    "dsCCPhos")))

Server_SiteB <- newDSLiteServer(tables = CCPTestData_B,
                                config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                    "dsCCPhos")))

Server_SiteC <- newDSLiteServer(tables = CCPTestData_C,
                                config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                    "dsCCPhos")))


# Check out some server properties
Server_SiteA$config()
Server_SiteA$profile()
Server_SiteA$assignMethods()
Server_SiteA$aggregateMethods()



# Returns an environment
LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)


LoginBuilder$append(server = "SiteTotal",
                    url = "Server_SiteTotal",
                    driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteA",
                    url = "Server_SiteA",
                    driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteB",
                    url = "Server_SiteB",
                    driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteC",
                    url = "Server_SiteC",
                    driver = "DSLiteDriver")


# Returns a data frame of login data to different Sites
LoginData <- LoginBuilder$build()


# Get list of DSConnection objects of all servers
CCPConnections <- DSI::datashield.login(logins = LoginData,
                                        assign = TRUE)


# List all available tables in Sites
datashield.tables(CCPConnections)


# Test with ds.mean()
Test <- ds.mean(x = "Patient$geburtsdatum",
                type = "both",
                datasources = CCPConnections)



datashield.assign(CCPConnections, symbol = "BioSampling", value = "BioSampling")
datashield.assign(CCPConnections, symbol = "Diagnosis", value = "Diagnosis")
datashield.assign(CCPConnections, symbol = "Histology", value = "Histology")
datashield.assign(CCPConnections, symbol = "Metastasis", value = "Metastasis")
datashield.assign(CCPConnections, symbol = "MolecularDiagnostics", value = "MolecularDiagnostics")
datashield.assign(CCPConnections, symbol = "Patient", value = "Patient")
datashield.assign(CCPConnections, symbol = "Progress", value = "Progress")
datashield.assign(CCPConnections, symbol = "RadiationTherapy", value = "RadiationTherapy")
datashield.assign(CCPConnections, symbol = "Staging", value = "Staging")
datashield.assign(CCPConnections, symbol = "Surgery", value = "Surgery")
datashield.assign(CCPConnections, symbol = "SystemicTherapy", value = "SystemicTherapy")



ds.list(x = c("BioSampling",
              "Diagnosis",
              "Histology",
              "Metastasis",
              "MolecularDiagnostics",
              "Patient",
              "Progress",
              "RadiationTherapy",
              "Staging",
              "Surgery",
              "SystemicTherapy"),
        newobj = "RawData",
        datasources = CCPConnections)


# Curate the raw data
dsCCPhosClient::ds.CurateData(Name_RawData = "RawData",
                              Name_Output = "CurationOutput",
                              DataSources = CCPConnections)


# Get Curation reports
CurationReports <- ds.CurationReport(Name_CurationOutput = "CurationOutput",
                                     DataSources = CCPConnections)


# Augment data (based on curated data)
dsCCPhosClient::ds.AugmentData(Name_CurationOutput = "CurationOutput",
                               Name_Output = "AugmentationOutput",
                               DataSources = CCPConnections)








# Generate synthetic data using package dsSynthetic (which in turn makes use of packages synthpop and simstudy)

library(dsSyntheticClient)

SyntheticData <- ds.syn(data = "Diagnosis")

                        method = "cart",
                        m = 1,
                        seed = 123)

SyntheticData <- SyntheticData$SiteTotal$Warning


SyntheticData



#OpalDB_A <- dsCCPhos::MakeTestDB(CCPTestData_A)
#res_CCPTestData_A <- resourcer::newResource(name = "CCPTest")
#resourcer::PostgresResourceConnector$new()

# res_CCPTestData_A <- resourcer::newResource(name = "CCPTestData",
#                                             url = "file://./Development/Data/RealData/CCPTestData_A.RData",
#                                             format = "list")

#Test <- resourcer::FileResourceGetter$new()
