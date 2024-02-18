

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual dataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install own dataSHIELD packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#devtools::install_github(repo = "BastianReiter/dsCCPhos")
#devtools::install_github(repo = "BastianReiter/dsCCPhosClient")
#devtools::install_github(repo = "BastianReiter/TinkerLab")

# Install additional datashield-packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#devtools::install_github("tombisho/dsSynthetic", dependencies = TRUE)
#devtools::install_github("tombisho/dsSyntheticClient", dependencies = TRUE)

#devtools::install_github("neelsoumya/dsSurvival")
#devtools::install_github("neelsoumya/dsSurvivalClient")


# Load needed packages
library(dsBase)
library(dsBaseClient)
library(dsCCPhos)
library(dsCCPhosClient)
library(DSLite)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load test data in local environment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("../dsCCPhos/Development/Data/TestData/CCPTestData_Small_A.RData")
load("../dsCCPhos/Development/Data/TestData/CCPTestData_Small_B.RData")
load("../dsCCPhos/Development/Data/TestData/CCPTestData_Small_C.RData")

CCPTestData_A <- CCPTestData_Small_A
CCPTestData_B <- CCPTestData_Small_B
CCPTestData_C <- CCPTestData_Small_C



#load("./Development/Data/RealData/CCPTestData_Total.RData")
#load("./Development/Data/RealData/CCPTestData_A.RData")
#load("./Development/Data/RealData/CCPTestData_B.RData")
#load("./Development/Data/RealData/CCPTestData_C.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setting up virtual servers with included test data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Server_SiteTotal <- newDSLiteServer(tables = CCPTestData_Total,
#                                     config = DSLite::defaultDSConfiguration(include = c("dsBase",
#                                                                                         "dsCCPhos")))

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
# Server_SiteA$config()
# Server_SiteA$profile()
# Server_SiteA$assignMethods()
# Server_SiteA$aggregateMethods()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish connection to virtual servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Returns an environment
LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)


# LoginBuilder$append(server = "SiteTotal",
#                     url = "Server_SiteTotal",
#                     driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteA",
                    url = "Server_SiteA",
                    driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteB",
                    url = "Server_SiteB",
                    driver = "DSLiteDriver")

LoginBuilder$append(server = "SiteC",
                    url = "Server_SiteC",
                    driver = "DSLiteDriver")

# Returns a data frame of login data to servers
LoginData <- LoginBuilder$build()

# Get list of DSConnection objects of all servers
CCPConnections <- DSI::datashield.login(logins = LoginData,
                                        assign = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore server configurations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List all available dataSHIELD methods on servers
DSI::datashield.methods(conns = CCPConnections)

# Alternatively use DSI::datashield.method_status() to get more comparable overview
# AGGREGATE functions
DSI::datashield.method_status(conns = CCPConnections,
                              type = "aggregate")

# ASSIGN functions
DSI::datashield.method_status(conns = CCPConnections,
                              type = "assign")

# Get info about installed packages on servers
DSI::datashield.pkg_status(conns = CCPConnections)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare data objects in server R sessions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goal: Assignment of list object "RawDataSet" on all R server sessions
#-------------------------------------------------------------------------------

# Get overview of accessible tables on servers
DSI::datashield.tables(conns = CCPConnections)

# Get table names of CCP core data set
CCPTableNames_Raw <- dsCCPhos::Meta_TableNames$TableName_Raw
CCPTableNames_Curated <- dsCCPhos::Meta_TableNames$TableName_Curated

# Check if all tables are accessible on all servers
ls_TableCheck <- purrr::map(as.list(CCPTableNames_Curated),
                            function(tbl)
                            {
                              datashield.table_status(conns = CCPConnections,
                                                      table = tbl)
                            })

# Turn list into data.frame
df_TableCheck <- do.call(rbind, ls_TableCheck)


# Make tables from data repository accessible in R session
for(i in 1:length(CCPTableNames_Curated))
{
  datashield.assign(conns = CCPConnections,
                    symbol = CCPTableNames_Curated[i],
                    value = CCPTableNames_Curated[i],
                    id.name = "_id")
}

# Consolidate all raw data tables in one list object called "RawDataSet"
ds.list(x = CCPTableNames_Curated,
        newobj = "RawDataSet",
        datasources = CCPConnections)

# Make sure assignment was successful on all servers
ds.GetObjectInfo(ObjectName = "RawDataSet",
                 DataSources = CCPConnections)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ready for working with dsCCPhos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First test with dsBase-function
# Test <- ds.mean(x = "Metastasis$datum_fernmetastasen",
#                 type = "both",
#                 datasources = CCPConnections)


# Get validation report of Raw Data Set (RDS)
# ValidationReportRDS <- ds.GetValidationReport_RDS(Name_RawDataSet = "RawDataSet",
#                                                       DataSources = CCPConnections)


# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
dsCCPhosClient::ds.CurateData(Name_RawDataSet = "RawDataSet",
                              Name_Output = "CurationOutput",
                              DataSources = CCPConnections)


# Get Curation reports
CurationReports <- dsCCPhosClient::ds.GetCurationReport(Name_CurationOutput = "CurationOutput",
                                                        DataSources = CCPConnections)

# Exemplary look at a curation report table
View(CurationReports$SiteA$Monitor_Staging)


# Make tables from Curated Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackCuratedDataSet(Name_CurationOutput = "CurationOutput",
                                        DataSources = CCPConnections)


# List all objects in server-sided R sessions
DSI::datashield.symbols(conns = CCPConnections)




# Get validation report of Curated Data Set (CDS)
# ValidationReportCDS <- ds.GetValidationReport_CDS(Name_CurationOutput = "CurationOutput",
#                                                   DataSources = CCPConnections)



# Try out data augmentation method
dsCCPhosClient::ds.AugmentData(Name_CurationOutput = "CurationOutput",
                               Name_Output = "AugmentationOutput",
                               DataSources = CCPConnections)


# Make tables from Augmented Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackAugmentedDataSet(Name_AugmentationOutput = "AugmentationOutput",
                                          DataSources = CCPConnections)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ds.colnames(x = "ADS_Patients",
            datasources = CCPConnections)


ds.GetSampleStatistics(TableName = "ADS_Patients",
                       MetricFeatureName = "PatientAgeAtDiagnosis",
                       GroupingFeatureName = "LastVitalStatus") %>%
    MakeBoxPlot()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)






# Generate synthetic data using package dsSynthetic (which in turn makes use of packages synthpop and simstudy)

# library(dsSyntheticClient)
#
# SyntheticData <- ds.syn(data = "Diagnosis")
#
#                         method = "cart",
#                         m = 1,
#                         seed = 123)
#
# SyntheticData <- SyntheticData$SiteTotal$Warning
#
#
# SyntheticData



#OpalDB_A <- dsCCPhos::MakeTestDB(CCPTestData_A)
#res_CCPTestData_A <- resourcer::newResource(name = "CCPTest")
#resourcer::PostgresResourceConnector$new()

# res_CCPTestData_A <- resourcer::newResource(name = "CCPTestData",
#                                             url = "file://./Development/Data/RealData/CCPTestData_A.RData",
#                                             format = "list")

#Test <- resourcer::FileResourceGetter$new()
