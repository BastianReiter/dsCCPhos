
# Install own dataSHIELD packages
devtools::install_github(repo = "BastianReiter/dsCCPhos")
devtools::install_github(repo = "BastianReiter/dsCCPhosClient")

# Load needed packages
library(dsBaseClient)
library(dsCCPhosClient)
library(DSI)
library(DSOpal)
library(dsCCPhos)

# Beam settings
set_config(use_proxy(url = "http://beam-connect", port = 8062))
set_config(config(ssl_verifyhost = 0L, ssl_verifypeer = 0L))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish test server connections
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Sissy" = Home/Local server
# "Franz" = Remote server
#-------------------------------------------------------------------------------

# Returns an environment
LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

# Append credentials for server "Sissy"
LoginBuilder$append(server = "Sissy",
                    url = "https://dktk-datashield-test/opal/",
                    token = "751b90dc-7440-4d73-b1a6-4af73106119b")

# Append credentials for server "Franz"
LoginBuilder$append(server = "Franz",
                    url = "https://dktk-test/opal/",
                    token = "5d5f914b-88da-4a19-935b-dd426db4f62e")

# Returns a data frame of login data to different Sites
LoginData <- LoginBuilder$build()

# Get list of DSConnection objects of all servers
CCPConnections <- DSI::datashield.login(logins = LoginData,
                                        assign = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore test server configurations
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

# Enter Opal project name
ProjectName <- "PROJECT-TEST_20231220_X1."

# Check if all tables are accessible on all servers
ls_TableCheck <- purrr::map(as.list(CCPTableNames_Raw),
                            function(tbl)
                            {
                              datashield.table_status(conns = CCPConnections,
                                                      table = paste0(ProjectName, tbl))
                            })

# Turn list into data.frame
df_TableCheck <- do.call(rbind, ls_TableCheck)


# Make tables from data repository accessible in R session
for(i in 1:length(CCPTableNames_Raw))
{
  datashield.assign(conns = CCPConnections,
                    symbol = CCPTableNames_Curated[i],
                    value = paste0(ProjectName, CCPTableNames_Raw[i]),
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
# Use dsCCPhos functionality to process data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
ds.CurateData(Name_RawData = "RawDataSet",
              Name_Output = "CurationOutput",
              DataSources = CCPConnections)


# Get Curation reports
CurationReports <- dsCCPhosClient::ds.GetCurationReport(Name_CurationOutput = "CurationOutput",
                                                        DataSources = CCPConnections)

# Exemplary look at a curation report table
View(CurationReports$Sissy$Monitor_Staging)


# Make tables of Curated Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackCuratedDataSet(Name_CurationOutput = "CurationOutput",
                                        DataSources = CCPConnections)


# For monitoring purposes: List all current objects in server-sided R sessions
DSI::datashield.symbols(conns = CCPConnections)


# Transfrom Curated Data Set (CDS) into Augmented Data Set (ADS)
ds.AugmentData(Name_CurationOutput = "CurationOutput",
               Name_Output = "AugmentationOutput",
               DataSources = CCPConnections)

# Make tables of Augmented Data Set directly addressable by unpacking them into R server session
dsCCPhosClient::ds.UnpackAugmentedDataSet(Name_AugmentationOutput = "AugmentationOutput",
                                          DataSources = CCPConnections)

# ... Check out the new objects
DSI::datashield.symbols(conns = CCPConnections)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)

