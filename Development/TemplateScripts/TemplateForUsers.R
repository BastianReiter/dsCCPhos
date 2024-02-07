
# Install own dataSHIELD packages
#devtools::install_github(repo = "BastianReiter/dsCCPhos")
#devtools::install_github(repo = "BastianReiter/dsCCPhosClient")



# Load needed packages
library(dsBaseClient)
library(dsCCPhosClient)
library(DSI)
library(DSOpal)

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
                    token = "80e589f1-c670-413e-a593-3bbf6f64cf4b")

# Append credentials for server "Franz"
LoginBuilder$append(server = "Franz",
                    url = "https://dktk-test/opal/",
                    token = "a02f3a81-1a49-48c8-9df2-c3225e564cc5")

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
ProjectName <- "PROJECT-."

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

ds.CurateData(Name_RawData = "RawDataSet",
              Name_Output = "CurationOutput",
              DataSources = CCPConnections)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)


