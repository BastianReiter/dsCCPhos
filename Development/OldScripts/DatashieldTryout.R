

# DataSHIELD Tryout

# install.packages("DSI")
# install.packages("DSOpal")
# install.packages("dsBaseClient", repos=c(getOption("repos"), "http://cran.datashield.org"), dependencies=TRUE)

library(devtools)
library(dsBaseClient)
library(DSLite)
library(DSOpal)
library(roxygen2)
library(usethis)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOGIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Returns an environment
builder <- DSI::newDSLoginBuilder(.silent = FALSE)

#ls(envir = builder)

builder$append(server = "server1",
               url = "https://opal-demo.obiba.org",
               user = "dsuser",
               password = "P@ssw0rd",
               driver = "OpalDriver",
               options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")

builder$append(server = "server2",
               url = "https://opal-demo.obiba.org",
               user = "dsuser",
               password = "P@ssw0rd",
               driver = "OpalDriver",
               options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")

# Returns a data.frame of login data to different Sites
LoginData <- builder$build()
#View(LoginData)


# Get list of DSConnection / OpalConnection objects of all servers
connections <- DSI::datashield.login(logins = LoginData,
                                     assign = TRUE)


DSI::datashield.assign.table(conns = connections,
                             symbol = "DST",                                    # Placeholder symbol to address
                             table = c("CNSIM.CNSIM1",
                                       "CNSIM.CNSIM2"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ds.dim(x = "DST")
ds.colnames(x = "DST")

ds.quantileMean(x = "DST$LAB_HDL")

# Create mutation variable
ds.log(x = 'DST$LAB_HDL',
       newobj='LAB_HDL_log')

# Create new variable
ds.assign(toAssign = "DST$LAB_HDL - 1.5",
          newobj = "LAB_HDL.sf")

ds.table(rvar = "DST$Gender")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOGOUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logging out of servers
#-------------------------------------------------------------------------------

DSI::datashield.logout(connections)
