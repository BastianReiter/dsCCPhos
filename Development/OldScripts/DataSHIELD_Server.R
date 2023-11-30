

source("./SETUP.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOGIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Returns an environment
CCP_Builder <- DSI::newDSLoginBuilder(.silent = FALSE)

#ls(envir = builder)

# CCP_Builder$append(server = "server1",
#                    url = "https://opal-demo.obiba.org",
#                    user = "dsuser",
#                    password = "P@ssw0rd",
#                    driver = "OpalDriver",
#                    options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")
# 
# CCP_Builder$append(server = "server2",
#                    url = "https://opal-demo.obiba.org",
#                    user = "dsuser",
#                    password = "P@ssw0rd",
#                    driver = "OpalDriver",
#                    options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")


CCP_SiteA <- newDSLiteServer(tables = list(df_SDM_Patients = df_Patients_A))


# Returns a data.frame of login data to different Sites
CCP_LoginData <- CCP_Builder$build()
#View(LoginData)


# Get list of DSConnection / OpalConnection objects of all servers
CCP_Connections <- DSI::datashield.login(logins = CCP_LoginData,
                                         assign = TRUE)


DSI::datashield.assign.table(conns = CCP_Connections,
                             symbol = "DST",                                    # Placeholder symbol to address
                             table = c("CNSIM.CNSIM1",
                                       "CNSIM.CNSIM2"))



