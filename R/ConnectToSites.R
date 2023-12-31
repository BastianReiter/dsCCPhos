
#' ConnectToSites
#'
#' @return
#' @export
#'
#' @examples
ConnectToSites <- function()
{
    require(DSI)
    require(DSOpal)

    set_config(use_proxy(url="http://beam-connect", port=8062))
    set_config(config(ssl_verifyhost = 0L, ssl_verifypeer = 0L))

    # Returns an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    LoginBuilder$append(server = "DockerOpal",
                        url = "https://128.140.13.237/opal/",
                        token = "602e1f02-306e-4954-9872-6a0fd5ff33ef")
                        #table = "PROJECT-TEST_20231220_X1.patient"

    # Returns a data frame of login data to different Sites
    LoginData <- LoginBuilder$build()

    # Get list of DSConnection objects of all servers
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE)

    return(CCPConnections)

}
