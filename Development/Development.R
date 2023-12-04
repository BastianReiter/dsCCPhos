

#===============================================================================
#
# CCPhos Package DEVELOPMENT TRACKER
#
#===============================================================================


library(devtools)



# Set preferred license in description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_ccby_license()

# Define part of project that should not be distributed in the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_build_ignore("Development")


# Adding package dependencies using usethis::use_package()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_package("dbplyr")
# use_package("dplyr")
# use_package("duckdb")
# use_package("ggplot2")
# use_package("gt")
# use_package("gtExtras")
# use_package("lubridate")
# use_package("readr")
# use_package("readxl")
# use_package("RPostgres")
# use_package("showtext")
# use_package("sysfonts")
# use_package("tibble")



# Adding function script files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("Augment")
# use_r("ConnectToOpalDB")
# use_r("Curate")
# use_r("data")
# use_r("DataListToDataModel")
# use_r("DataModelToCSV")
# use_r("DataModelToDB")
# use_r("GetDFInfo")
# use_r("Harmonize")
# use_r("LoadRawData")
use_r("Recode")
# use_r("TrackFeatureValues")
# use_r("XMLToDataModel")



# Get CCP Data Model from Opal DB Schema
CCPDataModel <- CCPhos::XMLToDataModel(XMLSchemaFilePath = "./Development/MetaData/SchemaOpalDB.xml")

