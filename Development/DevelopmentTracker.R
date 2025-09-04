

#===============================================================================
#
# dsCCPhos Package DEVELOPMENT TRACKER
#
#===============================================================================


library(devtools)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set preferred license in description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_ccby_license()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define parts of project that should not be distributed in the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use_build_ignore("Development")
# use_build_ignore("Documentation")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding package dependencies using usethis::use_package()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use_package("assertthat")
# use_package("cli")
# use_package("dplyr")
# use_package("DSI")
# use_package("DSOpal")
# use_package("lubridate")
# use_package("MatchIt")
# use_package("progress")
# use_package("readr", type = "Suggests")
# use_package("readxl", type = "Suggests")
# use_package("resourcer", type = "Suggests")
# use_package("rlang")
# use_package("stats")
# use_package("stringdist")
# use_package("stringr")
# use_package("survival")
# use_package("tibble")
# use_package("tidyr")
# use_package("validate")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding R script files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Structural files
#~~~~~~~~~~~~~~~~~
# use_r("data")

# General / Auxiliary functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("AttritionTracker_Close")
# use_r("AttritionTracker_Init")
# use_r("AttritionTracker_Update")
# use_r("CheckTable")
# use_r("ClassifyDiagnosisAssociation")
# use_r("ClassifyDiagnosisRedundancy")
# use_r("CleanTable")
# use_r("CompileClassificationCall")
# use_r("CompileDictionaries")
# use_r("CompileTransFeatureRules")
# use_r("CompileTransformativeAssignments")
# use_r("CountDeviations")
# use_r("CreateEventFeatures")
# use_r("DataListToDataModel")
# use_r("DataModelToCSV")
# use_r("DataModelToDB")
# use_r("FinalizeDataTransformation")
# use_r("FindRedundantEntries")
# use_r("FormatData")
# use_r("GetBestStringMatch")
# use_r("GetCrossTab")
# use_r("GetDFInfo")
# use_r("GetFuzzyStringMatches")
# use_r("HarmonizeData")
# use_r("MakeTestDB")
# use_r("NormalizeTable")
# use_r("LoadRawData")
# use_r("RecodeData")
# use_r("ReplaceDiagnosisIDs")
# use_r("SummarizeEventData")
# use_r("TrackFeatureValues")
# use_r("XMLToDataModel")


# DataSHIELD AGGREGATE functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("GetCohortDescriptionDS")
# use_r("GetDataSetOverviewDS")
# use_r("GetFeatureInfoDS")
# use_r("GetFrequencyTableDS")
# use_r("GetObjectStatusDS")
# use_r("GetObjectMetaDataDS")
# use_r("GetOutcomeMeasuresDS")
# use_r("GetRDSValidationReportDS")
# use_r("GetReportingObjectDS")
# use_r("GetSampleStatisticsDS")
# use_r("GetTableCheckDS")


# DataSHIELD ASSIGN functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_r("AugmentDataDS")
# use_r("CurateDataDS")
# use_r("ExcludeRawDataDS")
# use_r("ExcludeAugmentedDataDS")
# use_r("ExcludeCuratedDataDS")
# use_r("ExtractFromListDS")
# use_r("FilterTableDS")
# use_r("JoinTablesDS")
# use_r("MatchDataDS")
# use_r("MutateTableDS")




# Get CCP Data Model from Opal DB Schema
# CCPDataModel <- CCPhos::XMLToDataModel(XMLSchemaFilePath = "./Development/MetaData/SchemaOpalDB.xml")

