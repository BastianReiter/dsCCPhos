
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Table names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Tables <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                          sheet = "Tables")

# Save data in .rda-file and make it part of package
use_data(Meta_Tables, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Features <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                            sheet = "Features")

# Save data in .rda-file and make it part of package
use_data(Meta_Features, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Value sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_ValueSets <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                             sheet = "ValueSets")

# Save data in .rda-file and make it part of package
use_data(Meta_ValueSets, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Augment Event Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_AugmentEventData <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                    sheet = "AugmentEventData")

# Save data in .rda-file and make it part of package
use_data(Meta_AugmentEventData, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Feature obligations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureObligations <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                      sheet = "FeatureObligations")

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureObligations, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Raw Data Harmonization rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DataHarmonization <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                     sheet = "DataHarmonization")

# Save data in .rda-file and make it part of package
use_data(Meta_DataHarmonization, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Diagnosis association rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DiagnosisAssociation <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                        sheet = "DiagnosisAssociation")

# Save data in .rda-file and make it part of package
use_data(Meta_DiagnosisAssociation, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Diagnosis redundancy rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DiagnosisRedundancy <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                       sheet = "DiagnosisRedundancy")

# Save data in .rda-file and make it part of package
use_data(Meta_DiagnosisRedundancy, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data from TinkerLab
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_CancerGrouping <- TinkerLab::CancerGrouping

# Save data in .rda-file and make it part of package
use_data(Meta_CancerGrouping, overwrite = TRUE)
