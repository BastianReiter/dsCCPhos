
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Tables <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                          sheet = "Tables")

# Save data in .rda-file and make it part of package
use_data(Meta_Tables, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Features
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Features <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                            sheet = "Features")

# Save data in .rda-file and make it part of package
use_data(Meta_Features, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Feature values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Values <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                          sheet = "Values",
                          skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_Values, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Event Feature Engineering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_EventFeatures <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                 sheet = "EventFeatures")

# Save data in .rda-file and make it part of package
use_data(Meta_EventFeatures, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Feature obligations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureObligations <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                      sheet = "FeatureObligations",
                                      skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureObligations, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Feature tracking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureTracking <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                   sheet = "FeatureTracking",
                                   skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureTracking, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Raw Data Harmonization rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DataHarmonizationRules <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                          sheet = "DataHarmonizationRules",
                                          skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_DataHarmonizationRules, overwrite = TRUE)



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
