
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
# Meta data: Raw Data Harmonization rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RuleSet_RawDataHarmonization <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                           sheet = "RawDataHarmonization")

# Save data in .rda-file and make it part of package
use_data(RuleSet_RawDataHarmonization, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta data: Diagnosis association rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RuleSet_DiagnosisAssociation <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                           sheet = "DiagnosisAssociation",
                                           skip = 2)

# Save data in .rda-file and make it part of package
use_data(RuleSet_DiagnosisAssociation, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta data: Diagnosis redundancy rule set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RuleSet_DiagnosisRedundancy <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                          sheet = "DiagnosisRedundancy",
                                          skip = 2)

# Save data in .rda-file and make it part of package
use_data(RuleSet_DiagnosisRedundancy, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data from TinkerLab
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_CancerGrouping <- TinkerLab::CancerGrouping

# Save data in .rda-file and make it part of package
use_data(Meta_CancerGrouping, overwrite = TRUE)
