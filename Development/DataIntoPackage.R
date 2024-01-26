
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Table names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_TableNames <- read_excel(path = "./Development/Data/MetaData/MetaDataCCP.xlsx",
                              sheet = "TableNames")

# Save data in .rda-file and make it part of package
use_data(Meta_TableNames, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Feature names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureNames <- read_excel(path = "./Development/Data/MetaData/MetaDataCCP.xlsx",
                                sheet = "FeatureNames")

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureNames, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: CCP Value sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_ValueSets <- read_excel(path = "./Development/Data/MetaData/MetaDataCCP.xlsx",
                             sheet = "ValueSets")

# Save data in .rda-file and make it part of package
use_data(Meta_ValueSets, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Diagnosis association rules
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DiagnosisAssociation <- read_excel(path = "./Development/Data/MetaData/RuleSets.xlsx",
                                        sheet = "DiagnosisAssociation",
                                        skip = 2)

# Save data in .rda-file and make it part of package
use_data(Meta_DiagnosisAssociation, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data from TinkerLab
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_CancerGrouping <- TinkerLab::CancerGrouping

# Save data in .rda-file and make it part of package
use_data(Meta_CancerGrouping, overwrite = TRUE)
