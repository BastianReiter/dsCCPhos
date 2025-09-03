
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data from TinkerLab
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_CancerGrouping <- TinkerLab::CancerGrouping

# Save data in .rda-file and make it part of package
use_data(Meta_CancerGrouping, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Raw Data Harmonization Methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DataHarmonizationMethods <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                            sheet = "DataHarmonizationMethods",
                                            skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_DataHarmonizationMethods, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Diagnosis Association Rule Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DiagnosisAssociation <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                        sheet = "DiagnosisAssociation")

# Save data in .rda-file and make it part of package
use_data(Meta_DiagnosisAssociation, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Diagnosis Redundancy Rule Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_DiagnosisRedundancy <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                       sheet = "DiagnosisRedundancy")

# Save data in .rda-file and make it part of package
use_data(Meta_DiagnosisRedundancy, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Dictionary (used in Data Harmonization)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Dictionary <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                              sheet = "Dictionary",
                              skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_Dictionary, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Event Feature Engineering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_EventFeatures <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                 sheet = "EventFeatures")

# Save data in .rda-file and make it part of package
use_data(Meta_EventFeatures, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Feature Obligations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureObligations <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                      sheet = "FeatureObligations",
                                      skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureObligations, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Features
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Features <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                            sheet = "Features")

# Save data in .rda-file and make it part of package
use_data(Meta_Features, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Feature Tracking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FeatureTracking <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                   sheet = "FeatureTracking",
                                   skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_FeatureTracking, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Raw Data Harmonization Methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_FuzzyStringMatching <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                       sheet = "FuzzyStringMatching",
                                       skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_FuzzyStringMatching, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: Table Normalization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_TableNormalization <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                      sheet = "TableNormalization",
                                      skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_TableNormalization, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Tables <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                          sheet = "Tables")

# Save data in .rda-file and make it part of package
use_data(Meta_Tables, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings: TransformativeExpressions (used in Data Harmonization)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_TransformativeExpressions <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                                             sheet = "TransformativeExpressions",
                                             skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_TransformativeExpressions, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta Data: Values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta_Values <- read_excel(path = "./Development/Data/MetaData/MetaDataCCPhos.xlsx",
                          sheet = "Values",
                          skip = 1)

# Save data in .rda-file and make it part of package
use_data(Meta_Values, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DisclosureSettings <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                           NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(DisclosureSettings, overwrite = TRUE)
