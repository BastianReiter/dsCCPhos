

################################################################################
#------------------------------------------------------------------------------#
#                   DKTK-CCP Clinical Data Science Group                       #                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#   PROJECT:  LCNEC
#                                                                              #
################################################################################


# Clear workspace
# ---------------
rm(list = ls())

# Set site name
# -------------
SiteName <- "Frankfurt"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Project Setup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("./Scripts/SETUP.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize supporting monitoring objects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initialize Progress Bar
# -----------------------
try(ls_Progress_Total <- f_InitProgressBar(inp_Title = "Total",
                                           inp_ProgressTotalSteps = 14),      # Needs to be adjusted manually (How many times is f_UpdateProgressBar being called?)
    silent = TRUE)


# Initialize Attrittion Tracker
# -----------------------------
df_AttritionTracker <- f_InitAttritionTracker()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Seed Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site", paste0("1-Site-LoadData-", SiteName, ".R")))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Attrition Tracker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_AttritionTracker <- df_AttritionTracker %>%
                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_SDM_Patients$PatientID),
                                                     inp_InclusionComment = "Initial Sample Size")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of Unharmonized Seed Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/2-Site-Validation-RawData.R"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monitored Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/3-Site-HarmonizeData.R"))

#   ==>  Resulting Objects (Harmonized Seed Data Model)
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    - df_SDM_Patients
#    - df_SDM_Diagnosis
#    - df_SDM_Tumor
#    - df_SDM_Histology
#    - df_SDM_Staging
#    - df_SDM_Metastasis
#    - df_SDM_Progress
#    - df_SDM_Surgery
#    - df_SDM_SystemicTherapy
#    - df_SDM_Radiotherapy
#    - df_SDM_BioSampling
#    - df_SDM_MolecularDiagnostics



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of Harmonized Raw Data Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/4-Site-Validation-HarmonizedData.R"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Only once at development time: Get Info about HRDM for Documentation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# vc_InputObjects <- c("df_SDM_Patients",
#                      "df_SDM_Diagnosis",
#                      "df_SDM_Tumor",
#                      "df_SDM_Histology",
#                      "df_SDM_Progress",
#                      "df_SDM_SystemicTherapy",
#                      "df_SDM_Staging",
#                      "df_SDM_Radiotherapy",
#                      "df_SDM_Surgery",
#                      "df_SDM_Metastasis",
#                      "df_SDM_MolecularDiagnostics",
#                      "df_SDM_BioSampling")
# 
# 
# df_InputDataAttributes  <- list_rbind(map(vc_InputObjects,
#                                           f_GetAttributesInfo,
#                                           inp_Environment = environment(),
#                                           inp_IncludeRandomExampleValues = TRUE))
# 
# write.csv(df_SeedDataAttributes, "SeedDataAttributes.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exclusion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#source(here("Scripts/Site/5-Site-DataExclusion.R"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Major Data Processing / Data Wrangling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/6-Site-Processing.R"))

#   ==>  Major Resulting Objects:
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   df_...
#   df_...
#   ...


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processed Data Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/7-Site-Validation-ProcessedData.R"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Close Attrition Tracker (Calculate Attrition Counts)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_AttritionTracker <- df_AttritionTracker %>%
                            f_CloseAttritionTracker()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform Data Analysis and Output Generation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/8-Site-Analysis.R"))

source(here("Scripts/Site/9-Site-Output.R"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save output objects in an encrypted .RData-file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get names of all objects containing the term "Output" or "ValidationReport"
vc_OutputObjects <- as_tibble(ls()) %>%
                        filter(grepl("Output|ValidationReport", value)) %>%
                        pull(name = value)

# Save all relevant output objects in an .Rdata-file
save(list = vc_OutputObjects,
     file = here("Data/SiteOutputData", paste0("SiteOutput_", SiteName, ".RData")),
     compress = TRUE)

# Encrypt file using AES-256 with encryptr
encrypt_file(here("Data/SiteOutputData", paste0("SiteOutput_", SiteName, ".RData")),
             public_key_path = here("id_rsa.pub"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Info Table about all Output Objects for Project Documentation (only necessary at Development Time)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df_OutputObjectsInfo <- f_GetObjectInfo(inp_ObjectNames = vc_OutputObjects,
#                                         inp_Environment = environment())
# 
# write_csv(df_OutputObjectsInfo,
#           "OutputObjectsInfo.csv")




# ---------- END OF PIPELINE ---------------------------------------------------
