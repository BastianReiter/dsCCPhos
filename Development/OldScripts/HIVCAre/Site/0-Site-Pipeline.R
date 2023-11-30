

################################################################################
#-------------------------------------------------------------------------------
#   HIVCAre: SITE PIPELINE                                                     
#-------------------------------------------------------------------------------
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear Workspace
# ---------------
rm(list = ls())


# Set Site Name
# -------------
SiteName <- "Frankfurt"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Project Setup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("./Scripts/SETUP.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize General Monitor Objects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initialize Progress Bar
#-------------------------------------------------------------------------------

try(ls_Progress_Total <- f_InitProgressBar(inp_Title = "Total",
                                           inp_ProgressTotalSteps = 14),      # Needs to be adjusted manually (How many times is f_UpdateProgressBar being called?)
    silent = TRUE)


# Initialize Attrittion Trackers
#-------------------------------------------------------------------------------

df_Mon_AttritionTracker_AllPatients <- f_InitAttritionTracker()
df_Mon_AttritionTracker_CancerPatients <- f_InitAttritionTracker()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site Input Data: Load Raw Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here(paste0("Scripts/Site/01-Site-Loading-", SiteName, ".R")))

#   ==>  Input Data Model (IDM)
#   ---------------------------
#   df_IDM_Cases
#   df_IDM_CasesDepartment
#   df_IDM_CasesICD
#   df_IDM_CasesOPS

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Loading raw data"), silent = TRUE)



# Update Attrition Tracker
# ------------------------
df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_UpdateAttritionTracker(inp_NewSampleSize = n_distinct(df_IDM_Cases$PatientPseudonym),
                                                                     inp_InclusionComment = "Initial Sample Size")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of Raw Input Data 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/02-Site-Validation-RawData.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Validation of raw data"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monitored Data Harmonization / Transformation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/03-Site-HarmonizeData.R"))

#   ==>  Resulting Objects (Harmonized Input Data Model)
#   ----------------------------------------------------
#   df_IDM_Cases
#   df_IDM_CasesDepartment
#   df_IDM_CasesICD
#   df_IDM_CasesOPS

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Data harmonization"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validation of Harmonized Input Data 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/04-Site-Validation-HarmonizedData.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Validation of harmonized data"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exclusion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/05-Site-DataExclusion.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Data exclusion"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Major Data Processing / Data Wrangling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/06-Site-Processing.R"))

#   ==>  Major Resulting Objects:
#   -----------------------------
#   df_ADM_Events
#   df_ADM_Diagnoses
#   df_ADM_CaseInfo
#   df_ADM_Patients
#   df_ADM_PatientsCancer
#   df_ADM_PatientsHIVCancer

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Creation of Analysis Data Model"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processed Data Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/07-Site-Validation-ProcessedData.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Validation of Analysis Data Model"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Close Attrition Trackers (Calculate Attrition Counts)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Mon_AttritionTracker_AllPatients <- df_Mon_AttritionTracker_AllPatients %>%
                                            f_CloseAttritionTracker()

df_Mon_AttritionTracker_CancerPatients <- df_Mon_AttritionTracker_CancerPatients %>%
                                              f_CloseAttritionTracker()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtain matched data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/08-Site-Matching.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Obtain matched data set"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform Data Analysis and Output Generation with FULL (unmatched) data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Site/09-Site-Analysis.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Perform statistical analysis"), silent = TRUE)


PlotOutputPath <- paste0("Reporting/Exploration/", SiteName, "/Plots/FullDataSet")

source(here("Scripts/Site/10-Site-Output.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Generate output objects"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save output objects in an encrypted .RData-file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get names of all objects containing the term "Output" or "ValidationReport"
vc_OutputObjects <- as_tibble(ls()) %>%
                        filter(grepl("Output|ValidationReport", value)) %>%
                        pull(name = value)

# Save all relevant output objects in an .Rdata-file
save(list = vc_OutputObjects,
     file = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_", SiteName, ".RData")),
     compress = TRUE)

# Encrypt file using AES-256 with encryptr
encrypt_file(.path = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_", SiteName, ".RData")),
             crypt_file_name = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_", SiteName, "_", str_remove_all(as.character(now()), "[^[:alnum:]]"), ".RData.encryptr.bin")),
             public_key_path = here("id_rsa.pub"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Saving and encrypting output data"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get Info Table about all Output Objects for Project Documentation (only necessary at Development Time)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_OutputObjectsInfo <- f_GetObjectInfo(inp_ObjectNames = vc_OutputObjects,
#                                         inp_Environment = environment())
# 
# write_csv(df_OutputObjectsInfo,
#           "OutputObjectsInfo.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Repeat Analysis with df_ADM_PatientsCancer_Strict / df_ADM_PatientsHIVCancer_Strict
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Temporarily save full data sets
#df_ADM_PatientsCancer_Temp <- df_ADM_PatientsCancer
#df_ADM_PatientsHIVCancer_Temp <- df_ADM_PatientsHIVCancer



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform Data Analysis and Output generation with MATCHED data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Temporarily save full data set
df_ADM_PatientsCancer_Temp <- df_ADM_PatientsCancer

# Assign matched data set
df_ADM_PatientsCancer <- df_ADM_PatientsCancer_Matched

# Run Analysis again
source(here("Scripts/Site/09-Site-Analysis.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Perform statistical analysis with matched data set"), silent = TRUE)

# Run Output Generation again
PlotOutputPath <- paste0("Reporting/Exploration/", SiteName, "/Plots/MatchedDataSet")

source(here("Scripts/Site/10-Site-Output.R"))

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Generate output objects of matched analysis"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save output objects of MATCHED ANALYSIS in an encrypted .RData-file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save all relevant output objects in an .Rdata-file
save(list = vc_OutputObjects,
     file = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_MatchedDataset_", SiteName, ".RData")),
     compress = TRUE)

# Encrypt file using AES-256 with encryptr
encrypt_file(.path = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_MatchedDataset_", SiteName, ".RData")),
             crypt_file_name = here(paste0("Data/SiteOutputData/", SiteName, "/SiteOutput_MatchedDataset_", SiteName, "_", str_remove_all(as.character(now()), "[^[:alnum:]]"), ".RData.encryptr.bin")),
             public_key_path = here("id_rsa.pub"))

# Restore full data set
df_ADM_PatientsCancer <- df_ADM_PatientsCancer_Temp

# Update Progress Bar
try(ls_Progress_Total$ProgressStepInfo <- f_UpdateProgressBar(ls_Progress_Total, inp_StepDescription = "Saving and encrypting output data"), silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Close Progress Bar
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sys.sleep(3)
try(close(ls_Progress_Total$ProgressBar), silent = TRUE)

cat("Analysis finished successfully!")


# ========== END OF PIPELINE ===================================================
