
library(dplyr)
library(readxl)
library(usethis)


#===============================================================================
# Read in Package Data from xlsx-file
#===============================================================================

ExcelFilePath <- "./Development/Data/ModuleData/ModuleDataCCPhos.xlsx"

Sheetnames <- c("Meta.Tables",
                # "Meta.Relationships",
                # "Meta.Links",
                "Meta.Features",
                "Meta.Values",
                "Proc.TableNormalization",
                "Proc.EventFeatures",
                "Set.CurationProcess",
                "Set.PrimaryTableCleaning",
                "Set.SecondaryTableCleaning",
                "Set.RecordSubsumption",
                "Set.ValueAvailability",
                "Set.ValueValidity",
                "Set.FeatureTracking",
                "Set.DataRemediation",
                "Set.TransformativeExpressions",
                "Set.Dictionary",
                "Set.FuzzyStringMatching",
                "Set.DiagnosisRedundancy",
                "Set.DiagnosisAssociation")

for (sheetname in Sheetnames)
{
    Table <- read_excel(path = ExcelFilePath,
                        sheet = sheetname,
                        skip = 2,
                        col_types = "text")

    ColumnTypes <- read_excel(path = ExcelFilePath,
                              sheet = sheetname,
                              range = cell_rows(2),
                              col_names = colnames(Table)) %>%
                      tidyr::pivot_longer(everything(),
                             names_to = "Column",
                             values_to = "Type")

    if (nrow(ColumnTypes) > 0)
    {
        for (i in 1:nrow(ColumnTypes))
        {
            Table <- Table %>%
                          mutate(across(all_of(ColumnTypes$Column[i]),
                                        ~ dsFreda::FormatData(.x, ColumnTypes$Type[i])))
        }
    }

    assign(x = sheetname,
           value = Table)

    # Save data in .rda-file and make it part of the package
    do.call(usethis::use_data, list(as.name(sheetname), overwrite = TRUE))
}


#===============================================================================
# Add EligibleValueSets for ADS tables
#===============================================================================

# Extract table and feature names from ADS tables
Meta.ADS <- ADS %>%
              imap(\(Table, tablename) tibble(TableName = tablename,
                                              FeatureName = names(Table),
                                              FeatureType = sapply(Table, class))) %>%
              list_rbind()

# Get eligible values and labels from CDS features
CDSValues <- Meta.Values %>%
                  select(FeatureName.Curated,
                         Value.Curated,
                         Label.Curated) %>%
                  rename(c("FeatureName" = "FeatureName.Curated",
                           "Value" = "Value.Curated",
                           "Label" = "Label.Curated")) %>%
                  mutate(HasEligibleValueSet = TRUE,
                         FeatureIsFromCDS = TRUE)

# Attach eligible values and labels to ADS features
Meta.ADS <- Meta.ADS %>%
                left_join(CDSValues,
                          by = join_by(FeatureName),
                          relationship = "many-to-many") %>%
                distinct()

# Save data in .rda-file and make it part of package
usethis::use_data(Meta.ADS, overwrite = TRUE)


# ADSValues.NotFromCDS <- ADSValues %>%
#                             filter(is.na(FeatureIsFromCDS)) %>%
#                             mutate(GetsValueSet = case_when(Table == "Events" & FeatureName %in% c("EventType",
#                                                                                                    "EventClass",
#                                                                                                    "EventSubclass") ~ TRUE,
#                                                             Table == "DiseaseCourse" & FeatureName %in% c("EventTyoe",
#                                                                                                           "EventClass",
#                                                                                                           "EventSubclass",
#                                                                                                           )))


#===============================================================================
# Resource data from TinkerLab
#===============================================================================


Res.ATCCoding <- TinkerLab::Res.ATCCoding %>%
                      rename(ATCCode = "Code")


Res.CancerGrouping <- TinkerLab::Res.CancerGrouping

# Save data in .rda-file and make it part of package
usethis::use_data(Res.CancerGrouping, overwrite = TRUE)


Res.ICDOMorphology <- TinkerLab::Res.ICDOMorphology

usethis::use_data(Res.ICDOMorphology, overwrite = TRUE)



Res.SystemicTherapy.Regimens <- TinkerLab::Res.SystemicTherapy.Regimens %>%
                                    mutate(Substances = pmap(pick(starts_with("Substance")),
                                                             ~ unname(c(...)) %>% discard(is.na) %>% sort()),      # Put values of Substance columns in vectors and sort the alphabetically
                                           .after = Regimen) %>%
                                    select(-matches("^Substance[0-9]$")) %>%
                                    mutate(Substances.String = map_chr(Substances, ~ paste(.x, collapse = "*&*")),
                                           .after = Substances)

usethis::use_data(Res.SystemicTherapy.Regimens, overwrite = TRUE)



#===============================================================================
# CCPhos Privacy Settings
#===============================================================================

Set.Privacy <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                    NThreshold = 5)

# Save data in .rda-file and make it part of package
usethis::use_data(Set.Privacy, overwrite = TRUE)
