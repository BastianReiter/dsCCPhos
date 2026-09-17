
library(dplyr)
library(stringr)
library(tidyr)

# Instead of original MUST csv file, read in custom modification for CCPhos purposes
TNMGroupMapping <- readxl::read_xlsx("C:/Users/Basti/ARBEIT Lokal/Data/UICCClassification/CCPhosMapping.xlsx",
                                     sheet = "TNMGroupMapping")

UICCStageMapping <- readxl::read_xlsx("C:/Users/Basti/ARBEIT Lokal/Data/UICCClassification/CCPhosMapping.xlsx",
                                      sheet = "UICCStageMapping")


# TNMGroupMapping <- TNMGroupMapping %>%
#                   mutate(across(!Comment,
#                                 ~ str_remove_all(.x, " "))) %>%
#                   separate_longer_delim(cols = TNMVersion, delim = ",") %>%
#                   separate_longer_delim(cols = Inclusion.ICDOTopographyCode, delim = ",") %>%
#                   separate_longer_delim(cols = Inclusion.ICDOTopographyCode.Short, delim = ",") %>%
#                   separate_longer_delim(cols = Exclusion.ICDOTopographyCode, delim = ",") %>%
#                   separate_longer_delim(cols = Inclusion.ICD10Code.Short, delim = ",") %>%
#                   separate_longer_delim(cols = Exclusion.ICD10Code.Short, delim = ",") %>%
#                   separate_longer_delim(cols = Inclusion.ICDOMorphologyHistologyCode, delim = ",") %>%
#                   separate_longer_delim(cols = Exclusion.ICDOMorphologyHistologyCode, delim = ",") %>%
#                   separate_longer_delim(cols = Inclusion.PatientAgeAtStaging, delim = ",") %>%
#                   distinct()


UICCStageMapping <- UICCStageMapping %>%
                        distinct() %>%
                        filter(TNMGroup != "OropharynxP16Pos") %>%       # Filter out because no info on P16 in CCP data
                        mutate(TNM.T = str_remove_all(TNM.T, " "),
                               TNM.N = str_remove_all(TNM.N, " "),
                               TNM.M = str_remove_all(TNM.M, " "),
                               TNM.S = str_remove_all(TNM.S, " "),
                               TNM.T = na_if(TNM.T, ""),
                               TNM.N = na_if(TNM.N, ""),
                               TNM.M = na_if(TNM.M, ""),
                               TNM.S = na_if(TNM.S, ""),
                               TNM.T = na_if(TNM.T, "*"),
                               TNM.N = na_if(TNM.N, "*"),
                               TNM.M = na_if(TNM.M, "*"),
                               TNM.S = na_if(TNM.S, "*"),
                               TNMVersion = as.character(TNMVersion),
                               Grading = str_remove_all(Grading, " "),
                               Grading = str_remove_all(Grading, "[GX]"),      # Remove letters 'G' and 'X'
                               Grading = na_if(Grading, ""),
                               UICCStage = str_replace(UICCStage, "0kkuItesKarzin0m", "OkkultesKarzinom"))
                        #left_join(LOCMapping.Expand, by = join_by(LOC, TNMVersion), relationship = "many-to-many")



# Check if mapping from TNMGroupMapping to UICCStageMapping does not lead to empty subsets of UICCStageMapping
MismatchTest <- TNMGroupMapping %>%
                    mutate(across(!Comment,
                           ~ str_remove_all(.x, " "))) %>%
                    separate_longer_delim(cols = Match.TNMVersion, delim = ",") %>%
                    left_join(UICCStageMapping, by = join_by(TNMGroup, Match.TNMVersion == TNMVersion)) %>%
                    filter(is.na(UICCStage))



# Screening for duplicates
# DuplicateTest <- UICCStageMapping %>%
#                       group_by(across(!TNMGroup)) %>%
#                           summarize(NumberRecords = n(),
#                                     TNMGroups = paste0(TNMGroup, collapse = " / ")) %>%
#                       ungroup() %>%
#                       filter(NumberRecords > 1)


Res.UICCMapping <- list(TNMGroupMapping = TNMGroupMapping,
                        UICCStageMapping = UICCStageMapping)


Password <- paste0(sodium::random(n = 24), collapse = "")

Key <- sodium::hash(charToRaw(Password))

Nonce <- sodium::random(n = 24)

Blob.UICCMapping <- serialize(Res.UICCMapping,
                              connection = NULL)

Cipher.UICCMapping <- sodium::data_encrypt(msg = Blob.UICCMapping,
                                           key = Key,
                                           nonce = Nonce)

saveRDS(list(Nonce = Nonce,
             Cipher = Cipher.UICCMapping),
        file = "C:/Users/Basti/ARBEIT Lokal/Data/UICCClassification/Res.UICCMapping.rds")





