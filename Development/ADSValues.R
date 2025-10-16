

ADSValues <- ADS %>%
                imap(\(table, tablename) tibble(Table = tablename,
                                                FeatureName = names(table))) %>%
                list_rbind()

CDSValues <- Meta.Values %>%
                  select(FeatureName.Curated,
                         ScaleLevel,
                         Value.Curated,
                         Label.Curated) %>%
                  rename(c("FeatureName" = "FeatureName.Curated",
                           "Value" = "Value.Curated",
                           "Label" = "Label.Curated")) %>%
                  mutate(HasEligibleValueSet = TRUE,
                         FeatureIsFromCDS = TRUE)

ADSValues <- ADSValues %>%
                left_join(CDSValues,
                          by = join_by(FeatureName),
                          relationship = "many-to-many")




# ADSValues.NotFromCDS <- ADSValues %>%
#                             filter(is.na(FeatureIsFromCDS)) %>%
#                             mutate(GetsValueSet = case_when(Table == "Events" & FeatureName %in% c("EventType",
#                                                                                                    "EventClass",
#                                                                                                    "EventSubclass") ~ TRUE,
#                                                             Table == "DiseaseCourse" & FeatureName %in% c("EventTyoe",
#                                                                                                           "EventClass",
#                                                                                                           "EventSubclass",
#                                                                                                           )))



