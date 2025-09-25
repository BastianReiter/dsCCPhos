  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          Table <- Table %>% mutate(TrackID = row_number())      # Enables tracking of transformation (see above)

                          if (Settings$DataHarmonization$Run == TRUE)      # The check is placed here and not before the mapping function so that the 'TrackID' is appended regardless (see above)
                          {
                              # Get a list of vectors containing the eligible values of corresponding features
                              EligibleValueSets <- dsCCPhos::Meta.Values %>%
                                                        filter(Table == tablename) %>%
                                                        select(Feature, Value.Raw) %>%
                                                        split(.$Feature) %>%      # 'split' is a base function and needs '.$' to address 'Feature'
                                                        map(\(Set) Set$Value.Raw)

                              # Filter meta data with current table name
                              Methods <- Settings$DataHarmonization$Methods %>% filter(Table == tablename)
                              TransformativeExpressions = Settings$DataHarmonization$TransformativeExpressions %>% filter(Table == tablename)
                              Dictionary <- Settings$DataHarmonization$Dictionary %>% filter(Table == tablename)
                              FuzzyStringMatching <- Settings$DataHarmonization$FuzzyStringMatching %>% filter(Table == tablename)

                              # Perform Data Harmonization with table-specific settings
                              Table <- Table %>%
                                            dsFreda::HarmonizeData(EligibleValueSets = EligibleValueSets,
                                                                   Methods = Methods,
                                                                   TransformativeExpressions = TransformativeExpressions,
                                                                   TransformativeExpressions.Profile = Settings$DataHarmonization$TransformativeExpressions.Profile,
                                                                   Dictionary = Dictionary,
                                                                   Dictionary.Profile = Settings$DataHarmonization$Dictionary.Profile,
                                                                   FuzzyStringMatching = FuzzyStringMatching,
                                                                   FuzzyStringMatching.Profile = Settings$DataHarmonization$FuzzyStringMatching.Profile)
                          }

                          return(Table)
                       })




    DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          Table <- Table %>% mutate(TrackID = row_number())      # Enables tracking of transformation (see above)

                          if (Settings$DataHarmonization$Run == TRUE)      # The check is placed here and not before the mapping function so that the 'TrackID' is appended regardless (see above)
                          {
                              HarmonizationProcess <- Settings$DataHarmonization$Process %>%
                                                          filter(Profile == Settings$DataHarmonization$Process.Profile,
                                                                 Table == tablename,
                                                                 RunHarmonization == TRUE)

                              for (featurename in HarmonizationProcess$Feature)
                              {
                                  Methods <- Settings$DataHarmonization$Process %>%
                                                  filter(Table == tablename,
                                                         Feature == featurename) %>%
                                                  as.list()

                                  EligibleValueSet <- dsCCPhos::Meta.Values %>%
                                                          filter(Table == tablename,
                                                                 Feature == featurename) %>%
                                                          pull(Value.Curated)

                                  TransformativeExpressions <- Settings$DataHarmonization$TransformativeExpressions %>%
                                                                    filter(Table == tablename,
                                                                           Feature == featurename)

                                  Dictionary <- Settings$DataHarmonization$Dictionary %>%
                                                    filter(Table == tablename,
                                                           Feature == featurename) %>%
                                                    pull(var = NewValue,
                                                         name = LookupValue)

                                  FuzzyStringMatching <- Settings$DataHarmonization$FuzzyStringMatching %>%
                                                              filter(Table == tablename,
                                                                     Feature == featurename) %>%
                                                              as.list()

                                  Table <- Table %>%
                                                mutate(across(all_of(featurename),
                                                              ~ dsFreda::HarmonizeFeature(Vector = .x,
                                                                                          Methods = Methods,
                                                                                          EligibleValueSet = EligibleValueSet,
                                                                                          TransformativeExpressions = TransformativeExpressions,
                                                                                          Dictionary = Dictionary,
                                                                                          FuzzyStringMatching = FuzzyStringMatching)))
                              }
                          }

                          return(Table)
                       })
