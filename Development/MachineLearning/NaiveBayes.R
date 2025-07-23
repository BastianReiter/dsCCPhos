

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Naive Bayes Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(caret)
library(caTools)
library(dplyr)
library(e1071)
library(stringr)
library(text2vec)
library(tidyr)
library(TinkerLab)


# Clean class data

CleanData <- TinkerLab::ATCCodes %>%
                  distinct() %>%
                  filter(str_starts(ATCCode, pattern = "L")) %>%
                  arrange(NameGerman) %>%
                  pull(NameGerman)



TestData <- TinkerLab::MakeDirty(CleanData) %>%
                    select(-Dirty) %>%
                    pivot_longer(cols = !Original,
                                 names_to = NULL,
                                 values_to = "Dirty")


TokenizedData <- TestData %>%
                      rowwise() %>%
                      mutate(Token = list(GetNGrams(Original, n = 3))) %>%
                      ungroup() %>%
                      unnest_wider(Token, names_sep = "") %>%
                      select(Original,
                             all_of(paste0("Token", 1:10)))   # Only take first 10 n-grams into account


SplitIndicator <- sample.split(TokenizedData$Original, SplitRatio = 0.7)
TrainingData <- subset(TokenizedData, SplitIndicator == TRUE)
PredictionData <- subset(TokenizedData, SplitIndicator == FALSE)



Classifier <- naiveBayes(Original ~ ., data = TrainingData)


Predictions <- predict(Classifier, newdata = PredictionData)


ConfusionMatrix.Table <- table(PredictionData$Original, Predictions)



ConfusionMatrix <- confusionMatrix(ConfusionMatrix.Table)



PredictionsRW <- predict(Classifier, newdata = )



# Iterator.Training <- itoken(iterable = TrainingData,
#                             tokenizer = word_tokenizer,
#                             n_chunks = 3,
#                             progressbar = FALSE)
#
# Iterator.Prediction <- itoken(iterable = PredictionData,
#                               tokenizer = word_tokenizer,
#                               n_chunks = 3,
#                               progressbar = FALSE)
#
# Vocabulary <- create_vocabulary(it = Iterator.Training)
#
# Vectorizer <- vocab_vectorizer(Vocabulary)


#
# DTM.Training <- create_dtm(it = Iterator.Training,
#                            vectorizer = Vectorizer)
#
# DTM.Prediction <- create_dtm(it = Iterator.Prediction,
#                              vectorizer = Vectorizer)


#
# Model <- naiveBayes(x = as.matrix(DTM.Training),
#                     y = CleanData)
#
# Predictions <- predict(Model, as.matrix(DTM.Prediction))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# RawData <- RawDataSet$RDS_SystemicTherapy$systemische_therapie_substanzen

# TestData pre-processing

# PredictData <- RawData %>%
#                 str_split("[,/]") %>%
#                 unlist() %>%
#                 unique()
#
# View(as.data.frame(TestData))
