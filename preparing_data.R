setwd("/Users/Falrrema/MEGA/S2DS/StratifiedMedicalRepo/word_level_lstm/data")
library(caTools)

body <- readLines(paste("bodyQueries.txt", sep = " ", collapse = "<eoc>"))
train <- readLines("ptb.train.txt")
answers <- readLines(paste("answers.txt", sep = " ", collapse = "<eoc>"))

pair <- data.frame(body, answers, stringsAsFactors = F)

pair$body <- as.character(pair$body)
pair$answers <- as.character(pair$answers)
str(pair)

set.seed(30)
splitTrainTest = sample.split(pair[,2], SplitRatio = 0.6)

train = subset(pair, splitTrainTest ==TRUE)
testing = subset(pair, splitTrainTest ==FALSE)

splitTestValidation = sample.split(testing$answers, SplitRatio = 0.5)

test = subset(testing, splitTestValidation ==TRUE)
validation = subset(testing, splitTestValidation ==FALSE)

train <- train$body
test <- test$body
validation <- validation$body

write.table(train, file = "trainLSTM.txt")
write.table(test, file = "testLSTM.txt")
write.table(validation, file = "validationLSTM.txt")

