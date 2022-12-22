library(gbm)
library(readr)
library(dplyr)
library(caret)

#train <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/train.csv", stringsAsFactors = FALSE)
train <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/train40.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/test.csv", stringsAsFactors = FALSE)

train[is.na(train)] <- 0
test[is.na(test)] <- 0

sample_id <- test$sample_id
test$sample_id <- NULL

train$is_listened <- as.character(train$is_listened)

model_gbm = gbm(is_listened ~.,
                data = train,
                n.trees = 500,
                n.minobsinnode = 30,
                distribution = "bernoulli")

pred_y = predict.gbm(model_gbm, test, type = "response")

output <- data.frame(sample_id = sample_id, is_listened = pred_y)
write.csv(output, file = "./output_gradient.csv", row.names = FALSE)

