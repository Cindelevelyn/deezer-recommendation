library(readr)
library(dplyr)
library(caret)
library(xgboost)

train <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/train.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/test.csv", stringsAsFactors = FALSE)

rows.to.delete <- 0.4 * nrow(train)
train <- train[1:rows.to.delete,]

train[is.na(train)] <- 0
test[is.na(test)] <- 0

sample_id <- test$sample_id
test$sample_id <- NULL

train.x <- model.matrix(is_listened ~ ., data = train)

options(na.action='na.pass')
test.x <- model.matrix(is_listened ~ ., data = test)

dim(test.x)

model.xgb <- xgboost( data = data.matrix(train.x[,-1]),
                      label= as.numeric(as.character(train$is_listened)),
                      eta = 0.1,
                      max_depth = 20,
                      nround = 50,
                      objective = "binary:logistic")

y_pred <- predict(model.xgb, data.matrix(test.x[,-1]))

output <- data.frame(sample_id = sample_id, is_listened = y_pred)
write.csv(output, file = "./output_simple_model.csv", row.names = FALSE)
