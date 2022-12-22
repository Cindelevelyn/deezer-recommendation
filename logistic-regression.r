library(readr)
library(dplyr)
library(caret)

data <- read.csv("C:/Users/cinde/Downloads/db/trainData.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/cinde/Downloads/db/testData.csv", stringsAsFactors = FALSE)

rows.to.delete <- 0.4 * nrow(data)
data <- data[1:rows.to.delete,]

data[is.na(data)] <- 0
test[is.na(test)] <- 0

model <- glm(as.factor(is_listened) ~ ., data = data, family = binomial)

y_pred <- predict(model, test, type = "response")

output <- data.frame(sample_id = sample_id, is_listened = y_pred)
write.csv(output, file = "./output_logistic.csv", row.names = FALSE)
