library(class)
library(tree)
library(readr)
library(caret)
library(lubridate)

train <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/train.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/cinde/Documents/facul/8_PERIODO/TSI/trab_2/test.csv", stringsAsFactors = FALSE)

head(train)

train <- na.omit(train)

rows.to.delete <- 0.02 * nrow(train)
train <- train[1:rows.to.delete,]

##features novas
View(train)

#1 - é fã? (se o usuario ouve o mesmo artista varias vezes, considera-se um ouvinte casual / fã, o que significa que ele pode querer ouvir mais daquele artista)
is_fan <-  ifelse(duplicated(train$artist_id) == TRUE & duplicated(train$user_id) == TRUE, 1,0)
train$is_fan <- is_fan

#2 - caso seja não seja fã mas a musica contem baixa duração
genero_duracao <- ifelse(train$is_fan == 0 & train$media_duration <= 200, 1,0)
train$genero_duracao <- genero_duracao

#3 - periodo padrao de ferias? (janeiro/julho/dezembro)
data <- ymd(train$release_date)
train$release_date <- data

ferias <- ifelse(month(train$release_date) == 1 | month(train$release_date) == 7 | month(train$release_date) == 12, 1,0)
train$ferias <- ferias

#4 - periodo do dia que ouviu a musica (dia, noite)
hora <- (as_datetime(train$ts_listen))

train$periodo_dia <- ifelse(hour(hora) <= 18 & hour(hora) >= 6, 1,0)

#5 - musica agitada que possui radio
train$agitada_radio <- ifelse(train$song_bpm >= 120 & train$has_radio == 1, 1,0)


##### modelos

# random forest
rfm = randomForest(is_listened~.,data = train)

# predição
is_listened_pred <- predict(rfm, test)
test$is_listened_pred = is_listened_pred

# fazendo matriz de confusão
mdc = table(test$is_listened,test$is_listened_pred)
mdc

# acuracia da classificação
adc = sum(diag(mdc)/sum(mdc))
adc



