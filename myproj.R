# http://groupware.les.inf.puc-rio.br/har
# http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf

library(caret)
library(rpart)
library(rattle)
set.seed(42) # For reproductibility

data <- read.csv('pml-training.csv', header = TRUE)
#data_test <- read.csv('pml-testing.csv', header = TRUE)
inTrain  <- createDataPartition(data$classe, p=0.6, list=FALSE)
training <- data[inTrain,]
testing  <- data[-inTrain,]

# Remove columns that are not predictors: X, user_name, raw_timestamp_part_1,
# raw_timestamp_part_2, cvtd_timestamp, new_window, num_window
training <- training[, -c(1:7)]
testing  <- testing[, -c(1:7)]
nzv_id <- nearZeroVar(training)
# Remove 60 variables with near zero variance : 93 predictors
training <- training[, -nzv_id]
testing  <- testing[, -nzv_id]
# Remove 41 variables with more than 90% of NA values
nb_lin <- dim(training)[[1]]
nb_col <- dim(training)[[2]]
na_id <- sapply(1:nb_col, function(x) {
	na_ratio <- sum(is.na(training[, x])) / nb_lin })
is_na <- which(na_id > .9)
# We now gave only 52 predictors
training <- training[, -is_na]
testing  <- testing[, -is_na]

model_rpart <- train(classe ~ ., data = training, method = 'rpart')
pred_rpart <- predict(model_rpart, training)
cm <- confusionMatrix(pred_rpart, training$classe)
cm$overall[[1]]
pred_rpart_test <- predict(model_rpart, testing)
# Not great accuracy = .49 on test data
confusionMatrix(pred_rpart_test, testing$classe)
# No D in the model?
fancyRpartPlot(model_rpart$finalModel)

model_rf <- train(classe ~ ., data = training, method = 'rf', 
	trcontrol = trainControl(method = 'cv', number = 3))
