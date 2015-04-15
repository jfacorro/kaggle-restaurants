library("e1071")
library("rpart")
library("cvTools")

generate_output <- function(model, test_set, filename) {
  predictions <- predict(model, test_set)
  indexes <- 0:(length(predictions) - 1)
  predictionsStr <- paste(indexes, formatC(predictions, format = "f", digits = 1), sep = ",")
  file_conn <- file(filename)
  writeLines(c("Id,Prediction", predictionsStr), file_conn)
  close(file_conn)
}

process_dataset <- function(dataset) {
  dataset$Open.Date <- Sys.Date() - as.Date(dataset$Open.Date, format = "%m/%d/%Y") 
  dataset$Type[Type == "MB"] <- "IL"
  datatest$Type <- droplevels(datatest$Type)
}

load_data <- function() {
  train <- read.csv("~/dev/kaggle-restaurants/resources/train.csv")
  test <- read.csv("~/dev/kaggle-restaurants/resources/test.csv")
}

main <- function() {
  ignore_fields <- c(-1, -3)
  
  svm.model <- svm(revenue ~ ., data = train[, ignore_fields], 
                   cost = 10,
                   gamma = 0.001,
                   fitted = TRUE,
                   type = "eps-regression", 
                   kernel = "radial")
  cvFit(svm.model, data = train, y = train$revenue, K = 5, R = 10)
  # svm.preds <- predict(svm.model, newdata = test[, ignore_fields])
  
  rpart.model <- rpart(revenue ~ ., data = train[, ignore_fields], control = rpart.control(cp = 0.05, minsplit = 35))
  cvFit(rpart.model, data = train, y = train$revenue, K = 5, R = 10)
  # rpart.preds <- predict(rpart.model, test[, ignore_fields])
  
  # generate_output(svm.model, test[, ignore_fields], "svm.output")
  # generate_output(rpart.model, test[, ignore_fields], "rpart.output")
  str(train)
  str(test)
  length(unique(train$City))
  length(unique(test$City))
}
