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
  dataset$Open.Date <- as.integer(dataset$Open.Date)
  dataset$Type[dataset$Type == "MB"] <- "IL"
  dataset$Type <- droplevels(dataset$Type)
  dataset
}

load_data <- function() {
  train <- read.csv("~/dev/kaggle-restaurants/resources/train.csv")
  test <- read.csv("~/dev/kaggle-restaurants/resources/test.csv")
}

feature_selection <- function(dataset) {
  feat_vars <- var(dataset, y = dataset$revenue)
  feat_vars[, 1] <- abs(feat_vars)
  feat_vars[order(feat_vars[,1]),]
  # feat_vars
}

main <- function() {
  ignore_fields <- -c(1, 3)
  
  ########################################################################
  # Create a new feature that indicates whether the sample is an outlier
  
  train.sd <- sd(train$revenue)
  train.mean <- mean(train$revenue)
  outliers1 <- (abs(train$revenue - train.mean) <= train.sd)
  outliers2 <- (abs(train$revenue - train.mean) > train.sd) & (abs(train$revenue - train.mean) <= (train.sd * 2))
  outliers3 <- (abs(train$revenue - train.mean)) > (train.sd  * 2)
  train[outliers1, ]$Outlier <- 1
  train[outliers2, ]$Outlier <- 2
  train[outliers3, ]$Outlier <- 3
  train$Outlier <- factor(train$Outlier, labels = c("1", "2", "3"))
  
  svm.outliers <- svm(Outlier ~ ., data = train[, c(-43, ignore_fields)],
                      cost = 100,
                      gamma = 0.0001,
                      fitted = TRUE,
                      type = "C-classification",
                      kernel = "linear")

  svm.out.preds <- predict(svm.outliers, newdata = test[, ignore_fields])
  length(svm.out.preds[svm.out.preds == "1"])
  length(svm.out.preds[svm.out.preds == "2"])
  length(svm.out.preds[svm.out.preds == "3"])
  test$Outlier <- svm.out.preds
  
  svm.model <- svm(revenue ~ ., data = train[, ignore_fields],
                   cost = 100,
                   gamma = 0.0001,
                   fitted = TRUE,
                   type = "eps-regression",
                   kernel = "radial")
  cvFit(svm.model, data = train, y = train$revenue, K = 5, R = 10)
  # svm.preds <- predict(svm.model, newdata = test[, ignore_fields])
  # generate_output(svm.model, test[, ignore_fields], "~/svm.output")
  
  rpart.model <- rpart(revenue ~ ., data = train[, ignore_fields], control = rpart.control(cp = 10, minsplit = 5))
  cvFit(rpart.model, data = train, y = train$revenue, K = 5, R = 10)
  # rpart.preds <- predict(rpart.model, test[, ignore_fields])
  # generate_output(rpart.model, test[, ignore_fields], "rpart.output")
  str(train)
  str(test)
  length(unique(train$City))
  length(unique(test$City))
  
  ######################################################
  ### Remove samples that are outliers
  
  train.trimmed <- train[train$revenue < 1.0e+07, ]
  svm.model <- svm(revenue ~ .
                   , data = train.trimmed[, ignore_fields]
                   , cost = 100
                   , gamma = 0.0001
                   , fitted = TRUE
                   #, type = "eps-regression"
                   #, kernel = "radial"
                   )
  cvFit(svm.model, data = train.trimmed, y = train.trimmed$revenue, K = 10, R = 10)
  # generate_output(svm.model, test[, ignore_fields], "~/svm.output")
  # svm.preds <- predict(svm.model, newdata = test[, ignore_fields])
  
  rpart.model <- rpart(revenue ~ ., 
                       data = train.trimmed[, ignore_fields],
                       control = rpart.control(cp = 0.05, minsplit = 5))
  cvFit(rpart.model, data = train.trimmed, y = train.trimmed$revenue, K = 5, R = 10)
  # rpart.preds <- predict(rpart.model, test[, ignore_fields])
}
