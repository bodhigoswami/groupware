require(caret) ; require(ggplot2) ; require(dplyr); require(e1071)

rm(list=ls())
train <- read.csv("G:/My Drive/Data Science/R - Working Directories/Practice/Practice V1/data/groupware/pml-training.csv",
                     na.strings = c("","NA"))
test <- read.csv("G:/My Drive/Data Science/R - Working Directories/Practice/Practice V1/data/groupware/pml-testing.csv",
                     na.strings = c("","NA"))
test$classe <- rep("Hello",nrow(test))
processme <- function(dataset) {

  dataset[is.na(dataset)==TRUE] <- 0
  dataset[dataset == "#DIV/0!"] <- 0

  dataset[is.na(dataset)==TRUE] <- 0
  dataset[dataset == "#DIV/0!"] <- 0

  dataset$cvtd_timestamp <- NULL

  dataset$new_window <- ifelse(dataset$new_window == "yes", 1, 0)

  character_ok <- c("user_name","classe")
  dataset_charok <- dataset[,character_ok]
  dataset_tonum <- dataset[,!(names(dataset) %in% (character_ok))]
  dataset_tonum <- data.frame(sapply(dataset_tonum, as.numeric))

  dataset<- cbind(dataset_tonum,dataset_charok); rm(dataset_tonum,dataset_charok,character_ok)
  dataset$classe <- as.factor(dataset$classe)

  return(dataset)
}

train <- processme(train)
test <- processme(test)
test$classe <- NULL

trainID <- createDataPartition(train$classe, p=0.7, list = FALSE)
training <- train[trainID,] ; val <- train[-trainID,]

myControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#Train data
modFit.gbm <- train(classe~.-X-user_name, data = training,
                    tuneGrid = expand.grid(n.trees = 150,
                                           interaction.depth=3,
                                           shrinkage = 0.1,
                                           n.minobsinnode = 10),
                    method = "gbm",
                    trControl = myControl, verbose = FALSE)

pred_gbm <- predict(modFit.gbm, val)
confusionMatrix(pred_gbm,val$classe)
confusionMatrix(predict(modFit.gbm, training),training$classe)


#Running this model on entire data
#Train data
modFit.gbm.fullmodel <- train(classe~.-X-user_name, data = train,
                    tuneGrid = expand.grid(n.trees = 150,
                                           interaction.depth=3,
                                           shrinkage = 0.1,
                                           n.minobsinnode = 10),
                    method = "gbm",
                    trControl = myControl, verbose = FALSE)

pred <- predict(modFit.gbm.fullmodel,test)
predicted_data <- data.frame(X = test$X,
                             user_name = test$user_name,
                             classe = pred)


#Exploratory Data Analysis

g = ggplot (data=train, aes(x = X, y = pitch_arm))
g = g + geom_line(col="red")
g
