library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

## Download files with training and test data & load them in data frames
setwd("D:/Coursera/Data Science/Machine-Learning")

fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileName <- "pml-training.csv"

if(!file.exists(fileName))
{  
  download.file(fileUrl, fileName)
}

trainingDS <- read.csv(fileName, na.strings=c("NA","#DIV/0!", ""))


fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
fileName <- "pml-testing.csv"

if(!file.exists(fileName))
{  
  download.file(fileUrl, fileName)
}

testingDS <- read.csv(fileName, na.strings=c("NA","#DIV/0!", ""))

## Clean up training dataset

dim(trainingDS) 
dim(testingDS) 
#So we have 19622 observations of 160 variables in training dataset and 20 observations of 20 variables in testing dataset

summary(trainingDS)

#Remove variables "user_name", "X", "raw_timestamp_part_1", "raw_timestamp_part_2" and "cvtd_timestamp" as they shouldn't be influantional on predicted result by their nature 
trainingDS <- subset(trainingDS, select = -c(user_name, X, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))
testingDS <- subset(testingDS, select = -c(user_name, X, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))

str(trainingDS)

#Remove variables with NA's
trainingDS <-trainingDS[ ,colSums(is.na(trainingDS)) == 0]
testingDS <-testingDS[ ,colSums(is.na(testingDS)) == 0]

str(trainingDS)

#Exclude predictors with near zero variance as they doesn't affect prediction result
nzvCol <- nearZeroVar(trainingDS)
trainingDS <- trainingDS[ ,-nzvCol]

#Create data partitions
set.seed(210615)
trainset <- createDataPartition(trainingDS$classe, p = 0.8, list = FALSE)
training <- trainingDS[trainset, ]
validation <- trainingDS[-trainset, ]

#Train first model via Decision Tree algorythm
modelDT <- rpart(classe ~ ., data = training, method = "class")

#Make a prediction
predictionDT <- predict(modelDT, validation, type = "class")

#Plot the decision tree
rpart.plot(modelDT, main = "Classification Tree", extra = 102, under = TRUE, faclen = 0)

#Build the Confusion Matrix to check the result of prediction
confusionMatrix(predictionDT, validation$classe)

#Train second model via Random Forest algorythm
modelRF <- randomForest(classe ~. , data = training, method = "class")
