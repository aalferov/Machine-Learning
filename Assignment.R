library(caret)

## Download files with training and test data & load them in data frames
setwd("D:/Coursera/Data Science/Machine Learning")

fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileName <- "pml-training.csv"

if(!file.exists(fileName))
{  
  download.file(fileUrl, fileName)
}

trainingDS <- read.csv(fileName, stringsAsFactors = FALSE)


fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
fileName <- "pml-testing.csv"

if(!file.exists(fileName))
{  
  download.file(fileUrl, fileName)
}

testingDS <- read.csv(fileName, stringsAsFactors = FALSE)

## Clean up training dataset

dim(trainingDS) #So we have 19622 observations of 160 variables

summary(trainingDS)

#Let's remove variables with NA's
trainingDS <-trainingDS[,colSums(is.na(trainingDS)) == 0]

#Let's remove variables "X", "raw_timestamp_part_1", "raw_timestamp_part_2" and "cvtd_timestamp" as they shouldn't be influantional on predicted result by their nature 
trainingDS <- subset(trainingDS, select = -c(X, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))

str(trainingDS)

#TODO: remove variables with "", "#DIV/0!" values

## Remove factor variables with a single value as they have no influence on prediction
#trainingDS <- subset(trainingDS, select = -c(kurtosis_yaw_belt, skewness_yaw_belt, kurtosis_yaw_dumbbell, skewness_yaw_dumbbell,
                                         amplitude_yaw_dumbbell, kurtosis_yaw_forearm, skewness_yaw_forearm, amplitude_yaw_forearm))

str(trainingDS)

ctrl <- rfeControl(functions = lmFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
lmProfile <- rfe(subset(trainingDS, -classe), trainingDS$classe, sizes = subsets, rfeControl = ctrl)


