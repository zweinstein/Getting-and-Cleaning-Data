rm(list=ls()) # To clear global environment

### 1. Download the test and training datasets and merge them into one dataset.

## Download and unzip the data
if(!file.exists("UCI HAR Dataset")) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, "UCI_HAR_Dataset.zip", method="curl")
  unzip("./UCI_HAR_Dataset.zip")
}

## Read the feature names
# These names are descriptive variable names to label the data
features <- read.table(file="./UCI HAR Dataset/features.txt")

## Read Test dataset
testX <- read.table(file="./UCI HAR Dataset/test/X_test.txt", colClasses="numeric") # Test set
names(testX) <- features[,2] # Descriptive feature names for the measurements in testX
testY <- read.table(file="./UCI HAR Dataset/test/y_test.txt") # Test labels
names(testY) <- "Activity"
testID <- read.table(file="./UCI HAR Dataset/test/subject_test.txt") # Each row identifies the subject 
                  # who performed the activity for each window sample, in the Test set.
names(testID) <- "Subject"
testData <- cbind(testID, testY, testX) # Complete Test dataset

## Read Training dataset
# Since this data analysis deals with the merged dataset of test and training data, i.e.
# the distinction between Test and Training labels is undesired, I use the same variable
# names for the matching variables in the two datasets.
trainX <- read.table(file="./UCI HAR Dataset/train/X_train.txt", colClasses="numeric") # Training set
names(trainX) <- features[,2] # Descriptive feature names for the measurements in trainX
trainY <- read.table(file="./UCI HAR Dataset/train/y_train.txt") # Training labels
names(trainY) <- "Activity"
trainID <- read.table(file="./UCI HAR Dataset/train/subject_train.txt") # Each row identifies the subject 
                # who performed the activity for each window sample, in the Training set.
names(trainID) <- "Subject"
trainData <- cbind(trainID, trainY, trainX) # Complete Training dataset

## Merge test and training datasets together
allData <- rbind(testData,trainData)

### 2. Extract only the features of the mean and standard deviation for each measurement

meanData <- allData[grep("mean()", names(allData), fixed = TRUE)] # mean
stdData <- allData[grep("std()", names(allData), fixed = TRUE)] # standard deviation
labels <- allData[,1:2]  # Subject and Activity labels

mean_and_std <- cbind(labels,meanData,stdData) 

### 3. Use descriptive activity names to name the activities in the dataset

# This information is available in "activity_labels.txt"
activityID <- read.table(file="./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = F)

# Replace Activity labels (numbers 1-6) with the descriptive activity names
for(i in activityID[[1]]) {
  mean_and_std[mean_and_std$Activity == i,2] <- activityID[[2]][i]
}

### 4. Appropriately lable the dataset with descriptive variable names

# Convert column names to syntactically valid names while retaining the 
# description in the original names for each measurement.
names(mean_and_std) <- make.names(names(mean_and_std), unique = T)

### 5. Create a tidy dataset listing the average of each variable for each
### subject and each activity

library(dplyr)

# Group the dataset by Subject and Activity, and calculate the average
# for all the other columns
tidy <- mean_and_std %>% group_by(Subject,Activity) %>% 
  summarise_each(funs(mean))

# Write the tidy dataset into a .txt file
write.table(tidy,file="./Average.txt", row.names = F)

# Double check the data by reading it back into R
check <- read.table("./Average.txt", header = T)

