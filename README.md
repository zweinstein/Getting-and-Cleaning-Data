# ReadMe
## Getting and Cleaning Data Collected from the Accelerometers from Samsung Galaxy S Smartphones 

__This repo is for the course project of "Getting and Cleaning Data" organized by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD, at Johns Hopkins, through Coursera.__

### Content

This repo contains the following files:
  + A markdown file called "ReadMe.md" (this file) describing the objectives of this project and the connections between the files in this repo.
  + A R script called "run_analysis.R", to get and clean the data. The comments in the R script file explicitly explains each step of getting and cleaning the data.
  + A data file called "Average.txt", which is the output file of the R script. The data are described below and in more detail in the "CodeBook.md" file.
  + A markdown file called "CodeBook.md" describing the data, variables, and transformations performed to obtain the final data (output of the R script).

### Objectives

One of the most exciting areas in all of data science right now is wearable computing - see for example: 

http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/. 

Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

__The goal of this project is to get and clean the data from the link above, to output a tidier dataset that containig only the average of the mean and standard deviation of each movement feature for each subject and activity.__

### Explanation of run_analysis.R

The script run_analysis.R performs the following:

1. Download the zipped "UCI HAR Dataset" from the link above, and unzip it into the "UCI HAR Dataset" folder under the working directory (if the file doesn't exist yet). Then it merges the training and the test sets to create one data set (by cbind and rbind functions). At this step, each column of the data is also properly named, i.e. I used "Subject" for "subject_test.txt" and "subject_train.txt" data, "Activity" for "y_test.txt" and "y_train.txt" data, and extracted the names from "features.txt" for different features (data from "X_test.txt" and "X_train.txt").

2. Extracts only the measurements on the mean and standard deviation for each measurement (by grep function).

3. Uses descriptive activity names to name the activities in the data set, based on the information in "activity_labels.txt". 

4. Appropriately labels the data set with descriptive variable names. Converts the variable names to syntactically valid names while retaining the description in their original names (by make.names function).

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. This was done by using the group_by and summarise_each functions available in the dplyr package. 

__The tidy dataset is written as "Average.txt" file under the working directory. This data can be read into R by: read.table("./Average.txt", header = T)__

