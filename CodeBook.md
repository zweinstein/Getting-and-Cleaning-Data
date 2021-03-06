# CodeBook 
## Data Collected from the Accelerometers from Samsung Galaxy S Smartphones

### License:
Use of this dataset in publications must be acknowledged by referencing the following publication:

_Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012_

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

### Data Set Information
The original data and full description is available at: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, data were collected for 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The features selected for the original database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

### Variations from the Original Data

__My current analysis has merged the original test and training datasets into one dataset, and performed data cleaning to generate a tidy dataset, "Average.txt", that only contains the average of the mean and standard deviation of each movement feature for each subject and activity.__ I wrote the R script "run_analysis.R" to download the original data and perform this analysis. The details of this analysis are explained in ReadMe.md as well as the comments in the run_analysis.R script. 

### Variable Descriptions in "Average.txt"

Each row of the dataset "Average.txt" contains:
* Column 1: Subject ID
* Column 2. Activity
* Columns 3-68: 66 features of the activity, which are average values of the mean and standard devations in the original data (in both time and frequency domains). __These features are bounded within [-1,1] since the original data have been normalized.__ The following table lists the detailed description for each variable:

Variable        | Description
----------------|-------------
Subject	        | An identifier of the subject who carried out the experiment.
Activity        | The activity performed
tBodyAcc.mean...X	| Mean time of body acceleration in X direction
tBodyAcc.mean...Y	| Mean time of body acceleration in Y direction
tBodyAcc.mean...Z	| Mean time of body acceleration in Z direction
tGravityAcc.mean...X	| Mean time of gravity acceleration in X direction
tGravityAcc.mean...Y	| Mean time of gravity acceleration in Y direction
tGravityAcc.mean...Z	| Mean time of gravity acceleration in Z direction
tBodyAccJerk.mean...X	| Mean time of body acceleration jerk in X direction
tBodyAccJerk.mean...Y	| Mean time of body acceleration jerk in Y direction
tBodyAccJerk.mean...Z	| Mean time of body acceleration jerk in Z direction
tBodyGyro.mean...X	| Mean body gyroscope measurement in X direction
tBodyGyro.mean...Y	| Mean body gyroscope measurement in Y direction
tBodyGyro.mean...Z	| Mean body gyroscope measurement in Z direction
tBodyGyroJerk.mean...X	| Mean jerk signal of body in X direction
tBodyGyroJerk.mean...Y	| Mean jerk signal of body in Y direction
tBodyGyroJerk.mean...Z	| Mean jerk signal of body in Z direction
tBodyAccMag.mean..	| Mean magnitude of body acceleration
tGravityAccMag.mean..	| Mean magnitude of gravity acceleration
tBodyAccJerkMag.mean..	| Mean magnitude of body acceleration jerk
tBodyGyroMag.mean..	| Mean magnitude of body gyroscope measurement
tBodyGyroJerkMag.mean..	| Mean magnitude of body gyroscope jerk measurement
fBodyAcc.mean...X | Mean frequency of body acceleration in X direction
fBodyAcc.mean...Y | Mean frequency of body acceleration in Y direction
fBodyAcc.mean...Z | Mean frequency of body acceleration in Z direction
fBodyAccJerk.mean...X	| Mean frequency of body accerlation jerk in X direction
fBodyAccJerk.mean...Y	| Mean frequency of body accerlation jerk in Y direction
fBodyAccJerk.mean...Z	| Mean frequency of body accerlation jerk in Z direction
fBodyGyro.mean...X	| Mean frequency of body gyroscope measurement in X direction
fBodyGyro.mean...Y	| Mean frequency of body gyroscope measurement in Y direction
fBodyGyro.mean...Z	| Mean frequency of body gyroscope measurement in Z direction
fBodyAccMag.mean..	| Mean frequency of body acceleration magnitude
fBodyBodyAccJerkMag.mean..	| Mean frequency of body acceleration jerk magnitude
fBodyBodyGyroMag.mean..	| Mean frequency of magnitude of body gyroscope measurement
fBodyBodyGyroJerkMag.mean..	| Mean frequency of magnitude of body gyroscope jerk measurement
tBodyAcc.std...X  | Standard deviation of time of body acceleration in X direction
tBodyAcc.std...Y	| Standard deviation of time of body acceleration in Y direction
tBodyAcc.std...Z	| Standard deviation of time of body acceleration in Z direction
tGravityAcc.std...X  | Standard deviation of time of gravity acceleration in X direction
tGravityAcc.std...Y	| Standard deviation of time of gravity acceleration in Y direction
tGravityAcc.std...Z	| Standard deviation of time of gravity acceleration in Z direction
tBodyAccJerk.std...X  | Standard deviation of time of body acceleration jerk in X direction
tBodyAccJerk.std...Y	| Standard deviation of time of body acceleration jerk in Y direction
tBodyAccJerk.std...Z	| Standard deviation of time of body acceleration jerk in Z direction
tBodyGyro.std...X  | Standard deviation of body gyroscope measurement in X direction
tBodyGyro.std...Y	| Standard deviation of body gyroscope measurement in Y direction
tBodyGyro.std...Z	| Standard deviation of body gyroscope measurement in Z direction
tBodyGyroJerk.std...X  | Standard deviation of jerk signal of body in X direction
tBodyGyroJerk.std...Y	| Standard deviation of jerk signal of body in Y direction
tBodyGyroJerk.std...Z	| Standard deviation of jerk signal of body in Z direction
tBodyAccMag.std..  | Standard deviation of magnitude of body acceleration
tGravityAccMag.std..  | Standard deviation of gravity acceleration magnitude
tBodyAccJerkMag.std..  | Standard deviation of magnitude of body acceleration jerk
tBodyGyroMag.std..  | Standard deviation of magnitude of body gyroscope measurement
tBodyGyroJerkMag.std..  | Standard deviation of magnitude of body gyroscope jerk measurement
fBodyAcc.std...X  | Standard deviation of frequency of body acceleration in X direction
fBodyAcc.std...Y	| Standard deviation of frequency of body acceleration in Y direction
fBodyAcc.std...Z	| Standard deviation of frequency of body acceleration in Z direction
fBodyAccJerk.std...X  | Standard deviation frequency of body accerlation jerk in X direction
fBodyAccJerk.std...Y	| Standard deviation frequency of body accerlation jerk in Y direction
fBodyAccJerk.std...Z	| Standard deviation frequency of body accerlation jerk in Z direction
fBodyGyro.std...X  | Standard deviation frequency of body gyroscope measurement in X direction
fBodyGyro.std...Y	| Standard deviation frequency of body gyroscope measurement in Y direction
fBodyGyro.std...Z	| Standard deviation frequency of body gyroscope measurement in Z direction
fBodyAccMag.std..  | Standard deviation of frequency of body acceleration magnitude
fBodyBodyAccJerkMag.std..	| Standard deviation of frequency of body acceleration jerk magnitude
fBodyBodyGyroMag.std..	| Standard deviation of frequency of magnitude of body gyroscope measurement
fBodyBodyGyroJerkMag.std..	| Standard deviation frequency of magnitude of body gyroscope jerk measurement
