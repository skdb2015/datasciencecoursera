# Analysis of Human Activity Recognition tests using smartphones
This analysis analyzes the raw data downloaded from the UCI human activity recognition dataset at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.

## Analysis description
It combines the training and test data sets into a single data set, extracts just the mean and standard deviation measurements from th 561 measurements taken for each measurements in the experiment and summarizes the data the following way:

For each subject and activity pair, the average of the mean and standard deviation feature measurements. Essentially, each (subject ID, activity) tuple has a unique row in the resulting dataset. Since the experiment was carried out on 30 subjects over 6 kinds of activities WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING), the final dataset has 180 rows containing summary data.

## files required to run the analysis
* run_analysis.R : Script to run the analysis as described in Analysis description above
* UCI HAR data set : The raw data that has the measurements that the analysis will run on

## Instructions for running the analysis
1. Copy/download the run_analysis.R script
2. Download the raw data from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip to a subfolder called "Data" of the folder where you have copied the run_analysis.R script to
3. Unzip the raw data
4. Run the run_analysis.R script.
5. The resulting data set will be written to analysis_result.txt which will be in the same folder as run_anlysis.R. they can be read into R using Read

## Final dataset files
* analysis_result.txt : final dataset where each (subject ID, activity) tuple has a unique row in the resulting dataset. 