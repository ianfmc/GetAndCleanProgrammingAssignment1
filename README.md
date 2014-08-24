#Getting and Cleaning Data
## Course Project: UCI HAR Dataset Summary

### Contents
- README.md: this file
- run_analysis.R: code to read and condition the dataset
- Codebook.md: variable descriptions

### Directions
1. Place the UCI HAR data set folder (located at [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip]) in same directory as script
2. Run the script, `run_analysis()` found in the file run_analysis.R
3. The script has five steps:

- reads the metadata files (feature variable names and activity types)
- reads and merges the training and test data
- extracts only those features that measure the mean or standard deviation
- substitues the activities names for the activity number in each measurement
- changes the variable names to be more descriptive
- writes a *tidy* summary of the data set with the average of each variable

The name of the file produced is **Tidy-UCI-HAR-Summary.txt**

date: 24-August-2014
