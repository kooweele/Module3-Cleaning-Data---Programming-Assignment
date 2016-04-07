#######################################################################
## Pre-condition: Update the 3 paths specified in the following methods:
##  - setWorkingPath_UCIHARDatasetPath
##  - setWorkingPath_TrainingData
##  - setWorkingPath_TestData
##
## To run, execute "run_analysis" in R, which do these:- 
##      1. Merges the training and the test sets to create one data set.
##      2. Extracts only the measurements on the mean and standard deviation for each measurement.
##      3. Uses descriptive activity names to name the activities in the data set
##      4. Appropriately labels the data set with descriptive variable names.	
##      5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Post Condition:
## Result is a file named "tidy_data.txt" in your working R homedir
## 
#######################################################################

#######################################################################
## Specify the unzipPath as the path where you have extracted the ZIP file of : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## 
setWorkingPath_UCIHARDatasetPath <- function() {
        unzipPath <- "D:/Users/Koo Wee Leong/My Documents/My Education/Coursera/Data Science - John Hopkins/R-Workspace/Cleansing Data - Programming Assignment/UCI HAR Dataset"
        setwd(unzipPath)       
}

## Specify the trainingDataPath as the path where the training data path is 
setWorkingPath_TrainingData <- function() {
        trainingDataPath <- "D:/Users/Koo Wee Leong/My Documents/My Education/Coursera/Data Science - John Hopkins/R-Workspace/Cleansing Data - Programming Assignment/UCI HAR Dataset/train"
        setwd(trainingDataPath)       
}
## Specify the testDataPath as the path where the test data path is 
setWorkingPath_TestData <- function() {
        testDataPath <- "D:/Users/Koo Wee Leong/My Documents/My Education/Coursera/Data Science - John Hopkins/R-Workspace/Cleansing Data - Programming Assignment/UCI HAR Dataset/test"
        setwd(testDataPath)       
}
############ End of Specifying Paths ############ 

######### Main program is run_analysis()
run_analysis <- function() {
        ## Load all R libraries taught in the module
        library(lubridate)
        library(data.table)
        library(dplyr)
        library(plyr)
        library("stringr")
        
        clearMemory()
        ### Set path -- modify to suit your own env
        R_homedir <- "D:/Users/Koo Wee Leong/My Documents/My Education/Coursera/Data Science - John Hopkins/R-Workspace/Cleansing Data - Programming Assignment/"
        setwd(R_homedir)
        
        ### The extract functions will perform both step 1 & 3.
        ### 1. Merges the training and the test sets to create one data set.
        ### 3. Uses descriptive activity names to name the activities in the data set
        
        test_data <- extract_test_data_set()
        train_data <- extract_training_data_set()
        final_data <- rbind(test_data,train_data) 
        
        ## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        ##measurements_column_data <- final_data[,4:564]
        first_three_columns <- final_data[,1:2] ## retain 1st 2 columns which are "subject_id" and "activity_names"
        measurements_data_of_mean_and_std <- final_data[,grep("mean|std" , colnames(final_data))] ## this will include columns of "meanFreq" as well
        measurements_data_of_mean_and_std <- measurements_data_of_mean_and_std[,grep("Freq" , colnames(measurements_data_of_mean_and_std), invert = TRUE)] ## this will exclude columns of "meanFreq" 
        result <- cbind(first_three_columns,measurements_data_of_mean_and_std)
  
        ##  4. Appropriately labels the data set with descriptive variable names.
        ##  - remove brackets in column names
        ##  - use lower-case in column names
        names(result) <- gsub("\\(|\\)", "", names(result)) ## purge paranthesis in column names
        names(result) <- tolower(names(result))
                
        ## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        excluded_columns = which(names(result) %in% c("subject_id", "activity_name"))
        new_tidy_result <- ddply(result, .(subject_id, activity_name), .fun=function(x){ colMeans(x[,-excluded_columns]) })
        write.table(new_tidy_result,file="tidy_data.txt", row.names = FALSE)
        new_tidy_result
}

## Read vector of 561 features names 
extract_features_names <- function() {
        currdir <- getwd()
        setWorkingPath_UCIHARDatasetPath()
        feature_list_names <- read.table("./features.txt",sep = "", blank.lines.skip = TRUE,stringsAsFactors = FALSE)
        setnames(feature_list_names,c("count","feature_name"))
        setwd(currdir)
        feature_list_names$feature_name
}

## Read all activities names and labels performed by the 30 volunteers
extract_activity_names <- function() {
        currdir <- getwd()
        setWorkingPath_UCIHARDatasetPath()
        activity_names <- read.table("./activity_labels.txt",sep = "", blank.lines.skip = TRUE,stringsAsFactors = FALSE)
        setnames(activity_names,c("activity_id","activity_name"))
        setwd(currdir)
        activity_names
}

#######################################################################
## Purpose: R function that read and return raw Training Data Set
#######################################################################
extract_training_data_set <- function() {
        currdir <- getwd()
        setWorkingPath_TrainingData()
        
        ## subjects_vector <- extract_subject_train()
        ## activityPeformed_vector <- extract_ytrain()
        ## extract_activity_names_vector <- extract_activity_names()
        ## final_activityPeformed_vector <- merge(extract_activity_names_vector, activityPeformed_vector, by.x="activity_id", by.y="activity_id")
        ## measurements_of_all_features_vector <- extract_xtrain()
        ## result <- cbind(subjects_vector,final_activityPeformed_vector,measurements_of_all_features_vector)
        ## result <- result[, !(names(result) %in% ("activity_id"))] ## omit activity_id in final result due to Step 5
        
        subjects_vector <- extract_subject_train()
        activityPeformed_vector <- extract_ytrain()
        subjects_to_activity_vector <- cbind(subjects_vector,activityPeformed_vector) 
        extract_activity_names_vector <- extract_activity_names()
        final_activityPeformed_vector <- merge(subjects_to_activity_vector, extract_activity_names_vector)  ## this is for Step 3 - tie an activity name to each subject's activity
        measurements_of_all_features_vector <- extract_xtrain()
        result <- cbind(final_activityPeformed_vector,measurements_of_all_features_vector)
        result <- result[, !(names(result) %in% ("activity_id"))] ## omit activity_id in final result due to Step 5
        setwd(currdir)
        result

}

## Read 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.  
## (i.e. the subjectID from 1-30) indicating the ID of the volunteer who performed the observation
## Result Column: subject_id
extract_subject_train <- function() {
        subject_train <- read.table("./subject_train.txt",sep = "", blank.lines.skip = TRUE,stringsAsFactors = FALSE)
        setnames(subject_train,c("subject_id"))
        subject_train
}

## Read 'train/X_train.txt': Training set of all observations that recorded all 561 types of features performed by 70% volunteers for training 
## Result Columns: all 561 features names 
extract_xtrain <- function() {
        x_train <- read.table("./X_train.txt",stringsAsFactors = FALSE)
        all_features_names <-extract_features_names()
        setnames(x_train,all_features_names)
        x_train
}


## Read 'train/y_train.txt': Training labels (i.e. the activity id labels indicating the type of activity performed e.g. WALKING ) for each observation
## Result Column: activityID 
extract_ytrain <- function() {
        y_train <- read.table("./y_train.txt",stringsAsFactors = FALSE)
        setnames(y_train,c("activity_id"))
        y_train
}



#######################################################################
## Purpose: R function that read and return raw Test Data Set
#######################################################################
extract_test_data_set <- function() {
        currdir <- getwd()
        setWorkingPath_TestData()
        
        subjects_vector <- extract_subject_test()
        activityPeformed_vector <- extract_ytest()
        subjects_to_activity_vector <- cbind(subjects_vector,activityPeformed_vector) ## new
        extract_activity_names_vector <- extract_activity_names()
        final_activityPeformed_vector <- merge(subjects_to_activity_vector, extract_activity_names_vector)  ## this is for Step 3 - tie an activity name to each subject's activity
        measurements_of_all_features_vector <- extract_xtest()
        result <- cbind(final_activityPeformed_vector,measurements_of_all_features_vector)
        result <- result[, !(names(result) %in% ("activity_id"))] ## omit activity_id in final result due to Step 5
        setwd(currdir)
        result
}

## Read all subjects of test data set
extract_subject_test <- function() {
        subject_test <- read.table("./subject_test.txt",sep = "", blank.lines.skip = TRUE,stringsAsFactors = FALSE)
        setnames(subject_test,c("subject_id"))
        subject_test
}

## Read 'test/X_test.txt': Test set of all observations that recorded all 561 types of features performed by 30% volunteers for training 
## Result Columns: all 561 features names 
extract_xtest <- function() {
        x_test <- read.table("./X_test.txt",stringsAsFactors = FALSE)
        all_features_names <-extract_features_names()
        setnames(x_test,all_features_names)
        x_test
}

## Read 'test/y_test.txt': Test labels (i.e. the activity id labels indicating the type of activity performed e.g. WALKING ) for each observation
## Result Column: activity_id 
extract_ytest <- function() {
        y_test <- read.table("./y_test.txt",stringsAsFactors = FALSE)
        setnames(y_test,c("activity_id"))
        y_test
}

######################################################################
clearMemory <- function() {
        rm(list = ls())
}