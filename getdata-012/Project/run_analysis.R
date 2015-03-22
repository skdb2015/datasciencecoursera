## GetData course project
library(reshape2)

run_analysis <- function()
{
    ##
    ## 0. Read in the data into data frames that reflect the way 
    ##    the raw data is organized
    ##
    
    ## read the activity labels and feature strings
    activity_labels = read.table("data/UCI HAR Dataset/activity_labels.txt")
    features = read.table("data/UCI HAR Dataset/features.txt")
    
    ## read the raw train data
    subject_train = read.table("data/UCI HAR Dataset/train/subject_train.txt")
    X_train = read.table("data/UCI HAR Dataset/train/X_train.txt")
    y_train = read.table("data/UCI HAR Dataset/train/y_train.txt")
    
    ## read the raw test data
    subject_test = read.table("data/UCI HAR Dataset/test/subject_test.txt")
    X_test = read.table("data/UCI HAR Dataset/test/X_test.txt")
    y_test = read.table("data/UCI HAR Dataset/test/y_test.txt")
    
    ##
    ##  1. Merge the training and the test sets to create one data set
    ##
    
    ## merge the test data into a single data frame.
    ## the first column is the id of the subject, the second, the activity.
    ## the next 561 columns indicate the feature measurements
    testData = cbind(subject_test, y_test, X_test)
    
    ## merge the training data into a single data frame. 
    ## Same arrangement as that used for the test data
    trainData = cbind(subject_train, y_train, X_train)
    
    ## now, combine the test and training data into a single data frame
    rawData = rbind(testData,trainData)
    
    ##
    ##  label the dataset with descriptive variable names
    ##
    colnames(rawData) = c("subject ID", "activity", as.character(features[,2]))
    
    ##
    ##  2. Extract only the measurements that reflect mean and standard 
    ##     deviation numbers
    ##
    
    ##  First, get a vector that indicates which elements of the 561 feature
    ##  measurements are mean and standard deviation measurements
    isMeanOrSD = isFeatureMeanOrSD(features[,2])
    
    ##  Now, extract the relevant readings from the rawData dataframe
    ##  The first two columns will always need to be included as they
    ##  contain the ID of the subject and the activity
    extractedData = rawData[,c(TRUE,TRUE,isMeanOrSD)]
    
    ##
    ##  3. Use the descriptive activity names to name the activities in the 
    ##     data set
    ##
    extractedData[,2] = sapply(extractedData[,2], getActivityString, 
                               activity_labels)
    
    ##
    ##  4. Appropriately label the dataset with descriptive variable names
    ##
    
    ##  get the descriptive variable names for the features
    descriptiveFeatureNames = getLabels(features[isMeanOrSD,2])
    
    ##  set the column names with the descriptive variable names
    colnames(extractedData) = c("subject_ID", "activity", 
                                descriptiveFeatureNames)
    
    ##
    ##  5. From the data set in step 4, create a second, independent tidy data 
    ##     set with the average of each variable for each activity and each 
    ##     subject
    ##
    
    ##
    ##  Reshape the data so that it's ID is based on the subject's ID and the 
    ##  activity.
    ##
    fitMelt = melt(extractedData, id=c("subject_ID","activity"), 
                   measure.vars=descriptiveFeatureNames)
    
    ##
    ## cast the data frame on the basis of the subject's ID and the activity
    ## while averaging the feature measurements. So, for each activity and
    ## subject combination, the average for each variable is in that row.
    ##
    fitMelt_bySubjectAndActivity = dcast(fitMelt,
                                         subject_ID + activity ~ variable, 
                                         mean)
    
    ##
    ##  Appropriately label the columns to indicate that they are averages
    ##
    colnames(fitMelt_bySubjectAndActivity) = c("subject_ID", 
                                               "activity", 
                                               paste("Average_of",
                                                     descriptiveFeatureNames,
                                                     sep="_"))
    ##
    ##  Final step: Write the data out to a file
    ##
    write.table(fitMelt_bySubjectAndActivity, 
                "analysis_result.txt", row.names = FALSE)
}

##
##  This function takes the vector of features as listed in features.txt and
##  returns a vector of booleans indicating whether the indexed feature is a 
##  mean or a standard deviation
##
##  Approach: this function essentially assesses the text of the feature. If it
##  contains the substring "mean" or "std" it means it is a valid string
##
isFeatureMeanOrSD <- function(featureVector)
{
    ## the substrings to look for
    meanStr = "-mean"
    stdStr = "-std"
    
    ##  result vector
    resultVector = vector()
    idx = 1
    
    ## loop through the feature names
    for(feature in featureVector)
    {
        ##  Search for the substring '-mean' and then '-std' in the feature 
        ##  name. If neither are found, then the feature is not of interest
        if( length(grep(meanStr, feature, ignore.case = TRUE)) > 0)
        {
            resultVector[idx] = TRUE
        }
        else if (length(grep(stdStr,feature, ignore.case = TRUE)) > 0)
        {
            resultVector[idx] = TRUE
        }
        else
        {
            resultVector[idx] = FALSE
        }
        idx = idx + 1
    }
    
    resultVector
}

##
##  This function retuns strings that can be used as label names for the 
##  features in the features.txt file. 
##
##  Approach: replace all the special characters with underscore '_'
## 
getLabels <-function(featureVector)
{
    gsub("[^[:alnum:]]","_",featureVector)
}

##
##  This function returns the activity string corresponding to the activity 
##  index (activityIndex) that is passed to it as an argument
##
##  activityDataFrame is the data frame that extracted from the
##  activity_labels.txt file
##
getActivityString <- function(activityIndex, activityDataFrame)
{
    ## read the appropriate entry based on the index
    activityDataFrame[activityIndex,2]
}