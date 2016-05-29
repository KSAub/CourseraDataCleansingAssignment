##
## Getting and Cleaning Data Course Project: Reads the datasets specficied in 
## the assignment, downloading it if it doesn't exist. Merges the datasets and 
## filters the columns as specifies, calculates the averages grouped by Subject 
## and Activity, and writes the result to text files.
##
run_analysis <- function(downloadDataset=FALSE) {
    library(dplyr)
    
    # Path to the directory under which the dataset is located
    archiveRoot <- "UCI HAR Dataset"
    
    #
    # Download and extract the dataset files if they don't exist, or if the downloadDataset flag is set.
    #
    if (!file.exists(archiveRoot) | downloadDataset) {
        archiveUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        archiveFileName = "uci-har-tmp.zip"
        
        download.file(archiveUrl, archiveFileName)
        unzip(archiveFileName)
        file.remove(archiveFileName)
    }
    
    #
    # Read feature names and activity labels.
    #
    featureNames = readLines(file.path(archiveRoot, "features.txt"))
    activityLabels = read.delim(file.path(archiveRoot, "activity_labels.txt"), FALSE, "")
    
    #
    # Generate a list of features to keep, i.e. "the measurements on the mean
    # and standard deviation for each measurement" as defined in the assignment.
    #
    # We interpret this as:
    # - field name beginning with t (because the others are not measurement, but
    #   rather are derived from the measurements mathematically)
    # - fields not containing "Mag" or "Jerk" (since the Magnitudes and Jerk 
    #   signals are also derived from the measurements as per features_info.txt)
    # - field name containing "mean()" or "std()" (so only the means and 
    #   standard deviations)
    #
    keepFeature = grepl("^\\d+\\s+t(\\w+?)[^(Mag|Jerk)]-(mean\\(\\)|std\\(\\))", featureNames)
    
    #
    # Read train and test set and merge them (see read_dataset.R for details on
    # hot the files are read and the columns filtered and merged)
    #
    trainSet <- read_dataset(archiveRoot, "train", featureNames, keepFeature)
    testSet <- read_dataset(archiveRoot, "test", featureNames, keepFeature)
    dataset <- rbind(trainSet,testSet)
    
    #
    # Apply the activity labels to the activity column
    #
    dataset$activity <- factor(dataset$activity, activityLabels[,1], activityLabels[,2])
    
    #
    # Tidy up the label names
    #
    colnames(dataset) <- rename_features(colnames(dataset))
    
    #
    # Average each variable for each activity and each subject using a dplyr pipeline
    #
    averages <- dataset %>% group_by(Activity, Subject) %>% summarise_each(funs(mean(., na.rm=TRUE))) %>% as.data.frame()
    
    #
    # Save the data into a set of CSV files
    #
    write.table(dataset, "dataset.csv", row.name=FALSE)
    write.table(averages, "averages.csv", row.name=FALSE)
    
    averages
}