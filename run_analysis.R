run_analysis <- function(downloadDataset=FALSE) {
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
    
    # featureNames = gsub("\\s|\\d|\\(|\\)","", featureNames)
    
    #
    # Generate a list of features to keep, i.e. the means and standard deviations
    # (To do that, we check whether the field name contains "mean()" or "std()")
    #
    keepFeature = grepl("mean\\(\\)|std\\(\\)", featureNames)
    
    #
    #
    #
    featureNames = rename_features(featureNames)
    
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
    
    dataset
    
}