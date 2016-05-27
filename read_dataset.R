read_dataset <- function(rootDir, setName, featureNames, keepFeature) {
    
    # Read the data set, and keep only the specified columns
    dataset <- read.delim(file.path(rootDir, setName, paste0("X_",setName,".txt")), FALSE, "", quote = "", col.names = featureNames)
    dataset <- dataset[,keepFeature]
    
    # Read the activity
    # (this is a dataset intended for machine learning, where an algorithm is 
    # being trained to detect an activity. The y Value is what the ML algorithm 
    # is trying to detect, the X vector, aka Feature vector, is the data that 
    # the machine learning algorithm is provided)
    activity <- readLines(file.path(rootDir, setName, paste0("y_",setName,".txt")))

    # Read the identifier of the subject (the person being measured)
    # Not necessarily useful in this example, but let's read it anyway.
    subject <- readLines(file.path(rootDir, setName, paste0("subject_",setName,".txt")))
    
    # Returns all the columns merged: daset, then activity, then subject.
    # Note that the activity is still numeric at this point.
    cbind(dataset,activity,subject) 
}