require(dplyr)
require(plyr)

createDataSet <- function(dir){
  dataSet <- cleanData(dir)
  write.table(dataSet, file = "sensor_avg.txt", sep = ";", row.names = FALSE)
}

get.data <- function(dir, name){

  # Directory and files
  dataDirectory <- file.path(dir,name)
  featuresPath <- file.path(dataDirectory, paste("X_", name, ".txt", sep=""))
  activitiesPath <- file.path(dataDirectory, paste("Y_", name, ".txt", sep=""))
  subjectsPath <- file.path(dataDirectory, paste("subject_", name, ".txt", sep=""))
  featuresLabelsPath <- file.path(dir, paste("features.txt", sep=""))
  activitiesLabelsPath <- file.path(dir, paste("activity_labels.txt", sep=""))
  
  # Load raw data
  featuresData <- read.table(featuresPath)
  featuresLabels <- read.table(featuresLabelsPath, col.names = c("FeatureID", "Feature"), colClasses = c("character"))    
  activitiesData <- read.table(activitiesPath, col.names = c("ActivityID"))
  activitiesLabels <- read.table(activitiesLabelsPath, col.names = c("ActivityID", "Activity"))  
  subjectsData <- read.table(subjectsPath, col.names = c("Subject"))
  
  # Bind data
  cleanData <- cbind(cbind(featuresData, subjectsData), activitiesData)
  cleanDataNames <- rbind(rbind(featuresLabels, c(562, names(subjectsData))), names(activitiesData))
  
  # Add labels
  valid_names <- make.names(names = cleanDataNames[,2], unique = TRUE, allow_ = TRUE)
  names(cleanData) <- valid_names
  
  # Add test type if test or train
  cleanData <- mutate(cleanData, testType = name)
  
  cleanData
}

cleanData <- function(dir){
  test <- get.data(dir, "test")
  train <- get.data(dir, "train")
  
  # Merges the training and the test sets into one data set.
  allData <- rbind(test, train)
  
  # Extracts only mean and std for each measurement.
  dataSet <- allData[, grep("mean|std|Subject|ActivityID", names(allData))]
  
  # Use descriptive activity names to name the activities in the data set.
  dataSet <- join(dataSet, activitiesLabels, by = "ActivityID", match = "first")
  dataSet <- dataSet [, -1]
  
  # Labels with descriptive names
  names(dataSet) <- gsub('Acc', "Acceleration", names(dataSet))
  names(dataSet) <- gsub('GyroJerk', "AngularAcceleration", names(dataSet))  
  names(dataSet) <- gsub('Gyro', "AngularSpeed", names(dataSet))
  names(dataSet) <- gsub('Mag', "Magnitude", names(dataSet))
  names(dataSet) <- gsub('Freq', "Frequency", names(dataSet))
  names(dataSet) <- gsub("..", "", names(dataSet), fixed = TRUE)
  names(dataSet) <- gsub("...", "", names(dataSet), fixed = TRUE)
  
  # Creates a data set with the average of each variable for each activity and subject.
  
  cleanData <- ddply(dataSet, c("Subject", "Activity"), numcolwise(mean))
  
  cleanData
}