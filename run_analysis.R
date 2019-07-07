## fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## download.file(fileUrl, destfile <- "C:/Users/Janet/Desktop/JHUDS/code/GCD", method <- "auto")
## my laptop responded with "permission denied" when i attempted to download these files as per the course lessons
##    so i copied the files to a folder on my laptop and then unzipped (below).

unzip(zipfile <- "C:/Users/Janet/Desktop/JHUDS/3_Getting_and_Cleaning_Data/Week_4/Peer-graded Assignment/getdata_projectfiles_UCI HAR Dataset.zip", exdir <- "C:/Users/Janet/Desktop/JHUDS/3_Getting_and_Cleaning_Data/Week_4/Peer-graded Assignment/data")
dateDownloaded <- date()
dateDownloaded   ## the date that the data files were downloaded / unzipped

library(data.table)
library(dplyr)
library(stats)

## list files in zipfile
list.files("C:/Users/Janet/Desktop/JHUDS/3_Getting_and_Cleaning_Data/Week_4/Peer-graded Assignment/data/UCI HAR Dataset")

filepathdata <- file.path("C:/Users/Janet/Desktop/JHUDS/3_Getting_and_Cleaning_Data/Week_4/Peer-graded Assignment/data", "UCI HAR Dataset")
datafiles <- list.files(filepathdata, recursive=TRUE)
datafiles  ##list the unzipped data files

##  reading in testing files
test_features <- read.table(file.path(filepathdata, "test", "X_test.txt"), header=FALSE)
test_activity <- read.table(file.path(filepathdata, "test", "y_test.txt"), header=FALSE)
test_subject <- read.table(file.path(filepathdata, "test", "subject_test.txt"), header=FALSE)

##  reading in training files
train_features <- read.table(file.path(filepathdata, "train", "X_train.txt"), header=FALSE)
train_activity <- read.table(file.path(filepathdata, "train", "y_train.txt"), header=FALSE)
train_subject <- read.table(file.path(filepathdata, "train", "subject_train.txt"), header=FALSE)

##  reading in features file
name_features  <- read.table(file.path(filepathdata, "features.txt"), header=FALSE)

##  reading in activity labels file
activity_labels  <- read.table(file.path(filepathdata, "activity_labels.txt"), header=FALSE)

## 1.  begin merging data (item #1. "Merges the training and the test sets to create one data set.")
## combine training and test data
subject <- rbind(train_subject, test_subject)
activity <- rbind(train_activity, test_activity)
features <- rbind(train_features, test_features)

## naming columns
colnames(features) <- t(name_features [2])

##  merging data
colnames(activity) <- "ActivityName"
colnames(subject) <- "SubjectName"
merged_data <- cbind(features,activity,subject)

##  2.  "Extracts only the measurements on the mean and standard deviation for each measurement." 
mean_std_columns  <- grep(".*Mean.*|.*Std.*", names(merged_data), ignore.case=TRUE)
need_columns <- c(mean_std_columns , 562, 563)
dim(merged_data)

extract_data <- merged_data[,need_columns]
dim(extract_data)

##  3. "Use descriptive activity names to name the activities in the data set."
extract_data$ActivityName <- as.character(extract_data$ActivityName)
for (i in 1:6){
  extract_data$ActivityName[extract_data$ActivityName == i] <- as.character(activity_labels [i,2])
  }

extract_data$ActivityName <- as.factor(extract_data$ActivityName)

##  4.  "Appropriately labels the data set with descriptive variable names."
##  checKing what the current short names are
names(extract_data)

##changing short names to descriptive names
names(extract_data) <- gsub ("^t", "Time", names(extract_data))
names(extract_data) <- gsub ("Acc", "Accelerometer", names(extract_data))
names(extract_data) <- gsub ("-mean()", "Mean", names(extract_data), ignore.case = TRUE)
names(extract_data) <- gsub ("-std()", "STD", names(extract_data), ignore.case = TRUE)
names(extract_data) <- gsub ("Gyro", "Gyroscope", names(extract_data))
names(extract_data) <- gsub ("Mag", "Magnitude", names(extract_data))
names(extract_data) <- gsub ("BodyBody", "Body", names(extract_data))
names(extract_data) <- gsub ("^f", "Frequency", names(extract_data))
names(extract_data) <- gsub ("-freq()", "Frequency", names(extract_data), ignore.case = TRUE)

##  checKing that names are descriptive
names(extract_data)

##  5. "From the data set in step 4, creates a second, independent tidy data set with the 
##      average of each variable for each activity and each subject."
extract_data$SubjectName <- as.factor(extract_data$SubjectName)
extract_data <- data.table(extract_data)

tidy_data <- aggregate(. ~SubjectName + ActivityName, extract_data, mean)
tidy_data <- tidy_data[order(tidy_data$SubjectName,tidy_data$ActivityName),]
write.table(tidy_data, file = "Tidy_data_set.txt", row.names = FALSE)
