## The code in this file performs the following operations:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

## Execute this function to run the analysis
## The wd should be set to wherever the Samsung data (in its main folder, as unzipped) is
run_analysis <- function() {
      
      ## Load required libraries
      require(data.table)
      require(dplyr)

      ## 1. Merge the training and the tests set to create one dataset
      data <- mergesets()
      
      ## 2. Extract only the measurements on the mean and standard deviation for each measurement.
      ## Mean: column
      ## SD: column 
      data2 <- subset(data, select=(names(data)[grep("mean|std|subject|activity", names(data))]))
      
      ## 3. Use descriptive activity names to name the activities in the data set
      labels <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
      lindex <- labels$V2
      names(lindex) <- labels$V1
      data2$activity <- lindex[data2$activity]
      
      ## 4. Appropriately label the data set with descriptive variable names.
      ## Get rid of the parentheses
      names(data2) <- sub("(\\(\\))", "", names(data2))
      names(data2) <- gsub("\\-", "", names(data2))
      names(data2) <- sub("^t", "time", names(data2))
      names(data2) <- sub("^f", "freq", names(data2))
      
      ## 5. From the data set in step 4, create a second, independent tidy data set 
      ##    with the average of each variable for each activity and each subject.
      data3 <- data2 %>% group_by(subject, activity) %>% summarise_each(funs(mean))
      
      ## Write tidy data file as output
      write.table(data3, file="tidydata.txt", row.names=FALSE)
      
      data3
}

## This function merges the datasets
## The Samsung data should be in the working directory
mergesets <- function(){
      basefolder <- "./UCI HAR Dataset/"
      
      ## subject_test
      subject_test_test <- read.table(paste(basefolder, "test/subject_test.txt", sep=""))
      subject_test_train <- read.table(paste(basefolder, "train/subject_train.txt", sep=""))
      ## Join data
      subjecttest <- bind_rows(subject_test_test, subject_test_train)
      ## X_test
      x_test_test <- read.table(paste(basefolder, "test/X_test.txt", sep=""))
      x_test_train <- read.table(paste(basefolder, "train/X_train.txt", sep=""))
      ## Join data
      xtest <- bind_rows(x_test_test, x_test_train)
      ## Col names
      features <- read.table(paste(basefolder, "features.txt", sep=""), stringsAsFactors = FALSE)
      colnames(xtest) <- features$V2
      ## Y_test
      y_test_test <- read.table(paste(basefolder, "test/Y_test.txt", sep=""))
      y_test_train <- read.table(paste(basefolder, "train/Y_train.txt", sep=""))
      ## Join data
      ytest <- bind_rows(y_test_test, y_test_train)
      
      ## Join all 3 parts
      colnames(ytest) <- "activity"
      colnames(subjecttest) <- "subject"
      xtest$activity <- ytest$activity
      xtest$subject <- subjecttest$subject
      ## Reorder columns
      xtest <- xtest[, c(562, 563, 1:561)]

      xtest
}