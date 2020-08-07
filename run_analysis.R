## This script carries out the course assignment tasks. See the enclosed Readme  
# document for more detail.
library(tidyverse)
library(utils)
library(dataMaid)

# Download zipped HAR data file

filename <- "gadcproj.zip" 
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, filename, method="curl")

remove(fileurl, filename)

# Pre-modifications stage; Read and extract text format information for
# purposes of labeling measurements (features) and subjects' activities.

features <- read.table(unz("gadcproj.zip",
            "UCI HAR Dataset/features.txt"), header=F)
features  <- as.character(features[,2])

activitylabels <- read.table(unz("gadcproj.zip",
                  "UCI HAR Dataset/activity_labels.txt"), header=F)
activitylabels <- as.character(activitylabels[,2])

#First stage, step one
# Read and extract *training* data files, assigning adequate intermediate
# column names for measurement variables, and attach a key ID variable for 
# merging purposes.  

trainsubject <- read.table(unz("gadcproj.zip", 
                "UCI HAR Dataset/train/subject_train.txt"), header=F, 
                col.names = "subject")
trainsubject <- tibble::rowid_to_column(trainsubject, "ID")

trainx <- read.table(unz("gadcproj.zip",
          "UCI HAR Dataset/train/X_train.txt"), header=F, 
          col.names = features)
trainx <- tibble::rowid_to_column(trainx, "ID")

trainy <- read.table(unz("gadcproj.zip",
                     "UCI HAR Dataset/train/y_train.txt"), header=F, 
                     col.names = "activity")
trainy <- tibble::rowid_to_column(trainy, "ID")
                                    
# Read and extract *test* data files by a procedure corresponding to the truining
# data (immediately above)

testsubject <- read.table(unz("gadcproj.zip",
               "UCI HAR Dataset/test/subject_test.txt"), header=F, 
                col.names = "subject")
testsubject <- tibble::rowid_to_column(testsubject, "ID")

testx <- read.table(unz("gadcproj.zip",
          "UCI HAR Dataset/test/X_test.txt"), header=F,
          col.names = features)
testx <- tibble::rowid_to_column(testx, "ID")

testy <- read.table(unz("gadcproj.zip",
           "UCI HAR Dataset/test/y_test.txt"), header=F,
           , col.names = "activity")
testy <- tibble::rowid_to_column(testy, "ID")

# Merge all *training* data files into one training data frame 

trainadf <- bind_cols(trainsubject, trainy, id = "ID")
trainadf <- select(trainadf, -c(3,5))
traindf <- bind_cols(trainadf, trainx, id = "ID")
traindf <- select(traindf, -c(1, 4,566))

# Merge all test data files into one *test* data frame

testadf <- bind_cols(testsubject, testy, id = "ID")
testadf <- select(testadf, -c(3,5))
testdf <- bind_cols(testadf, testx, id = "ID")
testdf <- select(testdf, -c(1, 4,566))

# First stage, step two; Join training and test data into a common data frame 

alldf <- bind_rows(traindf, testdf)

remove (trainadf, testadf, traindf, testdf)

# Second stage; Extract mean and standard deviation measurement variables
# with subject and activity identifiers. Attach human readable labels and
# names that describe activities and measures. 

selectdf <- alldf %>% select(subject, activity, contains("mean"),
                    contains("std"))  
selectdf$activity <- activitylabels[selectdf$activity]

names(selectdf) <- gsub("Acc", "Accelerometer", names(selectdf))
names(selectdf) <- gsub("Gyro", "Gyroscope", names(selectdf))
names(selectdf) <- gsub("BodyBody", "Body", names(selectdf))
names(selectdf) <- gsub("Mag", "Magnitude", names(selectdf))
names(selectdf) <- gsub("^t", "Time", names(selectdf))
names(selectdf) <- gsub("^f", "Frequency", names(selectdf))
names(selectdf) <- gsub("tBody", "TimeBody", names(selectdf))
names(selectdf) <- gsub("-mean()", "Mean", names(selectdf), ignore.case = TRUE)
names(selectdf) <- gsub("-std()", "STD", names(selectdf), ignore.case = TRUE)
names(selectdf) <- gsub("-freq()", "Frequency", names(selectdf),
                   ignore.case = TRUE)
names(selectdf) <- gsub("angle", "Angle", names(selectdf))
names(selectdf) <- gsub("gravity", "Gravity", names(selectdf))

remove(features, activitylabels, alldf)


# Third stage; Compute average values for all selected measurements for
# each subject in each activity and create independent tidy data set

assignmentdata <- selectdf %>% group_by(subject, activity) %>%
                  summarise_all(mean)

remove(selectdf)

# Create data set 
write.table(assignmentdata, "GACD Course Project/assignmentdata.txt",
            row.name = FALSE)

# Create codebook for the tidy, independent data set
makeCodebook(assignmentdata, reportTitle = "Neat Codebook", 
             file = "Codebook")
