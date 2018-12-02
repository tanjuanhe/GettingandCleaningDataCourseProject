## Run this script with the working directory set to be the folder containing
## the UCI HAR dataset's "README", "activity_labels", "features", and
## "features_info" files, as well as the "test" and "train" folders i.e. the
## "UCI HAR Dataset" folder.

## This script uses the "data.table" package.
library(data.table)

## This script does the following:
## 1. Creates one data frame each for the training and test sets, by matching
## subject, activity, and measurement data. Measurement data is cleaned into
## the 561 features as documented in the UCI HAR dataset's README.
## 2. Merges the training and test set data frames into one data frame.
## 3. Extracts only those variables that relate to the mean or standard
## deviation of measurements. Does NOT include the mean frequency variables.
## 4. Labels the data with descriptive names.
## 5. Creates a second, independent tidy data set that reflects the average for
## each variable for each pair of (subject, activity).


## Step 1: We match the subject, activity and measurement data for the test and
## training sets.

## First, we extract and clean the measurement data for the test set.
test_data <- readLines("test/X_test.txt") ## Extracts raw data
test_data <- strsplit(test_data, " ") ## Splits data using space as separator
test_data <- lapply(test_data, function(x) x[!x==""]) ## Removes empty entries
test_data <- as.numeric(unlist(test_data)) ## Converts into numeric vector
test_data <- matrix(test_data, ncol=561, byrow=TRUE) ## Converts into matrix
test_data <- as.data.frame(test_data) ## Converts into data frame
features <- readLines("features.txt") ## Extracts names of features
colnames(test_data) <- features ## Labels measurements with names of features

## Next, we match measurement data with subject and activity for the test set.
test_subject <- readLines("test/subject_test.txt") ## Extracts subject
test_activity <- readLines("test/y_test.txt") ## Extracts activity
test_dataset <- cbind("subject" = test_subject, "activity" = test_activity, 
                      test_data)
## Matches subject, activity, and measurement data into one data frame

## Finally, we repeat this for the training set.
train_data <- readLines("train/X_train.txt") ## Extracts raw data
train_data <- strsplit(train_data, " ") ## Splits data using space as separator
train_data <- lapply(train_data, function(x) x[!x==""]) ## Removes empty entries
train_data <- as.numeric(unlist(train_data)) ## Converts into numeric vector
train_data <- matrix(train_data, ncol=561, byrow=TRUE) ## Converts into matrix
train_data <- as.data.frame(train_data) ## Converts into data frame
colnames(train_data) <- features ## Labels measurements with names of features

train_subject <- readLines("train/subject_train.txt") ## Extracts subject
train_activity <- readLines("train/y_train.txt") ## Extracts activity
train_dataset <- cbind("subject" = train_subject, "activity" = train_activity, 
                      train_data)
## Matches subject, activity, and measurement data into one data frame


## Step 2: We merge the test and train sets into one data frame.
dataset <- rbind(test_dataset, train_dataset) ## Merges test and train set
rm(test_data, test_dataset, train_data, train_dataset,
   test_subject, test_activity, train_subject, train_activity)
## Clears other sets to save space


## Step 3: We extract only those variables relating to mean ("mean()") and
## standard deviation ("std()") of measurements. We exclude mean frequency
## variables ("meanFreq()").
wanted_list <- 2 + grep("mean\\(\\)|std\\(\\)", features) ## Finds the index of 
## mean and std variables. Adds 2 to skip over subject and activity columns.
dataset <- dataset[, c(1, 2, wanted_list)] ## Extracts mean and std variables


## Step 4: We label the data with descriptive names.

## First we label the activities with descriptive names.
activities <- readLines("activity_labels.txt") ## Extracts activity labels
activities <- gsub("_", "", tolower(substring(activities, 3))) ## Changes labels
## to lowercase and removes digits and underscores
dataset$activity <- sapply(dataset$activity,
                           function(x) gsub(x, activities[[x]], x))
## Replaces number code for activities with number code and description
dataset$activity <- as.factor(dataset$activity) ## Converts into factor

## Next we label the variable names descriptively, using only lowercase letters, 
## spelling out abbreviations, and removing spaces/hyphens/brackets/digits,
names <- names(dataset) ## Extracts column names
names <- tolower(names) ## Converts all to lowercase letters
## The below steps spell out abbreviations
names <- sub("\\<t", "timesignal", names)
names <- sub("\\<f", "frequencysignal", names)
names <- sub("acc", "accelerometer", names)
names <- sub("gyro", "gyrometer", names)
names <- sub("mag", "magnitude", names)
names <- sub("x$", "xdimension", names)
names <- sub("y$", "ydimension", names)
names <- sub("z$", "zdimension", names)
names <- sub("std", "standarddeviation", names)
names <- gsub("| |\\-|\\(|\\)|[0-9]", "", names) ## Removes unwanted characters
names[1:2] <- c("subject", "activity") ## Restores names of first two columns
names(dataset) <- names

## Step 5: We create a second, independent tidy data set that reflects the
## average for each variable for each pair of (subject, activity).
dataset<-as.data.table(dataset) ## Converts into data table
result <- dataset[, lapply(.SD, mean), by = .(subject, activity)] ## Calculates
## mean of each variable, for each pair of (subject, activity)
result$subject <- as.integer(as.character(result$subject))
## Converts subject to integer for ordering
result <- result[order(subject, activity)] ## Orders data by subject, then
## activity
result$subject <- as.factor(result$subject) ## Converts subject back to factor
names[3:length(names)]<- paste0("average", names[3:length(names)]) ## Adds the
## term "average" to names of variables to reflect that averages were taken
names(result) <- names ## Changes the headings of result data set

## Write the result into .txt format:
write.table(result, file = "result.txt", row.names = FALSE)

## Clear away all stored data
rm(dataset, result, activities, features, names, wanted_list)