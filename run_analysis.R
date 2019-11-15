# libraries
library(dplyr)
library(data.table)

## Read in the test datasets
x.test <- read.csv("test/X_test.txt", sep="",
                   header=FALSE)

y.test <- read.csv("test/y_test.txt", sep="",
                   header=FALSE)

subject.test <- read.csv("test/subject_test.txt",
                         sep="", header=FALSE)

## Read in the training datasets
x.train <- read.csv("train/X_train.txt", sep="",
                    header=FALSE)

y.train <- read.csv("train/y_train.txt", sep="",
                    header=FALSE)

subject.train <- read.csv("train/subject_train.txt",
                          sep="", header=FALSE)

# Merge the test datasets into a single dataframe
test <- data.frame(subject.test, y.test, x.test)

# Merge test training datasets into a single dataframe
train <- data.frame(subject.train, y.train, x.train)

# Combine the training and test running datasets
run.data <- rbind(train, test)

### Now for labels

# Read in the measurement labels and apply to column names
features <- read.csv("features.txt", sep="", header=FALSE)

column.names <- as.vector(features[, 2])
column.names <- c("subject_id", "activity_labels", column.names)

#pick out the unique names, and excise the duplicates
uniqueColumns <- which(duplicated(column.names)==FALSE)
column.names <- column.names[uniqueColumns]

run.data <- run.data[,uniqueColumns]
colnames(run.data) <- column.names

# Select only the columns that contain mean or standard deviations, throw out rest

#why the hell do I have duplicate column names!?! Must fix that!

run.data <- select(run.data, contains("subject"), contains("label"),
                   contains("mean"), contains("std"), -contains("freq"),
                   -contains("angle"))


# Read in the activity labels dataset and label
activity.labels <- read.csv("activity_labels.txt", 
                            sep="", header=FALSE)


run.data$activity_labels <- as.character(activity.labels[
  match(run.data$activity_labels, activity.labels$V1), 'V2'])

# Clean up the column names. and what the hell with "body"
# word "Body".
setnames(run.data, colnames(run.data), gsub("\\(\\)", "", colnames(run.data)))
setnames(run.data, colnames(run.data), gsub("-", "_", colnames(run.data)))
setnames(run.data, colnames(run.data), gsub("BodyBody", "Body", colnames(run.data)))

# Group the running data by subject and activity, then
# calculate the mean of every measurement.
run.data.summary <- run.data %>%
  group_by(subject_id, activity_labels) %>%
  summarise_each(funs(mean))

# Write run.data to file
write.table(run.data.summary, file="run_data_summary.txt", row.name=FALSE)
