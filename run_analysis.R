
library(dplyr) #importing libraries

#Importing features and activities
features <- read.table("features.txt", header = FALSE)
activities <- read.table("activity_labels.txt", header = FALSE)

#Importing training data
X_train <- read.table("train/X_train.txt", header = FALSE)
Y_train <- read.table("train/Y_train.txt", header = FALSE, col.names = "Y")
subject_train <- read.table("train/subject_train.txt", header = FALSE, col.names = "subject")

#Importing test data
X_test <- read.table("test/X_test.txt", header = FALSE)
Y_test <- read.table("test/y_test.txt", header = FALSE, col.names = "Y")
subject_test <- read.table("test/subject_test.txt", header = FALSE, col.names = "subject")

#Merging test and training data
X_data <- rbind(X_train, X_test)
Y_data<- rbind(Y_train, Y_test)
subject_data <- rbind(subject_train, subject_test)
colnames(X_data) <- t(features[2]) #Giving label for "X_data"
Merged_Data <- cbind(X_data, Y_data, subject_data) #Merging all data


columns_MaV <- grep(".Mean.|.Std.", names(X_data), ignore.case=TRUE) #Getting column names which contain "Mean" and "Std"
columns_MaV <- c(columns_MaV, 562, 563) #Listing column name of interest and adding "Y" and "subject" columns 
MeanandVariance <- Merged_Data[, columns_MaV] #Getting data of interest
colnames(MeanandVariance)[87] <- "activity" #Giving "activity" label to column "Y"

#replacing the activity to the corresponding number under the correspondence given in activities$v2
MeanandVariance$activity <- as.numeric(MeanandVariance$activity)
for (i in 1:nrow(activities)) {MeanandVariance$activity[MeanandVariance$activity == i] <- activities$V2[i]}  

#Replacing abbreviated words with corresponding ones
colnames(MeanandVariance) <- gsub("Acc", "Accelerometer", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("Gyro", "Gyroscope", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("BodyBody", "Body", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("Mag", "Magnitude", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("^t", "Time", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("^f", "Frequency", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("tBody", "TimeBody", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("-mean()", "Mean", names(MeanandVariance), ignore.case = TRUE)
colnames(MeanandVariance) <- gsub("-std()", "STD", colnames(MeanandVariance), ignore.case = TRUE)
colnames(MeanandVariance) <- gsub("-freq()", "Frequency", colnames(MeanandVariance), ignore.case = TRUE)
colnames(MeanandVariance) <- gsub("angle", "Angle", colnames(MeanandVariance))
colnames(MeanandVariance) <- gsub("gravity", "Gravity", colnames(MeanandVariance))

#Getting tidy data from previous data by averaging every variable for each subject and activity
tidy_data <- aggregate(. ~subject + activity, MeanandVariance, mean)
tidy_data <- tidy_data[order(tidy_data$subject,tidy_data$activity),]
write.table(tidy_data, "tidy_data.txt", row.names = FALSE) #Exporting tidy data to txt file
