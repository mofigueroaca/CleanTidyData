# # Load dplyr package to subset and summarize data
library(dplyr)
library(reshape)
library(downloader)
# Set working directory
setwd("C:/Users/Administrador/Desktop")

# Download and unzip
if(!file.exists("data_project")){
	dir.create("data_project")
}
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
PATH <- "./data_project/"
FILE <- "getdata_projectfiles_UCI HAR Dataset.zip"
download(URL, paste(PATH,FILE,sep=""))
unzip(paste(PATH,FILE,sep=""))

# Open test_X data set
testX<-read.table("./UCI HAR Dataset/test/X_test.txt")
# Subset using only the mean and standard deviation variables
testX2<-select(testX,V1:V6, V41:V46, V81:V86, V121:V126, V161:V166, V201:V202, V214:V215, V227:V228, V240:V241, V253:V254, 
V266:V267, V268:V271, V345:V350,V424:V429, V503:V504, V516:V517,V529:V530,V555:V561)
# Open subject_test.txt file
SubjectTest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
# Merge id to data set
Subject<-SubjectTest
testX3<-cbind(Subject$V1, testX2)
# Add a character variable to especify the type of data (test or train)
testX4<-mutate(testX3, type = rep("test", length(testX3$V1)))
# Open code for activities
YTest<-read.table("./UCI HAR Dataset/test/y_test.txt")
# Make factor
Activities<-factor(YTest$V1)
# Merge new activities variable to the data set
testX5<-cbind(Activities, testX4)
# Rename the activities
levels(testX5$Activities) <- list("Walk" = "1", "WalkUp" = "2", "WalkDown" = "3", "Sit" = "4","Stand" = "5", "Lay" = "6")

# Open train_X data set
trainX<-read.table("./UCI HAR Dataset/train/X_train.txt")
# Subset using only the mean and standard deviation variables
trainX2<-select(trainX,V1:V6, V41:V46, V81:V86, V121:V126, V161:V166, V201:V202, V214:V215, V227:V228, V240:V241, V253:V254, 
V266:V267, V268:V271, V345:V350,V424:V429, V503:V504, V516:V517,V529:V530,V555:V561)
# Open subject_test.txt file
SubjectTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
Subject<-SubjectTrain
# Merge id to data set
trainX3<-cbind(Subject$V1, trainX2)
# Add a character variable to especify the type of data (test or train)
trainX4<-mutate(trainX3, type = rep("train", length(trainX3$V1)))
# Open code for activities
YTrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
# Make factor
Activities<-factor(YTrain$V1)
# Merge new activities variable to the data set
trainX5<-cbind(Activities, trainX4)
# Rename the activities
levels(trainX5$Activities) <- list("Walk" = "1", "WalkUp" = "2", "WalkDown" = "3", "Sit" = "4","Stand" = "5", "Lay" = "6")

# Merge both train and test data sets
TestTrain<-rbind(testX5, trainX5)

# str(TestTrain) # to check that that the datasets were correctly merged

# Change names of variables
noms<-c("Activities",
"Subject",
"MeanTimeBodyAcceleration_X", 
"MeanTimeBodyAcceleration_Y", 
"MeanTimeBodyAcceleration_Z", 
"StdTimeBodyAcceleration_X",
"StdTimeBodyAcceleration_Y",
"StdTimeBodyAcceleration_Z",
"MeanTimeGravityAcceleration_X",
"MeanTimeGravityAcceleration_Y",
"MeanTimeGravityAcceleration_Z",
"StdTimeGravityAcceleration_X",
"StdTimeGravityAcceleration_Y",
"StdTimeGravityAcceleration_Z",
"MeanTimeBodyJerk_X",
"MeanTimeBodyJerk_Y",
"MeanTimeBodyJerk_Z",
"StdTimeBodyJerk_X",
"StdTimeBodyJerk_Y",
"StdTimeBodyJerk_Z",
"MeanTimeBodyGyro_X",
"MeanTimeBodyGyro_Y",
"MeanTimeBodyGyro_Z",
"StdTimeBodyGyro_X",
"StdTimeBodyGyro_Y",
"StdTimeBodyGyro_Z",
"MeanTimeBodyGyrojerk_X",
"MeanTimeBodyGyrojerk_Y",
"MeanTimeBodyGyrojerk_Z",
"StdTimeBodyGyrojerk_X",
"StdTimeBodyGyrojerk_Y",
"StdTimeBodyGyrojerk_Z",
"MeanTimeBodyAccelerationMagnitude",
"StdTimeBodyAccelerationMagnitude",
"MeanTimeGravityAccelerationMagnitude",
"StdTimeGravityAccelerationMagnitude",
"MeanTimeBodyAccelerationJerkMagnitude",
"StdTimeBodyAccelerationJerkMagnitude",
"MeanTimeBodyGyroMagnitude",
"StdTimeBodyGyroMagnitude",
"MeanTimeBodyGyroJerkMagnitude",
"StdTimeBodyGyroJerkMagnitude",
"MeanFrequencyBodyAcceleration_X",
"MeanFrequencyBodyAcceleration_Y",
"MeanFrequencyBodyAcceleration_Z",
"StdFrequencyBodyAcceleration_X",
"StdFrequencyBodyAcceleration_Y",
"StdFrequencyBodyAcceleration_Z",
"MeanFrequencyBodyAccelerationJerk_X",
"MeanFrequencyBodyAccelerationJerk_Y",
"MeanFrequencyBodyAccelerationJerk_Z",
"StdFrequencyBodyAccelerationJerk_X",
"StdFrequencyBodyAccelerationJerk_Y",
"StdFrequencyBodyAccelerationJerk_Z",
"MeanFrequencyBodyGyro_X",
"MeanFrequencyBodyGyro_Y",
"MeanFrequencyBodyGyro_Z",
"StdFrequencyBodyGyro_X",
"StdFrequencyBodyGyro_Y",
"StdFrequencyBodyGyro_Z",
"MeanFrequencyBodyAccelerationMagnitude",
"StdFrequencyBodyAccelerationMagnitude",
"MeanFrequencyBodyAccelerationJerkMagnitude",
"StdFrequencyBodyAccelerationJerkMagnitude",
"MeanFrequencyBodyGyroMagnitude",
"StdFrequencyBodyGyroMagnitude",
"MeanTimeBodyAcceleration_Gravity_Angle",
"MeanTimeBodyAcceleration_Jerk_MeanGravity_Angle",
"MeanTimeBodyGyro_MeanGravity_Angle",
"MeanTimeBodyGyro_Jerk_MeanGravity_Angle",
"MeanGravity_X_Angle",
"MeanGravity_Y_Angle",
"MeanGravity_Z_Angle",
"Type")

colnames(TestTrain)<-noms

# Creation of new tidy data set for submission
TDS<- select(TestTrain, -Type)
TDS2<-as.tbl(TDS)
TDS3<-group_by(TDS2, Activities, Subject)
TDS4<-summarise_each(TDS3, funs(mean))
write.table(TDS4, "./UCI HAR Dataset/TidyDataSet.txt", sep = ",", row.name=FALSE) 




