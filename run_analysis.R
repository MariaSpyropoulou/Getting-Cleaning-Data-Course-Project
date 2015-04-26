
#This is data from a research from University of California, Irvine
#regarding Human Activity Recognition measurements
#The experiments have been carried out with a group of 30 volunteers (subjects) within an age 
#bracket of 19-48 years. Each person performed six activities 
#(WALKING, WALKING_UPSTAIRS,WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
#wearing a smartphone (Samsung Galaxy S II) on the waist. 
#Using its embedded accelerometer and gyroscope, they captured 3-axial 
#angular velocity and 3-axial linear accelerationat a constant rate of 50Hz
#70% of the volunteers was selected for generating the training data and 30% the test data

#Download and unzip data at the working directory
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file<-download.file(url,destfile="file.zip")
data_set <- unzip("file.zip")

#Read data
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels<-read.table("./UCI HAR Dataset/test/Y_test.txt")
test_subjects<-read.table("./UCI HAR Dataset/test/subject_test.txt")
train<-read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels<-read.table("./UCI HAR Dataset/train/Y_train.txt")
train_subjects<-read.table("./UCI HAR Dataset/train/subject_train.txt")
features<-read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Merge test set and training set to create one data set and name the variables
# according to features
merged<-rbind(train,test)
merged<-as.data.frame(merged)
colnames(merged)<-features[,2]

#Extract only the measurement on the mean and standard deviation for each measurement
#Essentially limit data frame to columns matching mean() or std()
matches<-grep("(mean|std)\\(\\)",colnames(merged))
meanstd<-merged[,matches]

#Use descriptive activity names to name the activities in the new data set
#Train and test labels give the acivities as numbers 1-6
#We create a data frame df out of them so we can merge the descriptive activityiD 
#from the activity_labels table with the numeric labels from df by the common numeric variable
labels<-rbind(train_labels,test_labels)[,1]
labels<-as.numeric(labels)
meanstd<-cbind(Activity= labels,meanstd)

#Appropriately label the data set with descriptive variable names
#Replace f with Frequency,t with Time, remove parentheses and dashes
names(meanstd)<-gsub("-mean\\(\\)","Mean",names(meanstd))
names(meanstd)<-gsub("-std\\(\\)","Std",names(meanstd))
names(meanstd)<-gsub("^f","Frequency",names(meanstd))
names(meanstd)<-gsub("^t","Time",names(meanstd))
names(meanstd)<-gsub("-","",names(meanstd))
names(meanstd)<-gsub("BodyBody","Body",names(meanstd))

#From this data set create a second independent tidy data set
#with the average for each variable for each activity and subject
subjects <- rbind(train_subjects, test_subjects)[, 1]
subjects<-as.numeric(subjects)
meanstd<-cbind(Subject=subjects,meanstd)

library(plyr)
library(Rcpp)
tidyColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidy <- ddply(meanstd, .(Subject, Activity),tidyColMeans)

names(activity_labels)[1]<-"Activity"
tidymeans<-join(tidy,activity_labels,by= "Activity",type= "left")
tidymeans[,2]<-tidymeans[,69]
tidymeans<-tidymeans[,1:68]

#Return data
write.table(tidymeans, "tidy.txt", row.names = FALSE)
read.table("tidy.txt", header = TRUE)