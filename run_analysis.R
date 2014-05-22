##  You should create one R script called run_analysis.R that does the following. 
##  Merges the training and the test sets to create one data set.

##   Extracts only the measurements on the mean and standard deviation for each measurement
. 
##   Uses descriptive activity names to name the activities in the data set

##   Appropriately labels the data set with descriptive activity names. 

##   Creates a second, independent tidy data set with the average of each variable for each activity and each subject.



## read data from test files
X_test <- read.table("D:/UCI HAR Dataset/test/X_test.txt", quote="\"")
y_test <- read.table("D:/UCI HAR Dataset/test/Y_test.txt", quote="\"")
subject_test <- read.table("D:/UCI HAR Dataset/test/subject_test.txt", quote="\"")

## read data from train files
X_train <- read.table("D:/UCI HAR Dataset/train/X_train.txt", quote="\"")
y_train <- read.table("D:/UCI HAR Dataset/train/Y_train.txt", quote="\"")
subject_train <- read.table("D:/UCI HAR Dataset/train/subject_train.txt", quote="\"")

##read feature
activity_labels <- read.table("D:/UCI HAR Dataset/activity_labels.txt", quote="\"")
features <- read.table("D:/UCI HAR Dataset/features.txt", quote="\"")


## merge test and train data

xdata<-rbind(X_test,X_train)
ydata<-rbind(y_test,y_train)
subjectdata<-rbind(subject_test,subject_train)


## names variables
colnames(xdata)<-features[,2]
colnames(ydata)<-"labels"
colnames(subjectdata)<-"subject"


##grep data

meandata<-grep("mean()",colnames(xdata),fixed=TRUE)

sddata<-grep("std()",colnames(xdata),fixed=TRUE)

msdata<-xdata[,c(meandata,sddata)]



## names data set

activity_labels[,2]<-as.character(activity_labels[,2])

for(i in 1:length(ydata[,1]))
  {
  
  ydata[i,1]<-activity_labels[ydata[i,1],2]

}

## Appropriately labels the data set with descriptive activity names

newdata<-cbind(subjectdata,ydata,msdata)

##   Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


tidynewdata<-aggregate(newdata[,3]~subject+labels,data=newdata,FUN="mean")

n<-ncol(newdata)
for( i in 3:n){

tidynewdata[,i]<-aggregate(newdata[,i]~subject+labels,data=newdata,FUN="mean")

}
colnames(tidynewdata)[3:ncol(tidynewdata)] <- colnames(msdata)

write.table(tidynewdata, file = "TidyData.txt")

