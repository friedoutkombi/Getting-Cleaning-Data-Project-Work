##Install dependent libaries, note install prior.
library(data.table)
library(dplyr)

##Download the file & Unzip
FileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File<-"./data/Data.zip"
Loc<-"./data"
if(!file.exists(Loc)){dir.create(Loc)}
download.file(FileURL,destfile=File)

unzip(zipfile=File,exdir=Loc)
##zippled file can now be found in your working directy in a folder named "data".

##check working directory with getwd() and change to the new folder location.
setwd('C:/Users/Nicholas/Documents/data/UCI HAR Dataset')
##check what's in this directory
list.files(getwd())

##Need to load in our datasets, from eyeballing the data we will not need to include all files.
##Since we set the working directory to the UCI Har Dataset folder this should be real easy.
##Note using header=FALSE as the first line of the file does not dictate our column headers!

#Reading training tables
subject_train<-read.table('train/subject_train.txt', header =FALSE)
X_train<-read.table('train/X_train.txt', header =FALSE)
y_train<-read.table('train/y_train.txt', header =FALSE)
#Read testing tables
subject_test<-read.table('test/subject_test.txt', header =FALSE)
X_test<-read.table('test/X_test.txt', header =FALSE)
y_test<-read.table('test/y_test.txt', header =FALSE)
#Read features
features <- read.table('features.txt')
#Read activity labels
activity_labels<-read.table('activity_labels.txt', header = FALSE)


##Now let's complete task "1.Merges the training and the test sets to create one data set."
##Will be first creating column names and then looking at cbind/rbind to merge.
##We have two paths 1) First merge all test sets, merge all training sets and then merge both OR
##2) Marge subject-subject,x-x, y-y and then combine the 3 sets.
##I have used method 1

###For guidance think of us trying to merge left to right the test data column by column, then repeating the same for the training data. Finally then putting the training data on top of the test data.
#As such we will want the colnames we apply to test to be the same for train.


###Test Data - Assign colNames and then merge
colnames(X_test) <- features[,2] ###taking the names from the features file
colnames(y_test) <- "Activity"
colnames(subject_test) <- "Subject"

colnames(activity_labels) <- c('Activity','Subject')
#run dim(*test variable*) to see we will be combining by column (cbind)
#then merge
mergetest<-cbind(y_test, subject_test, X_test)

####DO the same for the Train data
colnames(X_train) <- features[,2] ###taking the names from the features file
colnames(y_train) <- "Activity"
colnames(subject_train) <- "Subject"

#run dim(*train variable*) to see we will be combining by column (cbind)
#then merge
mergetrain<-cbind(y_train, subject_train, X_train)

##check dimensions of both mergetest & mergetrain... both have the same number of columns so we will bind by row.
Merged<-rbind(mergetrain,mergetest)


#### Our next tasks is "2. Extracts only the measurements on the mean and standard deviation for each measurement".
##Eyeballing the colnames we can see there is multiple variables with min,max,kurtosis etc etc. We want to extract out these headers where they look at the mean and std dev and do this for each row.

#Create a vector for the column names from Merged which will be used to select the mean & stddev columns only.
colNames=colnames(Merged)

# Create a vector that contains TRUE values for the Id, mean() & stddev() columns and FALSE for others
meanstddevVector = (grepl("Activity..",colNames) |
                   grepl("Subject..",colNames) |
                   grepl("mean..",colNames) |
                   grepl("std..",colNames))

#Create a final table that only keeps the columns of data wherein all conditions of meanstdevVector are TRUE.
part2data=Merged[meanstddevVector==TRUE]

####Moving on to part "3. Uses descriptive activity names to name the activities in the data set."

Merged$Activity <- as.character(Merged$Activity)
for (i in 1:6){
  Merged$Activity[Merged$Activity == i] <- as.character(activity_labels[i,2])
}
Merged$Activity <- as.factor(Merged$Activity)

##### Part "4 Appropriately label the data set with descriptive activity names"

# Cleaning up the variable names
names(Merged)

names(Merged)<-gsub("Acc", "Accelerometer", names(Merged))
names(Merged)<-gsub("Gyro", "Gyroscope", names(Merged))
names(Merged)<-gsub("BodyBody", "Body", names(Merged))
names(Merged)<-gsub("Mag", "Magnitude", names(Merged))
names(Merged)<-gsub("^t", "Time", names(Merged))
names(Merged)<-gsub("^f", "Frequency", names(Merged))
names(Merged)<-gsub("tBody", "TimeBody", names(Merged))
names(Merged)<-gsub("-mean()", "Mean", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("-std()", "STD", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("-freq()", "Frequency", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("angle", "Angle", names(Merged))
names(Merged)<-gsub("gravity", "Gravity", names(Merged))


####Create a second independent tidy data set w/ the average of each variable for each activity and subject.

#Firstly, let us set Subject as a factor variable.

Merged$Subject <- as.factor(Merged$Subject)
Merged <- data.table(Merged)

##Create "Tidy" as a data set with average for each activity and subject. Then write it into data file Tidy.txt

Tidy <- aggregate(. ~Subject + Activity, Merged, mean)
Tidy <- Tidy[order(Tidy$Subject,Tidy$Activity),]
write.table(Tidy, file = "Tidy.txt", row.names = FALSE)
