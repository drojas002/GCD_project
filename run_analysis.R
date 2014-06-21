setwd("C:/RWorkspace/P3/project")

###### 1 - Merges the training and the test sets to create 
######     one data set.

## read the files
train_data <- read.table("./train/X_train.txt")
train_label <- read.table("./train/y_train.txt")
train_subject <- read.table("./train/subject_train.txt")
test_data <- read.table("./test/X_test.txt")
test_label <- read.table("./test/y_test.txt")
test_subject <- read.table("./test/subject_test.txt")

## join the files
data <- rbind(train_data,test_data) # 10299 x 561,  561 fetures
label <- rbind(train_label,test_label) # 10299 # 1
subject <- rbind(train_subject,test_subject) # 10299 # 1

###### 2 - Extracts only the measurements on the mean and 
######     standard deviation for each measurement.

features <- read.table("./features.txt")
meanSTD_i <-grep("mean\\(\\)|std\\(\\)", features[, 2])

## select the mean and std measurements from data
data <- data[,meanSTD_i] # 10299 x 66
## asign the variable names
names(data)=features[meanSTD_i,2]
names(data) <- gsub("\\(\\)", "", features[meanSTD_i, 2]) # remove "()"
names(data) <- gsub("mean", "Mean", names(data)) # standardize to Mean
names(data) <- gsub("std", "STD", names(data)) # standardize to STD
#names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

###### 3 - Uses descriptive activity names to name the 
######     activities in the data set
# read file
activity <- read.table("./activity_labels.txt")
# subset the activity names
activity<- gsub("_","",activity[,2])
# change the label by the activity 
activity_L <- activity[label[,1]]
label[,1] <- activity_L
# asign a name for label data frame
names(label) = "activity"
names(subject) = "subject"

###### 4 - Appropriately labels the data set with 
######     descriptive variable names.
dataSet <- cbind(subject, label, data)


###### 5 - Creates a second, independent tidy data set with the average 
######     of each variable for each activity and each subject. 
## Tidy data rows= activity, subject.  cols =cols
r<- 6*30; c<- dim(dataSet)[2];
Mat <- matrix(NA,nrow=r,ncol=c)
Mat <- as.data.frame(Mat)
names(Mat) = names(dataSet)

row <- 1
for(i in 1:30) {
  for(j in 1:6) {
    Mat[row, 1] <- sort(unique(subject)[, 1])[i]
    Mat[row, 2] <- activity[j]
    aux1 <- i == dataSet$subject
    aux2 <- activity[j] == dataSet$activity
    Mat[row, 3:c] <- colMeans(dataSet[aux1&aux2, 3:c])
    row <- row + 1
  }
}

head(Mat)
write.table(Mat, "./tidy_data_frame.txt") 


