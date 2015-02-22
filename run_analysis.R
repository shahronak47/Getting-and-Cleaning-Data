#Q1 - Merges the training and the test sets to create one data set.
#merging data from training and testing set

mergedData <- rbind(X_train, X_test)


#Q2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

#finding column names that have mean in it
#grep function would find columns which has mean in it

mean_index <- grep("mean", features$V2)

#46 index positions for mean
mean_colnames <- features$V2[mean_index]

#finding column names that have std in it
#grep function would find columns which has std in it

std_index <- grep("std", features$V2)

#33 index positions for std

std_colnames <- features$V2[std_index]

#data table for mean columns
mergedData_mean <- mergedData[,mean_index]
#data table for std columns
mergedData_std <- mergedData[, std_index]
#Merging the two data frames
mergedData_mean_std <- cbind(mergedData_mean, mergedData_std)


#Q3 - Uses descriptive activity names to name the activities in the data set

#we need to add a new column in this data frame with test and train labels.
#Test and train labels should be in the same column hence using rbind to combine training and test labels
#and then using cbind to combine it with the data frame

mergedData_mean_std <- cbind(mergedData_mean_std, rbind(y_train, y_test))

#Renaming the column name to label, hence it is easy to identify

colnames(mergedData_mean_std)[80] <- "label"

#Now to replace every activity number with descriptive terms
#creating a new column name label for the descriptive terms

for(i in 1:10299)
{
  if(mergedData_mean_std$labels[i] == 1)
    mergedData_mean_std$label[i] = "WALKING"
  if(mergedData_mean_std$labels[i] == 2)
    mergedData_mean_std$label[i] = "WALKING_UPSTAIRS"
  if(mergedData_mean_std$labels[i] == 3)
    mergedData_mean_std$label[i] = "WALKING_DOWNSTAIRS"
  if(mergedData_mean_std$labels[i] == 4)
    mergedData_mean_std$label[i] = "SITTING"
  if(mergedData_mean_std$labels[i] == 5)
    mergedData_mean_std$label[i] = "STANDING"
  if(mergedData_mean_std$labels[i] == 6)
    mergedData_mean_std$label[i] = "LAYING"
}

table(mergedData_mean_std$labels)

# 1    2    3    4    5    6 
# 1722 1544 1406 1777 1906 1944 
#Now deleting the previously numbered column named labels

mergedData_mean_std$labels <- NULL

#Conmfirming if the mapping from number to descriptive terms is properly done

table(mergedData_mean_std$label)
#LAYING            SITTING           STANDING            WALKING 
#1944               1777               1906               1722 
#WALKING_DOWNSTAIRS   WALKING_UPSTAIRS 
#1406               1544 


#Q4 - Appropriately labels the data set with descriptive variable names. 

#Labelling the column names with descriptive terms
#we already have fetched the column names in mean_colnames and std_colnames
#so now we just need to paste it in this data_frame
#the point to remember here is we first merged the mean columns and then std columns
#so we need to do the same while adding column names

colnames(mergedData_mean_std)[1:46] <- mean_colnames
colnames(mergedData_mean_std)[47:79] <- std_colnames

#there are certain columns which are not labelled properly, renaming them manually
#column names consist of 'body' word twice so removing one of it

colnames(mergedData_mean_std)[41] <- "fBodyAccJerkMag-mean()"
colnames(mergedData_mean_std)[42] <- "fBodyAccJerkMag-meanFreq()" 
colnames(mergedData_mean_std)[43] <- "fBodyGyroMag-mean()" 
colnames(mergedData_mean_std)[44] <- "fBodyGyroMag-meanFreq()"  
colnames(mergedData_mean_std)[45] <- "fBodyGyroJerkMag-mean()"
colnames(mergedData_mean_std)[46] <- "fBodyGyroJerkMag-meanFreq()"
colnames(mergedData_mean_std)[77] <- "fBodyAccJerkMag-std()" 
colnames(mergedData_mean_std)[78] <- "fBodyGyroMag-std()"  
colnames(mergedData_mean_std)[79] <- "fBodyGyroJerkMag-std()" 


#Q5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


colnames(mergedData_mean_std)[80] <- "activity"

#Adding subject names to the data set

mergedData_mean_std <- cbind(mergedData_mean_std, rbind(subject_train, subject_test))
colnames(mergedData_mean_std)[81] <- "subject"

#reordering columns so that subject and activity come in first 2 columns

mergedData_mean_std <- mergedData_mean_std[, c(80, 81, 1:79)]

#Creating a new final data frame

final <- data.frame()

#Creating a temp data frame just for checking purpose
#temp <- head(filter(mergedData_mean_std, activity == activity_labels$V2[1] & subject == 1))
#temp <- temp[, -c(1,2)]
#temp1 <- data.frame(apply(temp, 2, mean))
#col_names <- data.frame(colnames(mergedData_mean_std)[3:81])
#final <- cbind(col_names, temp1)
#final <- cbind(final, activity_labels$V2[1], 1)
#This worked pefectly

col_names <- data.frame(colnames(mergedData_mean_std)[3:81])

for(i in 1:6)
{
  for(j in 1:30)
  {
    temp<- filter(mergedData_mean_std, activity == activity_labels$V2[i] & subject == j)
    temp <- temp[, -c(1,2)]
    temp1 <- data.frame(apply(temp, 2, mean))
    temp2 <- cbind(activity_labels$V2[i], j, col_names, temp1)
    final <- rbind(final, temp2)
    
  }
}

#Renaming columns
colnames(final)[1] <- "activity"
colnames(final)[2] <- "subject"
colnames(final)[3] <- "feature"
colnames(final)[4] <- "mean"
