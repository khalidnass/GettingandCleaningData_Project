
library(sqldf)
options(gsubfn.engine = "R")
require(RH2) 
# the Samsung data must be in your working directory. if not use the below function to set your active directory.
setwd("D:\\R\\Getting and Cleaning Data\\")

subject_test_file  <- file("UCI HAR Dataset\\test\\subject_test.txt") 
xtest_file         <- file("UCI HAR Dataset\\test\\X_test.txt")
ytest_file         <- file("UCI HAR Dataset\\test\\y_test.txt")

subject_train_file <- file("UCI HAR Dataset\\train\\subject_train.txt") 
Xtrain_file        <- file("UCI HAR Dataset\\train\\X_train.txt")
ytrain_file        <- file("UCI HAR Dataset\\train\\y_train.txt")

features           <- file("UCI HAR Dataset\\features.txt") 

Activity_Labels_file <- file("UCI HAR Dataset\\activity_labels.txt") 

#1. Merges the training and the test sets for Y and subject files to DataSet  test.df  . 

#2. Load the test sets for Y and test subject files to Data Frame. 
test.list          <- lapply((readLines(ytest_file)), as.integer)
test.df            <- data.frame( Activity  =  matrix(unlist(test.list), nrow= length(test.list), byrow=T))
test.df$subject    <-  lapply((readLines(subject_test_file)), as.integer)


#3. Load the training sets for Y and training subject files to Data Frame train.df . 
train.list         <- lapply((readLines(ytrain_file)), as.integer) 
train.df           <- data.frame( Activity =  matrix(unlist(train.list ), nrow= length(train.list), byrow=T))
train.df$subject   <-  lapply((readLines(subject_train_file)), as.integer) 

#4. add Train/Test label to both data frame
train.df$Type      <- "Train"
test.df$Type       <- "Test"

#5. Load All training and the test sets for X. Don't increase buffer size unless you have enough memory. 
#if you increase the buff size it will speed up the process
X_test  <- read.fwf(  file= xtest_file, buffersize =100 , widths= rep(16,561)) 
X_train <- read.fwf(  file= Xtrain_file, buffersize =100 , widths= rep(16,561)) 

#6.  Extracts only the measurements on the mean and standard deviation for each measurement using Mfeatures.df 

# Load featuers file. 
features.df <- read.table(file=features )
colnames(features.df) <- c("Num","Name") 
 
Mfeatures.df <- features.df[(grepl("mean", features.df$Name, ignore.case=TRUE) | 
                            grepl("std", features.df$Name, ignore.case=TRUE)),]
 
Mfeatures.df$Name <- gsub("[[:punct:]]", "", Mfeatures.df$Name)
 
 
X_test.M            <- X_test[,Mfeatures.df$Num]
X_train.M           <- X_train[,Mfeatures.df$Num]

#7. Uses descriptive activity names to name the activities in the data set
count <- 1
for(i in Mfeatures.df$Name)
{
  names(X_test.M)[count]<- i
  names(X_train.M)[count]<- i
  X_test.M[,count] <- as.numeric(X_test.M[,count])
  X_train.M[,count] <- as.numeric(X_train.M[,count])
  count <- count + 1
}

#8. Merg All Data and create the dataset 
test.df  <- cbind(test.df  , X_test.M )
train.df  <- cbind(train.df  , X_train.M )
 
dataset  <- rbind(test.df , train.df)  
dataset$subject <- sapply(dataset$subject,FUN = paste,collapse = "-")

#9. Appropriately labels the data set with descriptive activity names.
Activity.df <- read.table(file=Activity_Labels_file )
colnames(Activity.df) <- c("Num","Activity") 
 
#datasett <- dataset
dataset$Activity  <- as.character(dataset$Activity)
 
for(i in 1:nrow(dataset))
{
  value <- which( Activity.df$Num %in%  dataset$Activity[i] )
  
  if(length(value > 0))
  {
    dataset$Activity[i] <- as.character(Activity.df$Activity[value])
  }
  else
  {
    dataset$Activity[i] <- NA
  }  
}

#10.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
query <- paste("select subject , count(subject) count from dataset group by subject  order by subject")
SubjectData <- sqldf(query)
  
for(i in 1:nrow(SubjectData))
{
   
    subjectvalue <- as.numeric(SubjectData$subject[i])
  
  for(j in 1:nrow(Activity.df))
  {
    
    ActivityValue <-  as.character(Activity.df[j,2])
    
    tempDataSet <- dataset[  which(  dataset$subject == subjectvalue  & dataset$Activity  == ActivityValue)  , 4:ncol(dataset)]
     mean <- colMeans(tempDataSet ,na.rm= TRUE) 
    
    mean <-  c(subjectvalue,ActivityValue, mean)
    tempDataSet <- cbind(  data.frame( Subject = subjectvalue)  , data.frame( Activity = ActivityValue) , tempDataSet)  
  
    #tempDataSet$Subject <- subjectvalue
    #tempDataSet$Activity <- ActivityValue
    
    if(i == 1 && j == 1)
    {
      AvgDataSet <- tempDataSet  
      AvgDataSet <- AvgDataSet[1 == 2,] # remove all rows
    }
    #  AvgDataSet <- rbind(AvgDataSet, as.list(mean))
    AvgDataSet <- rbind(AvgDataSet,do.call(data.frame,setNames(as.list(mean), names(AvgDataSet)))) 
  }
}
 

#11. Save both tables
write.table(dataset, "dataset.txt")
write.table(AvgDataSet, "groupActivitySubject.txt")
 
close(subject_test_file)
close(xtest_file)
close(ytest_file)
close(subject_train_file)
close(Xtrain_file)
close(ytrain_file)
close(features)
close(Activity_Labels_file)