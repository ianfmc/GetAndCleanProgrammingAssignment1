createTidyTable <- function() {
  
  c.RowsToRead = 100
  setwd("UCI HAR Dataset")
  
  print(Sys.time())
  
  #### step 0. read metadata files
  
  ## begin with the two metadata files: the components of the feature 
  ## vector (in "features.txt") and the activity number and name cross-
  ## reference (in "activity_labels.txt")
  
  ## read in the feature names for the complete feature vectory.
  ## the names of the features will become the names of the variables
  ## for each observations
  
  features.df <- read.table("features.txt", 
                            sep="\n", 
                            header=FALSE,
                            stringsAsFactors=FALSE)
  
  ## strip the number from the start of the name for a more 
  ## readable name
  
  names(features.df) = c("index")    
  
  feature.names <- sapply(features.df$index, 
                          function(s) strsplit(s, " "))
  features.df[,"index"] = sapply(feature.names, 
                                 "[[",
                                 2)
  
  ## read in the activity names; split the fixed-width contents of 
  ## the rows into two columns
  
  activity.df <- read.table("activity_labels.txt", 
                            sep="\n", 
                            header=FALSE,
                            stringsAsFactors=FALSE)
  
  names(activity.df) = c("index")    
  activity.df$name <- sapply(activity.df$index, 
                             function(s) substring(s, 3, nchar(s)))
  activity.df$index <- sapply(activity.df$index,
                              function(s) substring(s, 1, 2))
  activity.df$index <- as.numeric(activity.df$index)
   
  #### step 1. read and merge the observation files from the 
  #### test and training data sets
  
  ## read in the subject numbers from both the test and training data; 
  ## the subject number is one of the variables in each observation
  
  subject.df <- data.frame(subject=as.integer())
 
  s <- read.table("train/subject_train.txt",
                  sep="\n",
                  nrows=c.RowsToRead,
                  header=FALSE)
  names(s) <- c("subject")
  subject.df <- rbind(subject.df, s)
  
  s <- read.table("test/subject_test.txt",
                  sep="\n",
                  nrows=c.RowsToRead,
                  header=FALSE)
  names(s) <- c("subject")
  subject.df <- rbind(subject.df, s)
    
  names(subject.df) <- c("Subject.Number")

  ## read in the feature vector and activity type for each 
  ## observation from both the train and test data sets
  
  ## feature vector
  
  x.df <- data.frame(observation=as.character())

  x <- read.table("train/X_train.txt",
                  sep="\n", 
                  nrows=c.RowsToRead,
                  header=FALSE)
  names(x) <- c("observation")
  x.df <- rbind(x.df, x)
  
  x <- read.table("test/X_test.txt",
                  sep="\n", 
                  nrows=c.RowsToRead,
                  header=FALSE)
  names(x) <- c("observation")
  x.df <- rbind(x.df, x)
  
  ## activity type  
  
  y.df <- data.frame(observation=as.integer())
  
  y <- read.table("train/y_train.txt", 
                     sep="\n", 
                     nrows=c.RowsToRead,
                     header=FALSE)
  names(y) <- c("observation")
  y.df <- rbind(y.df, y)
  
  y <- read.table("test/y_test.txt", 
                  sep="\n", 
                  nrows=c.RowsToRead,
                  header=FALSE)
  names(y) <- c("observation")
  y.df <- rbind(y.df, y)
  
  names(y.df) <- c("index")
  
  #### data conditioning
  
  ## convert the activity label index for each observation into the activity
  ## label (via a 'table lookup') and collapse the frame to contain only the 
  ## activity label
      
  m <- match(y.df$index, activity.df$index)
  y.df$Activity.name <- sapply(m, 
                               function(s) activity.df$name[s])
  
  y.df <- subset(y.df, select=c("Activity.name"))
  
  ## convert the fixed width observation (X) record contents 
  ## into the individual (numeric) feature vector values
  
  x.string <- as.character(x.df[,1])
  x.observations <- lapply(x.string, function(s) as.numeric(substring(s, 
                                                                      seq(1, nchar(s), 16),
                                                                      seq(16, nchar(s), 16))))
  
  ## convert all feature vectors into a data frame and use the feature
  ## frame as the names for feature vector variables
  
  obs.df <- as.data.frame(do.call(rbind, x.observations))
  names(obs.df) <- features.df[,"index"]
  
  ## bind the activity label with the feature vector
  
  obs.df <- cbind.data.frame(y.df, obs.df)
  obs.df <- cbind.data.frame(subject.df, obs.df)
  
  #### step 2. extract only those measures that measure the mean
  #### or standard deviation
  
  o.cols <- features.df[grep(".*(mean|std)",features.df$index),]
  obs.df <- subset(obs.df, select=c("Subject.Number", "Activity.name", o.cols))
  
  #### step 3. Change feature vector names to be more descriptive
  
  ## change lead 't' or 'f' to time or frequency
  
  names(obs.df) <- gsub("(-|,)", "", names(obs.df))
  names(obs.df) <- gsub("^t", "Time", names(obs.df))
  names(obs.df) <- gsub("^f", "Frequency", names(obs.df))
  names(obs.df) <- gsub("Acc", "Acceleration", names(obs.df))
  names(obs.df) <- gsub("mean\\(\\)", "Mean", names(obs.df))
  names(obs.df) <- gsub("std\\(\\)", "StandardDeviation", names(obs.df))
  
  #### Step 5. output the independent, tidy data set
  
  ## write the table to a file
  ## do not include row names in the file
  
  write.table(obs.df,
              "Tidy-UCI-HAR-Data.txt",
              row.names=FALSE,
              col.names=TRUE)
  
  print(Sys.time())
  names(obs.df)
}