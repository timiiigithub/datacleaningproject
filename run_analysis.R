
## read in test data headers
  col_header_list <- read.delim("./UCI HAR Dataset/features.txt",header=FALSE,as.is=TRUE)

## strsplit the column headers in to index values and column lables
  col_header_list_v2 <- sapply(col_header_list$V1,strsplit," ",USE.NAMES=FALSE)

## subset the index and headers for the variables we need
  secondcolumn <- function(x){x[2]}
  firstcolumn <- function(x){x[1]}
  col_header_list_v3 <- sapply(col_header_list_v2, secondcolumn)
  col_labels <- subset(col_header_list_v3, grepl('mean|std', col_header_list_v3))
  col_index <- grep('mean|std',col_header_list_v3
  set_widths <- ifelse(grepl('mean|std', col_header_list_v3), 16, -16)
  rm(col_header_list)
  rm(col_header_list_v2)
  rm(col_header_list_v3)

## read in actvity lables
  activity_labels <- data.frame(id=c('0','0','0','0','0','0'), activity_name=c('0','0','0','0','0','0'), stringsAsFactors = FALSE)
  activity_labels_v1 <- read.delim("./UCI HAR Dataset/activity_labels.txt",header=FALSE,as.is=TRUE)
  activity_labels_v2 <- sapply(activity_labels_v1$V1,strsplit," ",USE.NAMES=FALSE)
  activity_labels$id <- sapply(activity_labels_v2, firstcolumn)
  activity_labels$activity_name <- sapply(activity_labels_v2, secondcolumn)
  rm(activity_labels_v1)
  rm(activity_labels_v2)
  
## Since the data is large, read only the "test" data for the required columns 
## as noted in col_labels and col_index
  ## read in all of the datsets for the test data, activity lables, and subject IDs
  test_data <- read.fwf('./UCI HAR Dataset/test/X_test.txt', widths=set_widths)
  test_activity_ids <- read.delim("./UCI HAR Dataset/test/y_test.txt",header=FALSE,as.is=TRUE)
  test_activity_ids$test_num <- c(1:nrow(test_activity_ids))
  test_subject_ids <- read.delim("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,as.is=TRUE)
  test_subject_ids$test_num <- c(1:nrow(test_subject_ids))

  ## apply the col_labels to the test_data set
  colnames(test_data) <- col_labels

  ## merge the activity ID and activity labels
  ## and then add the activity labels to the test_data
  merged_activity <- merge(test_activity_ids, activity_labels,by.x="V1",by.y="id",all=TRUE,sort=FALSE)
  merged_activity <- merged_activity[order(merged_activity$test_num),]
  test_data$activity_name <- merged_activity$activity_name

  ## combine the test_data with the subject IDS
  test_data$subject_id <- test_subject_ids$V1

  ## add a test data type column for combining "test" and "train" data later.
  test_data$data_type <- rep.int("test",nrow(test_data))

  ## clean up work area to conserve available memory
  rm(merged_activity)
  rm(test_activity_ids)
  rm(test_subject_ids)

## Since the data is large, read only the "train" data for the required columns 
## as noted in col_labels and col_index
  ## read in all of the datsets for the train data, activity lables, and subject IDs
  train_data <- read.fwf('./UCI HAR Dataset/train/X_train.txt', widths=set_widths)
  train_activity_ids <- read.delim("./UCI HAR Dataset/train/y_train.txt",header=FALSE,as.is=TRUE)
  train_activity_ids$train_num <- c(1:nrow(train_activity_ids))
  train_subject_ids <- read.delim("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,as.is=TRUE)
  train_subject_ids$train_num <- c(1:nrow(train_subject_ids))

  ## apply the col_labels to the test_data set
  colnames(train_data) <- col_labels

  ## merge the activity ID and activity labels
  ## and then add the activity labels to the train_data
  merged_activity <- merge(train_activity_ids, activity_labels,by.x="V1",by.y="id",all=TRUE,sort=FALSE)
  merged_activity <- merged_activity[order(merged_activity$train_num),]
  train_data$activity_name <- merged_activity$activity_name

  ## combine the test_data with the subject IDS
  train_data$subject_id <- train_subject_ids$V1

  ## add a test data type column for combining "test" and "train" data later.
  train_data$data_type <- rep.int("train",nrow(train_data))

  ## clean up work area to conserve available memory
  rm(merged_activity)
  rm(train_activity_ids)
  rm(train_subject_ids)


## combine the test and training datasets
  combined_data <- rbind(test_data,train_data)


## write out the dataset to the working directory in CSV format
  write.csv(combined_data, file="combined_data.csv", row.names=FALSE)

## clean up work area to conser available memory
  rm(test_data)
  rm(train_data)

## Create a second, independent tidy data set with the average of each variable
##for each activity and each subject.
library("reshape2", lib.loc="C:/Users/timni_000/Documents/R/win-library/3.1")
melted_data <- melt(combined_data, id.vars = c("activity_name","subject_id","data_type"))
average_data <- dcast(melted_data,activity_name + subject_id ~ variable, mean)
write.csv(average_data, file="tidy_average_data.csv", row.names=FALSE)
