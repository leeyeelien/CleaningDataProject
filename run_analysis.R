#Load dyplr library
library (dyplr)

#Load labels and features tables
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

#Create feature description from features table
feature_names = as.vector(features[,2])

#Load train data tables
train_dataset <- read.table("UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subj <- read.table("UCI HAR Dataset/train/subject_train.txt")

#Merge all train data tables to train_dataset
train_dataset <- bind_cols(train_y, train_dataset)
train_dataset <- bind_cols(train_subj, train_dataset)

#Load test data tables
test_dataset <- read.table("UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subj <- read.table("UCI HAR Dataset/test/subject_test.txt")

#Merge all test data tables to test_dataset
test_dataset <- bind_cols(test_y, test_dataset)
test_dataset <- bind_cols(test_subj, test_dataset)

#Merge train and test set
complete_data <- bind_rows(train_dataset,test_dataset)

#Rename all the columns to subject, label, and feature names
feature_names <- c("Subject", "Activity", feature_names)
colnames(complete_data)<-feature_names 

#Extract columns with mean and std dev
m_s <- feature_names[grepl(".*mean\\(\\)|.*std\\(\\)", feature_names)]
mean_std_data <- select (complete_data, c(Subject,Activity,all_of(m_s)))

#Replace activity number with activity labels (in the labels column)
mean_std_data$Activity <- activity_labels[mean_std_data$Activity,2]

#Make variables names more descriptive/readable
variable_names <- names(mean_std_data)
variable_names <- sub("\\(\\)", "",variable_names) #remove the parentheses
variable_names <- gsub("-", "",variable_names) #remove the dashes
variable_names <- sub("mean", "Mean",variable_names)
variable_names <- sub("std", "StdDev",variable_names)
colnames(mean_std_data) <- variable_names

#Create a factor of subject and activities
mean_std_data$Subject <- factor(mean_std_data$Subject)
mean_std_data$Activity <- factor(mean_std_data$Activity)

#Create a table for summary of average values
summary_data <- mean_std_data %>% group_by(Subject,Activity) %>% summarise_all(mean)

#write the tidy datasets to files
write.table(mean_std_data, file="UCI HAR Dataset/tidy_dataset.txt", sep=",", col.names=TRUE)
write.table(summary_data, file="UCI HAR Dataset/tidy_summary.txt", sep=",", col.names=TRUE)
