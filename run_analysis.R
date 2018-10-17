# The code runs the analysis on the HAR Data set available on the UCI machine
# learning website.
rm(list = ls())
################################################################################
# loading the relevant libraries
library(dplyr)


################################################################################
# Set Current directory as the working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()
setwd("Dataset\\UCI HAR Dataset")

dir()
# Loading the activity labels.
activity_labels <- read.table("activity_labels.txt", header = FALSE)
activity_labels
# loading the features 
features <- read.table("features.txt", header = FALSE)
features[,2] <- as.character(features[, 2])
str(features)


################################################################################
# Test data
setwd("test")
dir()

x_test <- read.table("X_test.txt", header = FALSE)
y_test <- read.table("y_test.txt", header = FALSE)
sub_test <- read.table("subject_test.txt", header = FALSE)

colnames(x_test) <- features[,2]
colnames(y_test) <- "Activity_ID"
colnames(sub_test) <- "Subject_ID"

test_df <- cbind(sub_test, y_test, x_test) 

# Train data
setwd("..")
setwd("train")


x_train <- read.table("X_train.txt", header = FALSE)
y_train <- read.table("y_train.txt", header = FALSE)
sub_train <- read.table("subject_train.txt", header = FALSE)


colnames(x_train) <- features[,2]
colnames(y_train) <- "Activity_ID"
colnames(sub_train) <- "Subject_ID"

train_df <- cbind(sub_train, y_train, x_train)
###############################################################################
# Merging the test and train data frame

Merged_df <- rbind(test_df, train_df)

###############################################################################
# Extracting the mean and standard deviation observations. 
# forming the colNames vector - names 
# finding the variables with name mean and std (for standard deviation in the file)
features_required <- unique(grep("mean|std", ignore.case = TRUE, features[,2]))
features_required
names_of_columns <- features[features_required, 2]

Merged_df_1 <- Merged_df[ ,-(1:2)]
Merged_df_1 <- Merged_df_1[, names_of_columns]
Relevant_df <- cbind(Merged_df[ , (1:2)], Merged_df_1)

################################################################################
setwd("..")
colnames(activity_labels) <- c("Activity_ID", "Action")

activity_label_df <- merge(activity_labels, Relevant_df, by.x = "Activity_ID", 
                           by.y = "Activity_ID")


################################################################################
activity_label_df$Activity_ID <-
        as.factor(as.character(activity_label_df$Activity_ID))
str(activity_label_df$Activity_ID)

summary_df <- activity_label_df %>% group_by(Activity_ID) %>% 
        summarize_at(vars(-(Activity_ID:Subject_ID)), 
                     funs(mean( . , na.rm = TRUE)))

setwd("..")
setwd("..")
write.table(summary_df, "tidy_data.txt", row.names = FALSE)

