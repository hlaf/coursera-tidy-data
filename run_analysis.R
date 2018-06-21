library(data.table)
library(dplyr)
#library(utils)

DATA_DIR <- './data'
RAW_DATA_ARCHIVE <- file.path(DATA_DIR, 'samsung_raw.zip')
RAW_DATA_DIR <- file.path(DATA_DIR, 'UCI HAR Dataset')
DEBUG = FALSE

if (!file.exists(DATA_DIR)) {
  dir.create(DATA_DIR)
}

readInputDataSet <- function(dataset_id) {
  
  subjects_filename <- paste0('subject_', dataset_id, '.txt')
  activities_filename <- paste0('y_', dataset_id, '.txt')
  features_filename <- paste0('X_', dataset_id, '.txt')
  dataset_dir <- file.path(RAW_DATA_DIR, dataset_id)
  
  subject_ids <- fread(file.path(dataset_dir, subjects_filename), header=FALSE)
  names(subject_ids) <- 'subject'
  
  activity_ids <- fread(file.path(dataset_dir, activities_filename), header=FALSE)
  names(activity_ids) <- 'activity'
  
  X <- fread(file.path(dataset_dir, features_filename), header=FALSE)
  feature_names <- read.table(file.path(RAW_DATA_DIR, 'features.txt'),
                              header=FALSE,
                              stringsAsFactors=FALSE)[, 2]
  names(X) <- feature_names
  
  data <- cbind(subject_ids, activity_ids, X)
  data
}

# Download the raw (compressed) data archive
dataset_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists(RAW_DATA_ARCHIVE)) {
  download.file(dataset_url, destfile = RAW_DATA_ARCHIVE, mode='wb')
}

# Decompress the raw data
if (!file.exists(RAW_DATA_DIR)) {
  unzip(RAW_DATA_ARCHIVE, exdir=DATA_DIR)
}

# Read the test and training sets
data_test <- readInputDataSet('test')
data_train <- readInputDataSet('train')

# Merge the test and training sets (Requirement 1).
data <- rbind(data_test, data_train)

# Extract only the variables that are estimates of either the mean or the
# standard deviation of each measurement (Requirement 2).
variables_of_interest <- c(names(data)[1:2], 
                           grep('(mean|std)\\(\\)', names(data), value=TRUE))
data <- data[, ..variables_of_interest]

if (DEBUG) { # Convert to tibble for debugging purposes
  data <- tibble::as.tibble(data)
}

# Convert the numeric activity IDs to descriptive names (Requirement 3).
activity_labels <- read.table(file.path(RAW_DATA_DIR, 'activity_labels.txt'),
                              header=FALSE)
data <- mutate(data, activity = activity_labels[activity, 2])

# Label the data set with descriptive variable names (Requirement 4).

# Make the time- or frequency-domain nature of the variables more explicit
names(data) <- gsub('^t', 'time', names(data))
names(data) <- gsub('^f', 'freq', names(data))

# Remove hyphenation and parentheses
names(data) <- gsub('\\(|\\)|-', '', names(data))

# Use camel case for the mean and std keywords
names(data) <- gsub('mean', 'Mean', names(data))
names(data) <- gsub('std', 'Std', names(data))

# Fix the BodyBody typo affecting some variables in the raw dataset
names(data) <- gsub("BodyBody", "Body", names(data))

# Make sure the axial component identifiers appear in the same relative position
# as the one used for magnitude 'Mag', so that all measurement variables names
# end in either 'Mean' or 'Std'
names(data) <- gsub("(Mean|Std)(X|Y|Z)", "\\2\\1", names(data), fixed=FALSE)

# Create a tidy data set with the average of each variable for each activity
# and each subject (Requirement 5).
tidy_data <- {data %>% group_by(subject, activity) %>% summarise_all(funs(mean))}

# Save the tidy data set
write.table(tidy_data, './tidy_data.txt', row.name=FALSE)
