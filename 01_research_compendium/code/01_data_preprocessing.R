### This .R file contains EDA and data preprocessing 
### Install all needed libraries before running the script with install.packages("package_name")

library(tidyverse)
library(data.table) 
library(corrplot)
library(ggplot2)
library(caret)
library(dplyr)
library(smotefamily)
library(ROSE)
library(themis)

# For reproducible results 
set.seed(123)  

# Data loading
train_data <- read.csv("raw_data/Train.csv")

# Plots for analysing the data 
plot_feature <- function(data, feature_name) {
  p1 <- ggplot(data, aes_string(x = feature_name)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black") 
  print(p1)
  p2 <- ggplot(data, aes_string(x = feature_name)) +
    geom_boxplot(fill = "blue", color = "black")
  print(p2)
  
  p3 <- ggplot(data, aes_string(x = 'as.factor(Class)', y = feature_name)) +
      geom_boxplot(fill = "blue", color = "black") 
    print(p3)
}
plot_feature(train_data, 'feature_1') # lots of outliers, if feature_1 > 200, it seems more likely that Class = 0
plot_feature(train_data, 'feature_2') # lots of outliers, if feature_2 > 200, Class = 1
plot_feature(train_data, 'feature_3') #lots of outliers, if feature_2 > 10, Class = 1

# Check missing values 
missing_counts <- colSums(is.na(train_data))
missing_counts[missing_counts > 0] # no missings 

# Check near zero values 
nzv <- nearZeroVar(train_data)
nzv

# Inspect unique values 
unique_counts <- sapply(train_data, function(x) length(unique(x)))
# Show how many have 1, 2, or 3+ unique values
sum(unique_counts == 1)
sum(unique_counts == 2)
sum(unique_counts >= 3)
# Keep only 1558 features that have different values (so std != 0)
cols_keep <- unique_counts > 1
length(cols_keep) 
data_keep <- train_data[, cols_keep]

# Check for duplicated rows 
duplicates <- data_keep[duplicated(data_keep), ]
dim(duplicates) 
sum(duplicated(data_keep)) 

# Outliers inspections
detect_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)  # Count of outliers
}
outlier_counts <- sapply(data, function(col) {
  if (is.numeric(col)) detect_outliers_iqr(col) else NA
})
outlier_counts[!is.na(outlier_counts) & outlier_counts > 0] # features has lots of outliers, we won't 

# Standardize the data
# Because we have lots of outliers, Robust Scaling is applied 
# It removes the median and scales the data according to the IQR
robust_scaling <- function(data) {
  data_copy <- data
  numeric_cols <- sapply(data_copy, is.numeric)  
  
  for (col in names(data)[numeric_cols]) {
    col_median <- median(data[[col]], na.rm = TRUE)
    col_iqr <- IQR(data[[col]], na.rm = TRUE)
    data_copy[[col]] <- (data[[col]] - col_median) / col_iqr
  }
  
  return(data_copy)
}
numeric_cols <- c("feature_1", "feature_2", "feature_3")
data_to_scale <- data_keep %>% select(numeric_cols)
scaled_data <- robust_scaling(data_to_scale)

data_1 <- data_keep %>% select(-numeric_cols)
data_combined <- bind_cols(scaled_data, data_1)
dim(data_combined)

## Oversampling 
# Check class distribution
table(data_combined$Class) / nrow(data_combined)

# Apply oversampling 
data_combined$Class <- as.factor(data_combined$Class)
smote_data <- smotenc(data_combined, var = "Class",  over_ratio = 0.4)
shuffled_data <- smote_data[sample(nrow(smote_data)), ]
dim(smote_data)

# Split on train and validation 
trainIndex <- createDataPartition(shuffled_data$Class, p = 0.8, list = FALSE)
train_data <- shuffled_data[trainIndex, ]
test_data <- shuffled_data[-trainIndex, ]
dim(train_data)
dim(test_data)
table(train_data$Class) / nrow(train_data)
table(test_data$Class) / nrow(test_data)

# Save preprocessed data
write.csv(train_data, "processed_data/train_data.csv")
write.csv(test_data, "processed_data/test_data.csv")
