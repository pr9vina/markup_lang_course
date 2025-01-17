### This .R file contains analysis (KNN + Isolation Forest)
### Install all needed libraries before running the script with install.packages("package_name")

## Libraries
library(dplyr)
library(caret)
library(isotree)

set.seed(123)

## load data 
train <- read.csv("processed_data/train_data.csv")
test <- read.csv("processed_data/test_data.csv")
train$Class <- as.factor(train$Class)
test$Class <- as.factor(test$Class)
table(train$Class) / nrow(train)

# Methods 
## KNN 
train_control <- trainControl(
  method = "cv",    
  number = 5, 
  search = "random"
)
# Test k values from 1 to 20
tune_grid <- expand.grid(k = seq(1, 20, by = 2))  

# Train the model with cross-validation
knn_model <- train(
  Class ~ .,
  data = train,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tune_grid
)
results <- knn_model$results

#plot accuracy vs k 
ggplot(results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Validation Curve: Accuracy vs. k",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

### Validation metrics
knn_predictions <- predict(knn_model, newdata = test)
knn_cm <- confusionMatrix(knn_predictions, test$Class)
accuracy <- knn_cm$overall["Accuracy"]
precision <- knn_cm$byClass["Pos Pred Value"]
recall <- knn_cm$byClass["Sensitivity"]
f1_score <- knn_cm$byClass["F1"]


### Isolation Forest 
data_iso_train <- train 

cross_validate_isolation_forest <- function(data, n_folds = 2, threshold = 0.3) {
  folds <- sample(rep(1:n_folds, length.out = nrow(data)))
  metrics_list <- list()
  
  for (fold in 1:n_folds) {
    train_data <- data[folds != fold, ]
    val_data <- data[folds == fold, ]
    iso_model <- isolation.forest(train_data)
    anomaly_scores <- predict(iso_model, val_data)
    predictions <- ifelse(anomaly_scores > threshold, "1", "0")
    predictions <- factor(predictions, levels = c("0", "1"))
    true_labels <- factor(val_data$Class, levels = c("0", "1"))
    
    cm_iso <- confusionMatrix(predictions, true_labels, positive = "1")
    # print(cm_iso)
    metrics <- list(
      accuracy = cm_iso$overall['Accuracy'],
      precision = cm_iso$byClass['Precision'],
      recall = cm_iso$byClass['Recall'],
      f1_score = cm_iso$byClass['F1']
    )
        metrics_list[[fold]] <- metrics
  }
  
  aggregated_metrics <- sapply(metrics_list, function(x) unlist(x))
  aggregated_metrics <- rowMeans(aggregated_metrics)
  
  return(aggregated_metrics)
}

iso_results <- cross_validate_isolation_forest(data=data_iso_train)


