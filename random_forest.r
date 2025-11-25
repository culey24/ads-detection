# ==============================================================================
# STEP 1: SETUP - LIBRARIES
# ==============================================================================

# Load libraries
library(tidyverse)
library(randomForest)
library(caret)

# ==============================================================================
# STEP 2: DATA LOADING
# ==============================================================================

# Doc file du lieu
# Important: Ensure the file is in your current working directory.
tryCatch({
  # FIX: Changed variable name from 'data' to 'ads_data' to avoid system
  # conflict
  # FIX: Used simple filename "data.csv" assuming files are in the same folder
  ads_data <- read.csv("ads-detection/data.csv")
  print("Data loaded successfully!")
}, error = function(e) {
  print("Error: Cannot find csv file. Please check the path.")

  # Create dummy data for demo purposes if file is missing
  set.seed(1)
  # FIX: Using 'ads_data' instead of 'data'
  ads_data <<- data.frame(
    time_spent = rnorm(1000, 50, 10),
    age = runif(1000, 18, 60),
    area_income = rnorm(1000, 50000, 5000),
    internet_usage = rnorm(1000, 150, 30),
    ad_status = sample(c(0, 1), 1000, replace = TRUE)
  )
  print("Generated dummy data for demonstration.")
})

# Quick look at data structure
str(ads_data)

# ==============================================================================
# STEP 3: PREPROCESSING
# ==============================================================================

# 1. Handle Missing Values
data_clean <- na.omit(ads_data)

# 2. Convert Target Variable to Factor
# Required for Classification in R
data_clean$ad_status <- as.factor(data_clean$ad_status)

# Check target distribution
print("Target variable distribution:")
print(table(data_clean$ad_status))

# ==============================================================================
# STEP 4: SPLITTING
# ==============================================================================

set.seed(123)

# Split data 80/20
train_index <- createDataPartition(data_clean$ad_status,
                                   p = 0.8,
                                   list = FALSE)

train_data <- data_clean[train_index, ]  # 80% Training data
test_data  <- data_clean[-train_index, ] # 20% Test data

print(paste("Training set size:", nrow(train_data)))
print(paste("Test set size:", nrow(test_data)))

# ==============================================================================
# STEP 5: MODELING
# ==============================================================================

print("Training Random Forest model...")

# Train model
# Formula: ad_status ~ . (Predict ad_status using all other variables)
rf_model <- randomForest(ad_status ~ .,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Model summary
print(rf_model)

# ==============================================================================
# STEP 6: EVALUATION
# ==============================================================================

# 1. Predict on Test set
predictions <- predict(rf_model, newdata = test_data)

# 2. Create Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_data$ad_status,
                               mode = "everything")

# Print detailed results
print("=== TEST SET EVALUATION RESULTS ===")
print(conf_matrix$table)

# 3. Extract and print key metrics
accuracy  <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall    <- conf_matrix$byClass["Recall"]

cat("\n--- Key Metrics ---\n")
cat(sprintf("Overall Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision:        %.4f\n", precision))
cat(sprintf("Recall:           %.4f\n", recall))

# ==============================================================================
# STEP 7: VISUALIZATION
# ==============================================================================

# Variable Importance Plot
varImpPlot(rf_model,
           main = "Variable Importance Plot",
           col = "blue",
           pch = 19)