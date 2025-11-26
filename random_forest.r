# ==============================================================================
# STEP 1: SETUP - LIBRARIES
# ==============================================================================

library(tidyverse)
library(randomForest)
library(caret)

# ==============================================================================
# STEP 2: DATA LOADING
# ==============================================================================

# Read data file
tryCatch({
  # Handle potential missing values represented by "?" or "NA"
  ads_data <- read.csv("ads-detection/clean_data.csv",
                       na.strings = c("?", "NA"))
  print("Data loaded successfully!")
}, error = function(e) {
  print("Error: Cannot find csv file. Please check the path.")
  stop(e)
})

# ==============================================================================
# STEP 3: PREPROCESSING
# ==============================================================================

# Remove rows with missing values
ads_data <- na.omit(ads_data)

# Ensure the target variable 'Label' exists and is a factor
if ("Label" %in% colnames(ads_data)) {
  ads_data$Label <- as.factor(ads_data$Label)
} else {
  stop("Error: Column 'Label' not found in the dataset.")
}

# Clean column names to ensure they are valid R identifiers
colnames(ads_data) <- make.names(colnames(ads_data))

# ==============================================================================
# STEP 4: SPLIT DATA
# ==============================================================================

set.seed(42)
# createDataPartition preserves the distribution of the target class
train_idx <- createDataPartition(ads_data$Label, p = 0.7, list = FALSE)
train_data <- ads_data[train_idx, ]
test_data <- ads_data[-train_idx, ]

# ==============================================================================
# STEP 5: MODEL TRAINING
# ==============================================================================

print("Training Random Forest model... This may take a moment.")

# Formula: Label ~ . (Predict Label using all other variables)
rf_model <- randomForest(Label ~ .,
                         data = train_data,
                         ntree = 100,
                         importance = TRUE)

# Model summary
print(rf_model)

# ==============================================================================
# STEP 6: EVALUATION
# ==============================================================================

# 1. Predict on Test set
predictions <- predict(rf_model, newdata = test_data)

# 2. Create Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_data$Label,
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

# Variable Importance Plot (Top 15 variables)
png("feature_importance.png")  # <--- Thêm dòng này
varImpPlot(rf_model,
           main = "Variable Importance Plot",
           n.var = 15)
dev.off()                      # <--- Và dòng này