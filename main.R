# ==============================================================================
# COMBINED SCRIPT: DATA ANALYSIS & RANDOM FOREST MODELING
# ==============================================================================

# ------------------------------------------------------------------------------
# GLOBAL SETUP: LOAD LIBRARIES
# ------------------------------------------------------------------------------
# Kiểm tra và load tất cả thư viện cần thiết ngay từ đầu
required_packages <- c("GGally", "tidyverse", "randomForest", "caret")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
cat("Libraries loaded successfully.\n")

# ==============================================================================
# PART 1: PREPROCESSING & EXPLORATORY DATA ANALYSIS (From firsttry.R)
# ==============================================================================

cat("\n>>> STARTING PART 1: PREPROCESSING & EDA <<<\n")

# 1. Load Data
# Note: The raw file uses "?" for missing values.
raw_data <- read.csv("add.csv", header = TRUE,
                     na.strings = c("?", " ?"), stringsAsFactors = FALSE)

# Remove the first column (index column)
df <- raw_data[, -1]

# 2. Rename Columns and Type Casting
# First 3 columns: height, width, aratio. Last column: Label.
colnames(df)[1] <- "height"
colnames(df)[2] <- "width"
colnames(df)[3] <- "aratio"
colnames(df)[ncol(df)] <- "Label"

# Type casting: first 3 to numeric, Label to factor
df$height <- as.numeric(df$height)
df$width  <- as.numeric(df$width)
df$aratio <- as.numeric(df$aratio)
df$Label  <- as.factor(df$Label)

# 3. Imputation (Fill NA with Median)
impute_median <- function(x) {
  med_val <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med_val
  x 
}

df$height <- impute_median(df$height)
df$width  <- impute_median(df$width)
df$aratio <- impute_median(df$aratio)

# Check for remaining NAs
cat("Number of remaining NA values:", sum(is.na(df[, 1:3])), "\n")

# 4. Data Export
# Xuất file ra thư mục hiện tại để Part 2 có thể đọc được
write.csv(df, "clean_data.csv", row.names = FALSE)
cat("Successfully exported 'clean_data.csv'!\n")


# --- Task 1: Visualization ---

# 1. Bar chart: Ad vs Non-ad Count
png("barchart_ad_count.png")
barplot(table(df$Label),
        main = "Count of Ad vs Non-ad Images",
        col = c("tomato", "steelblue"),
        xlab = "Label",
        ylab = "Count"
)
dev.off()

# 2. Boxplot: Width Distribution
png("boxplot_width.png")
boxplot(width ~ Label,
        data = df,
        main = "Width Distribution by Image Type",
        xlab = "Image Type (Label)",
        ylab = "Width",
        col = c("tomato", "steelblue"),
        outline = FALSE
)
dev.off()

cat("Saved plots: 'barchart_ad_count.png' and 'boxplot_width.png'\n")

# --- Detailed Statistics ---

# Select 3 continuous variable columns
metric_cols <- df[, c("height", "width", "aratio")]

# Create statistics table
stats_table <- data.frame(
  Mean   = sapply(metric_cols, mean),
  SD     = sapply(metric_cols, sd),
  Min    = sapply(metric_cols, min),
  Median = sapply(metric_cols, median),
  Max    = sapply(metric_cols, max)
)

stats_table <- round(stats_table, 2)

print("--- DESCRIPTIVE STATISTICS TABLE ---")
print(stats_table)

# --- Correlation Matrix ---
print("Drawing Correlation Matrix... (This might take a while)")

png("correlation_matrix.png", width = 1000, height = 800)
p <- ggpairs(df, 
             columns = 1:3, 
             aes(color = Label, alpha = 0.5), 
             title = "Correlation Matrix of Geometric Features",
             upper = list(continuous = wrap("cor", size = 5)))
print(p) 
dev.off()
print("Image saved: correlation_matrix.png")

# --- Task 2: Hypothesis Testing ---

# Independent 2-sample T-test
t_test_result <- t.test(width ~ Label, data = df)

print("T-TEST RESULTS:")
print(t_test_result)
cat("P-value is:", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject H0. Significant difference in width.\n")
} else {
  cat("Conclusion: Fail to reject H0.\n")
}

cat("\n================================================================\n")
cat("PART 1 COMPLETE. Clean data saved to 'clean_data.csv'.\n")
cat("================================================================\n")


# ==============================================================================
# USER INTERACTION: CONTINUE OR STOP?
# ==============================================================================

# Yêu cầu người dùng nhập liệu để tiếp tục
cat("\nDo you want to continue to Random Forest?\n")
user_input <- readline(prompt = "Y for continue, N for stop: ")

if (toupper(trimws(user_input)) != "Y") {
  cat("Terminated, goodbye!\n")
  quit(save = "no") # Dừng script tại đây nếu không chọn Y
}

cat("\n>>> USER SELECTED CONTINUE. STARTING PART 2: MACHINE LEARNING <<<\n")


# ==============================================================================
# PART 2: RANDOM FOREST MODELING (From random_forest.r)
# ==============================================================================

# 1. Read Data File
tryCatch({
  # Đọc file clean_data.csv vừa được tạo ở Part 1 (nằm cùng thư mục)
  ads_data <- read.csv("clean_data.csv", na.strings = c("?", "NA"))
  print("Data re-loaded successfully for modeling!")
}, error = function(e) {
  print("Error: Cannot find clean_data.csv. Did Part 1 run correctly?")
  stop(e)
})

# 2. Preprocessing for Model
ads_data <- na.omit(ads_data)

if ("Label" %in% colnames(ads_data)) {
  ads_data$Label <- as.factor(ads_data$Label)
} else {
  stop("Error: Column 'Label' not found in the dataset.")
}

# Clean column names
colnames(ads_data) <- make.names(colnames(ads_data))

# 3. Split Data
set.seed(42)
train_idx <- createDataPartition(ads_data$Label, p = 0.7, list = FALSE)
train_data <- ads_data[train_idx, ]
test_data <- ads_data[-train_idx, ]

# 4. Model Training
print("Training Random Forest model... This may take a moment.")

rf_model <- randomForest(Label ~ .,
                         data = train_data,
                         ntree = 100,
                         importance = TRUE)

print(rf_model)

# 5. Evaluation
predictions <- predict(rf_model, newdata = test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Label,
                               mode = "everything")

print("=== TEST SET EVALUATION RESULTS ===")
print(conf_matrix$table)

accuracy  <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall    <- conf_matrix$byClass["Recall"]

cat("\n--- Key Metrics ---\n")
cat(sprintf("Overall Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision:        %.4f\n", precision))
cat(sprintf("Recall:           %.4f\n", recall))

# 6. Visualization (Feature Importance)
png("feature_importance.png")
varImpPlot(rf_model,
           main = "Variable Importance Plot",
           n.var = 15)
dev.off()
cat("Image saved: feature_importance.png\n")

cat("\n>>> ALL TASKS COMPLETED SUCCESSFULLY! <<<\n")