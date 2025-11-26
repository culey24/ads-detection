# ==============================================================================
# PART 1: PREPROCESSING
# ==============================================================================

# 1. Load Data
# Note: The raw file uses "?" for missing values.
# Split arguments to multiple lines to satisfy line_length_linter
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
  # Indentation fixed to 2 spaces
  med_val <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med_val
  x # Implicit return (fixed return_linter)
}

df$height <- impute_median(df$height)
df$width  <- impute_median(df$width)
df$aratio <- impute_median(df$aratio)

# Check for remaining NAs
cat("Number of remaining NA values:", sum(is.na(df[, 1:3])), "\n")

# 4. Data Export
write.csv(df, "clean_data.csv", row.names = FALSE)
cat("Successfully exported 'clean_data.csv'!\n")

# ==============================================================================
# PART 2: DATA ANALYSIS
# ==============================================================================

# --- Task 1: Visualization ---

# 1. Bar chart: Ad vs Non-ad Count
png("barchart_ad_count.png")
# Indentation fixed to 2 spaces
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
) # Hiding outliers for better visibility
dev.off()

cat("Saved plots: 'barchart_ad_count.png' and 'boxplot_width.png'\n")

# ==============================================================================
# SUPPLEMENT: DETAILED DESCRIPTIVE STATISTICS & CORRELATION
# ==============================================================================

# --- 1. Calculate Summary Statistics ---
# (Similar to Figure 4.1 in the sample report)

# Select 3 continuous variable columns
metric_cols <- df[, c("height", "width", "aratio")]

# Create statistics table: Mean, SD (Standard Deviation), Min, Median, Max
stats_table <- data.frame(
  Mean   = sapply(metric_cols, mean),
  SD     = sapply(metric_cols, sd),
  Min    = sapply(metric_cols, min),
  Median = sapply(metric_cols, median),
  Max    = sapply(metric_cols, max)
)

# Round to 2 decimal places for better presentation
stats_table <- round(stats_table, 2)

print("--- DESCRIPTIVE STATISTICS TABLE ---")
print(stats_table)
# Note: Copy this output to create a table in your report

# --- 2. Draw Correlation Matrix ---
# (Similar to Figure 4.4 - Using GGally library)

# Install library if not already installed (Uncomment the line below if needed)
# install.packages("GGally")
library(GGally)

print("Drawing Correlation Matrix... (This might take a while)")

png("correlation_matrix.png", width = 1000, height = 800)

# Draw ggpairs for 3 dimension variables + classified by Label
# columns = 1:3 means only taking height, width, aratio
ggpairs(df, 
        columns = 1:3, 
        aes(color = Label, alpha = 0.5), # Color by Ad/Non-ad
        title = "Correlation Matrix of Geometric Features",
        upper = list(continuous = wrap("cor", size = 5))) # Adjust font size of r coefficient

dev.off()
print("Image saved: correlation_matrix.png")

# --- Task 2: Hypothesis Testing ---

# Independent 2-sample T-test
# Question: Is the mean width of 'ad' different from 'nonad'?
t_test_result <- t.test(width ~ Label, data = df)

# Print Results
print("T-TEST RESULTS:")
print(t_test_result)

# Print p-value interpretation
cat("P-value is:", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  # Split long strings to satisfy line_length_linter
  cat("Conclusion: Reject H0.",
      "There is a statistically significant difference",
      "in width between Ad and Non-ad images.\n")
} else {
  cat("Conclusion: Fail to reject H0.",
      "Not enough evidence to suggest a difference.\n")
}