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