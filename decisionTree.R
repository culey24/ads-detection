# ==============================================================================
# DEMO: DECISION TREE FOR AD DETECTION
# ==============================================================================

# 1. Cài đặt và Load thư viện
# Nếu chưa cài thì chạy dòng dưới:
# install.packages(c("rpart", "rpart.plot", "caret", "tidyverse"))

library(tidyverse)
library(caret)
library(rpart)       # Thư viện chính để xây cây
library(rpart.plot)  # Thư viện để vẽ cây đẹp

# 2. Load dữ liệu sạch
# Đảm bảo bạn đã có file clean_data.csv từ bước Data Cleaning
tryCatch({
  df <- read.csv("clean_data.csv", stringsAsFactors = TRUE)
  print("Đã load dữ liệu thành công!")
}, error = function(e) {
  stop("Lỗi: Không tìm thấy file 'clean_data.csv'. Hãy chạy code xử lý dữ liệu trước.")
})

# Đảm bảo cột Label là Factor (để model hiểu là bài toán phân loại)
df$Label <- as.factor(df$Label)

# 3. Chia tập dữ liệu (70% Train - 30% Test)
set.seed(123)
train_idx <- createDataPartition(df$Label, p = 0.7, list = FALSE)
train_data <- df[train_idx, ]
test_data  <- df[-train_idx, ]

# 4. Huấn luyện mô hình Cây quyết định
print("Đang xây cây... (Quá trình này rất nhanh)")

# Công thức: Label ~ . nghĩa là "Dự đoán Label dựa trên tất cả cột còn lại"
# method = "class": Bắt buộc cho bài toán phân loại
model_tree <- rpart(Label ~ ., 
                    data = train_data, 
                    method = "class",
                    control = rpart.control(cp = 0.001)) # cp thấp để cây mọc sâu hơn chút

# 5. Vẽ Cây quyết định (Phần quan trọng để đưa vào báo cáo)
# Lưu hình ảnh ra file
png("decision_tree_visualization.png", width = 1200, height = 800)

rpart.plot(model_tree, 
           type = 4,           # Kiểu vẽ (4 là đẹp nhất cho báo cáo)
           extra = 104,        # Hiển thị xác suất của từng lớp
           under = TRUE,       # Hiển thị nhãn dưới hộp
           faclen = 0,         # Không viết tắt tên biến
           digits = 3,         # Số chữ số thập phân
           main = "Mô hình Cây quyết định nhận diện Quảng cáo",
           box.palette = "BuOr") # Màu sắc (Xanh - Cam)

dev.off()
print("Đã lưu hình ảnh cây tại: decision_tree_visualization.png")

# 6. Đánh giá mô hình
# Dự đoán trên tập Test
pred_tree <- predict(model_tree, test_data, type = "class")

# Xem kết quả
conf_matrix <- confusionMatrix(pred_tree, test_data$Label)
print(conf_matrix)

# In ra các biến quan trọng nhất theo Cây quyết định
print("--- CÁC BIẾN QUAN TRỌNG NHẤT (VAR IMPORTANCE) ---")
print(model_tree$variable.importance[1:5])