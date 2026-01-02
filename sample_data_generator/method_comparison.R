# ================= Step 0: 環境設定 =================
# 1. 強制設定鏡像站
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件
pacman::p_load(writexl)

# ================= Step 1: 參數設定 =================

# --- 設定 1: 樣本數量 (預設 100) ---
n_samples <- 100

# --- 設定 2: y_ref 的範圍 ---
ref_min <- 10   # 參考方法最小值
ref_max <- 200  # 參考方法最大值

# --- 設定 3: 誤差模式設定 (請修改這裡) ---
# 選項: "constant" (固定 SD) 或 "proportional" (固定 CV%, SD 隨數值變大)
error_mode <- "proportional"

# --- 設定 4: 誤差大小係數 ---
# 若 error_mode = "constant"，此數值代表標準差 (例如 2.5 表示 SD=2.5)
# 若 error_mode = "proportional"，此數值代表比例 (例如 0.05 表示 CV=5%)
noise_factor <- 0.05

# --- 設定 5: 線性關係參數 ---
set.seed(42)            # 固定種子
true_slope <- 1.0       # 斜率
true_intercept <- 0.0   # 截距

# ================= Step 2: 產生資料 =================

# 1. 產生 y_ref
y_ref <- runif(n_samples, min = ref_min, max = ref_max)

# 2. 計算每個點的標準差 (SD)
# 根據 error_mode 決定每個樣品的雜訊大小
if (error_mode == "constant") {
  # 模式 A: 固定 SD (所有濃度誤差範圍一致)
  current_sd <- rep(noise_factor, n_samples)
  cat("模式: 固定 SD ( SD =", noise_factor, ")\n")

} else if (error_mode == "proportional") {
  # 模式 B: 比例 SD (濃度越高，誤差越大，模擬 CV 固定)
  current_sd <- abs(y_ref) * noise_factor
  cat("模式: 比例 SD ( CV =", noise_factor * 100, "% )\n")

} else {
  stop("error_mode 設定錯誤，請填寫 'constant' 或 'proportional'")
}

# 3. 產生隨機誤差與 y_test
# rnorm 支援向量化的 sd 輸入
random_error <- rnorm(n_samples, mean = 0, sd = current_sd)
y_test <- (y_ref * true_slope) + true_intercept + random_error

# 4. 整理 Dataframe
mc_data <- data.frame(
  y_ref  = round(y_ref, 2),
  y_test = round(y_test, 2)
)

# ================= Step 3: 執行與匯出 =================
output_filename <- "method_comparison_data.xlsx"
write_xlsx(mc_data, path = output_filename)

# 顯示簡單統計
cat("----------------------------------\n")
cat("檔案已產生:", output_filename, "\n")
cat("樣本數:", nrow(mc_data), "\n")
cat("範圍:", min(mc_data$y_ref), "-", max(mc_data$y_ref), "\n")
