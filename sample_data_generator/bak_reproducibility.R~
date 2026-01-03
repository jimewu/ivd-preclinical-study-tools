# ================= Step 0: 環境設定 =================

# 1. 強制設定鏡像站為 Global CDN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件
pacman::p_load(writexl)

# ================= Step 1: 參數設定 =================

# --- 修改處：在此指定 X 的名稱與層級 ---
x_name <- "operator"      # 您可以改為 "lot", "operator" 或其他名稱
x_levels <- 1:3       # 定義 X 有多少個 (例如: 1:2 代表有 2 個 site)

days <- 1:5          # Days
reps <- 1:5           # Replicates

# 定義多組資料 (濃度設定維持不變)
settings <- data.frame(
  conc_label  = c("Placebo", "Low_Dose", "High_Dose", "Positive_Ctrl"),
  target_mean = c(1.0,       5.5,        12.0,        15.5),
  target_sd   = c(0.2,       1.5,         2.0,         0.5)
)

# ================= Step 2: 定義生成函數 =================
generate_one_set <- function(label, mu, sigma) {
  
  # --- 修改處：建立網格時包含 x_levels，暫時命名為 temp_x ---
  # expand.grid 的順序決定了資料排列的快慢頻率
  df <- expand.grid(temp_x = x_levels, day = days, replicate = reps)

  # 使用 label 生成種子,確保每組雜訊不同但可重現
  set.seed(sum(utf8ToInt(label)))

  df$y <- rnorm(nrow(df), mean = mu, sd = sigma)
  df$y <- round(df$y, 2)
  df$conc <- label

  # --- 修改處：將 temp_x 改名為使用者設定的 x_name ---
  # 找出欄位名稱是 "temp_x" 的位置，並改為 x_name 的內容 (例如 "site")
  names(df)[names(df) == "temp_x"] <- x_name

  # 回傳資料，並將自定義的欄位排在前面
  # 使用 get() 或直接引用 x_name 字串來動態選擇欄位
  return(df[, c("conc", x_name, "day", "replicate", "y")])
}

# ================= Step 3: 執行與匯出 =================
data_list <- lapply(1:nrow(settings), function(i) {
  generate_one_set(settings$conc_label[i], settings$target_mean[i], settings$target_sd[i])
})

final_df <- do.call(rbind, data_list)

# 依據變數名稱動態命名檔案，方便識別
file_name <- paste0("precision_data_by_", x_name, ".xlsx")
write_xlsx(final_df, path = file_name)

message(paste("檔案已輸出為:", file_name))
