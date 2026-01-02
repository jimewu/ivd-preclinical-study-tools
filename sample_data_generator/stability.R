# ================= Step 0: 環境設定 (參照參考檔案結構 [1]) =================

# 1. 強制設定鏡像站為 Global CDN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件
# writexl 用於匯出 Excel, dplyr 用於資料整理 (若未安裝會自動安裝)
pacman::p_load(writexl, dplyr)

# ================= Step 1: 參數設定 =================

# --- 試驗設計核心開關 ---
# TRUE: 產生 anchor 數值 (Reference-Anchored Design)
# FALSE: y_anchor 欄位保持全空 (NA)
use_anchor_design <- TRUE

# --- 時間點與重複次數 ---
# 模擬檢測天數 (例如: 0, 30, 60... 天)
time_points <- c(0, 30, 60, 90, 180, 270, 360)
replicates <- 1:7 # 每個時間點、每個批號的重複測量次數

# --- 器材批號 (Device Lots) ---
dev_lot_list <- c("DevLot_A", "DevLot_B", "DevLot_C")

# --- 樣品資訊設定 ---
# 定義不同樣品及其特性 (模擬隨時間衰退的趨勢)
# 修改說明：已移除 sample_lot 欄位
sample_config <- data.frame(
  sample_name = c("Human_Pool", "QC", "Calibrator_L1"),
  initial_mean = c(25.0,            150.0,             50.0),    # T=0 的初始濃度
  cv_percent   = c(0.04,            0.02,              0.025),   # 測量變異係數 (雜訊大小)
  drift_per_month = c(-0.15,        -0.5,              0.0)      # 每月衰退量 (負值代表濃度下降)
)

# ================= Step 2: 定義生成函數 =================

generate_stability_data <- function(s_info, dev_lot, use_anchor) {

  # 展開基本網格: 包含時間點與重複次數
  df <- expand.grid(day = time_points, replicate = replicates)

  # 為了結果可重現,使用樣品名稱與批號生成隨機種子 (邏輯參考 [1])
  seed_val <- sum(utf8ToInt(paste0(s_info$sample_name, dev_lot)))
  set.seed(seed_val)

  # --- 計算 y (On-test 樣品: 隨時間變化) ---
  # 模擬邏輯: 初始值 + (每月衰退量 * 經過月數) + 隨機誤差

  months_elapsed <- df$day / 30
  current_mean <- s_info$initial_mean + (s_info$drift_per_month * months_elapsed)

  # 計算標準差 (SD = Mean * CV)
  sigma <- s_info$initial_mean * s_info$cv_percent

  df$y <- rnorm(nrow(df), mean = current_mean, sd = sigma)

  # --- 計算 y_anchor (Anchor 樣品: 假設高度穩定) ---
  if (use_anchor) {
    # Anchor 儲存於穩定條件下,故 Mean 不隨時間衰退 (保持 initial_mean)
    # 且通常期望 Anchor 的變異比一般測量更小 (此處設為一般 SD 的 0.5 倍以表現"特別穩")
    anchor_sigma <- sigma * 0.5
    df$y_anchor <- rnorm(nrow(df), mean = s_info$initial_mean, sd = anchor_sigma)
  } else {
    df$y_anchor <- NA
  }

  # --- 數值修整 ---
  df$y <- round(df$y, 2)
  if(use_anchor) df$y_anchor <- round(df$y_anchor, 2)

  # --- 填入識別欄位 ---
  df$dev_lot <- dev_lot
  df$sample <- s_info$sample_name
  # 修改說明：已移除 df$sample_lot 的指派動作

  # 整理欄位順序 (符合需求: dev_lot, sample, day, y, y_anchor)
  # 修改說明：return 列表中已移除 sample_lot
  return(df[, c("dev_lot", "sample", "day", "y", "y_anchor")])
}

# ================= Step 3: 執行與匯出 (參照參考檔案結構 [1]) =================

# 雙重迴圈: 遍歷所有樣品設定 + 遍歷所有器材批號
final_data_list <- list()

cnt <- 1
for (i in 1:nrow(sample_config)) {
  for (d_lot in dev_lot_list) {
    # 呼叫生成函數
    generated_data <- generate_stability_data(sample_config[i, ], d_lot, use_anchor_design)
    final_data_list[[cnt]] <- generated_data
    cnt <- cnt + 1
  }
}

# 合併所有資料框
final_df <- do.call(rbind, final_data_list)

# 依序排列方便閱讀
final_df <- final_df %>%
  arrange(sample, dev_lot, day)

# 匯出 Excel
filename <- paste0("stability_sample_data_", ifelse(use_anchor_design, "anchored", "non_anchored"), ".xlsx")
write_xlsx(final_df, path = filename)

# 顯示前幾行預覽
print(head(final_df))
cat("檔案已輸出至:", filename, "\n")
