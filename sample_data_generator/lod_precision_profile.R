# ================= Step 0: 環境設定 =================

# 1. 強制設定鏡像站為 Global CDN (避免跳出詢問視窗)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件 (pacman 會自動判斷安裝)
# 沿用原腳本使用的 writexl 進行 Excel 輸出 [1]
pacman::p_load(writexl, dplyr)

# ================= Step 1: 參數設定 =================

# 設定測試筆數 (每組樣品在每個批次測量的次數)
n_test_count <- 60

# 定義器材批次 (預設 2 個批次)
lot_list <- c("Lot_A", "Lot_B")

# 定義樣品參數 (預設 5 個濃度的樣品)
# target_mean: 理論平均值 (濃度由低到高)
# target_sd: 理論標準差 (模擬低濃度時的變異)
sample_settings <- data.frame(
  sample_id   = c("Sample_01", "Sample_02", "Sample_03", "Sample_04", "Sample_05"),
  target_mean = c(0.00,        0.50,        1.00,        2.50,        5.00),
  target_sd   = c(0.10,        0.12,        0.15,        0.18,        0.25)
)

# ================= Step 2: 定義生成函數 =================

# 建立單一情境模擬函數
simulate_precision_profile <- function(lot, sample_info, n) {
  
  # 組合唯一的識別字串，用於種子生成
  label <- paste0(lot, "_", sample_info$sample_id)
  
  # 使用 label 生成種子，確保每組雜訊不同但可重現 [1]
  set.seed(sum(utf8ToInt(label)))
  
  # 生成模擬測量數據
  # 這裡模擬 n 次測試的原始數據
  raw_values <- rnorm(n, mean = sample_info$target_mean, sd = sample_info$target_sd)
  
  # 若模擬結果出現負值 (且不允許負值時)，可在此處理，此處保留原始分佈
  # raw_values[raw_values < 0] <- 0 
  
  # 計算統計量
  obs_mean <- mean(raw_values)
  obs_sd   <- sd(raw_values)
  
  # 建立回傳的資料列
  result_row <- data.frame(
    dev_lot = lot,
    sample  = sample_info$sample_id,
    mean    = round(obs_mean, 4), # 樣品測量平均
    sd_wl   = round(obs_sd, 4),   # 樣品的 Within-Lab Precision (以該批次之 SD 代表)
    n_test  = n
  )
  
  return(result_row)
}

# ================= Step 3: 執行與匯出 =================

# 使用巢狀迴圈遍歷所有批次與樣品組合
# 參考參考檔中的 lapply 結構進行數據生成與合併 [1]
final_list <- list()
counter <- 1

for (lot in lot_list) {
  for (i in 1:nrow(sample_settings)) {
    # 呼叫函數生成單列資料
    res <- simulate_precision_profile(lot, sample_settings[i, ], n_test_count)
    final_list[[counter]] <- res
    counter <- counter + 1
  }
}

# 合併所有結果
final_df <- do.call(rbind, final_list)

# 顯示前幾筆資料確認
print(head(final_df))

# 匯出至 Excel
write_xlsx(final_df, path = "lod_precision_profile_data.xlsx")
