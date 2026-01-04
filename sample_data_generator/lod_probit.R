# ================= Step 0: 環境設定 =================
# 參考提供的範本，設定環境與載入套件 [1]

# 1. 強制設定鏡像站為 Global CDN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件 (若尚未安裝)
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件
pacman::p_load(writexl, stats)

# ================= Step 1: 參數設定 =================
# 定義實驗設計參數
lot_list <- c("Lot_A", "Lot_B", "Lot_C") # dev_lot: 器材批次
days     <- 1:3                          # day: 測試天數 (連續5天)
n_runs   <- 10                           # test_total: 每個濃度點的總測試數

# 設定濃度梯度 (由陰性至高濃度)
# conc: 樣品配置濃度
concentrations <- c(0, 0.1, 0.25, 0.5, 1.0, 2.0, 4.0)

# ================= Step 2: 定義生成函數 =================
generate_probit_set <- function(lot_id) {
  
  # 建立基礎網格：組合天數與濃度
  df <- expand.grid(day = days, conc = concentrations)
  df$dev_lot <- lot_id
  df$test_total <- n_runs
  
  # 設定隨機種子，確保不同 Lot 的表現有些微差異但可重現 [1]
  set.seed(sum(utf8ToInt(lot_id)))
  
  # --- 模擬檢測機率 (Probit 模型邏輯) ---
  # 設定該批次的靈敏度參數 (隨機微調斜率與截距)
  # 這模擬了批次間的製造變異 (Inter-lot variability)
  true_lod_50 <- 0.5 + runif(1, -0.1, 0.1) # 模擬 50% 檢出率的濃度點
  slope       <- 2.0 + runif(1, -0.2, 0.2) # 模擬反應曲線的陡峭度
  
  # 計算理論陽性機率 (Probability of Hit)
  # 使用羅吉斯函數 (Logistic function) 模擬濃度反應曲線
  # 當濃度為 0 時，設定極低機率 (模擬偽陽性極少)
  logits <- slope * (df$conc - true_lod_50)
  probs  <- 1 / (1 + exp(-logits))
  
  # 強制修正濃度 0 的機率 (背景雜訊極低)
  probs[df$conc == 0] <- 0.001 
  
  # --- 生成測試結果 ---
  # 使用二項式分佈 (Binomial Distribution) 生成陽性次數
  # rbinom(n, size=總測試數, prob=陽性機率)
  df$test_hit <- rbinom(nrow(df), size = df$test_total, prob = probs)
  
  # 整理欄位順序以符合需求
  return(df[, c("dev_lot", "day", "conc", "test_hit", "test_total")])
}

# ================= Step 3: 執行與匯出 =================
# 對每個 Lot 執行生成函數 [1]
data_list <- lapply(lot_list, function(lot) {
  generate_probit_set(lot)
})

# 合併所有資料
final_df <- do.call(rbind, data_list)

# 預覽部分資料
print(head(final_df))

# 匯出至 Excel
write_xlsx(final_df, path = "lod_probit_sample_data.xlsx")

cat("資料生成完畢，已存檔為 'lod_probit_sample_data.xlsx'\n")
