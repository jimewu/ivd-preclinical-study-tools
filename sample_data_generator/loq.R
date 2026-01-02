# ================= Step 0: 環境設定 =================
# 參考來源設定鏡像站並載入 pacman 與 dplyr 等必要套件 [1]
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(writexl, dplyr) 

# ================= Step 1: 實驗參數設定 =================
# 1. 實驗架構參數
# 通常定量分析會包含多個批號與多天的測試以評估長期精密度
n_lots <- 2        # 試劑批號數量
n_days <- 3        # 每個批號測試的天數
n_reps <- 5        # 每個濃度每天的重複測量次數 (Replicates)

# 2. 濃度層級設定 (conc)
# 設定一組低濃度範圍的樣品，用於評估定量能力的極限
# 這通常包含預期精密目標附近的濃度點
target_concentrations <- c(0.05, 0.10, 0.20, 0.40, 0.80, 1.60)

# 3. 誤差設定
# 設定量測的標準差 (SD)，用來模擬儀器的雜訊
# 在低濃度下，通常假設會有一個固定的背景雜訊 SD
noise_sd <- 0.04 

# 資料分佈模式: "parametric" (常態分佈) 或 "non_parametric" (偏態分佈)
distribution_type <- "parametric"

# ================= Step 2: 定義生成函數 =================
generate_loq_data <- function(levels, n_lot, n_day, n_rep, error_sd, mode) {
  
  # 建立實驗架構 grid (批號 x 天數 x 濃度 x 重複次數)
  # 使用與參考來源相同的 expand.grid 結構來建立基礎資料表 [1]
  df <- expand.grid(
    lot = 1:n_lot,
    day = 1:n_day,
    conc = levels,       # 樣品配置濃度 (名目濃度)
    replicate = 1:n_rep
  )
  
  n <- nrow(df)
  set.seed(789) # 設定不同的種子碼以區別於 LoD 資料
  
  # 模擬量測值 y (實際測量值)
  if (mode == "parametric") {
    # --- 參數化模式 (常態分佈) ---
    # 假設測量值 = 配置濃度 + 常態隨機誤差
    df$y <- rnorm(n, mean = df$conc, sd = error_sd)
    
  } else {
    # --- 無母數模式 (偏態分佈) ---
    # 適用於無法為負值的檢測系統 (模擬 Gamma 分佈)
    df$y <- mapply(function(m, s) {
      if(m <= 0) return(0)
      shape <- (m / s)^2
      scale <- (s^2) / m
      rgamma(1, shape = shape, scale = scale)
    }, df$conc, error_sd)
  }
  
  # 數值修整 (保留小數點後 3 位，模擬儀器輸出)
  df$y <- round(df$y, 3)
  
  # 整理欄位順序: 批號, 天數, 配置濃度, 測量值
  # 參考來源僅回傳分析所需的特定欄位 [1]
  return(df[, c("lot", "day", "conc", "y")])
}

# ================= Step 3: 執行與匯出 =================
# 執行資料生成函數
loq_df <- generate_loq_data(
  levels = target_concentrations,
  n_lot = n_lots,
  n_day = n_days,
  n_rep = n_reps,
  error_sd = noise_sd,
  mode = distribution_type
)

# 檢查資料結構
print(head(loq_df))

# 定義檔案名稱並匯出為 Excel
# 依照參考來源的命名邏輯進行儲存 [1]
file_name <- paste0("loq_analysis_data_", distribution_type, ".xlsx")
write_xlsx(loq_df, path = file_name)

message(paste("定量極限分析資料已生成並儲存為:", file_name))
