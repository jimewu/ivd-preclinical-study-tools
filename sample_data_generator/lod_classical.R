# ================= Step 0: 環境設定 =================
# 參考提供的範本，設定鏡像站並載入必要套件 [1]
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(writexl, dplyr) # 加入 dplyr 以便整理欄位

# ================= Step 1: 實驗參數設定 =================
# 1. 實驗架構參數
n_lots <- 2        # 試劑批號數量
n_days <- 3        # 每批號的天數
n_reps <- 5        # 每個濃度每天的重複測量次數

# 2. 濃度層級設定 (conc)
# 設定一組低濃度樣品的預期數值 (例如：0.05, 0.1, 0.2, 0.5, 1.0)
target_concentrations <- c(0.05, 0.1, 0.25, 0.5, 1.0)

# 3. 誤差與分佈設定
# 設定量測的標準差 (雜訊大小)，假設低濃度範圍內 SD 相對固定
noise_sd <- 0.05

# 資料分佈模式: "parametric" (常態分佈) 或 "non_parametric" (偏態分佈)
distribution_type <- "parametric"

# ================= Step 2: 定義生成函數 =================
generate_lod_data <- function(levels, n_lot, n_day, n_rep, error_sd, mode) {
  
  # 建立實驗架構 grid (批號 x 天數 x 濃度 x 重複次數)
  # 參考引用來源使用 expand.grid 建立基礎結構 [1]
  df <- expand.grid(
    lot = 1:n_lot,
    day = 1:n_day,
    conc = levels,       # 加入濃度欄位
    replicate = 1:n_rep
  )
  
  n <- nrow(df)
  set.seed(456) # 設定種子碼確保不同腳本間的差異性
  
  # 模擬量測值 y
  # 假設儀器回收率理想，平均值(Mean)接近設定濃度(conc)
  if (mode == "parametric") {
    # --- 參數化模式 (常態分佈雜訊) ---
    # y = 濃度 + 常態隨機誤差
    df$y <- rnorm(n, mean = df$conc, sd = error_sd)
    
  } else {
    # --- 無母數模式 (偏態分佈雜訊) ---
    # 使用 Gamma 分佈模擬，適用於無法呈現負值的檢測系統
    # 將 Mean=conc, SD=error_sd 轉換為 Gamma 參數
    # 注意：若濃度極低，此數學轉換需小心除以零的狀況
    df$y <- mapply(function(m, s) {
      if(m <= 0) return(0) # 避免濃度為0時錯誤
      shape <- (m / s)^2
      scale <- (s^2) / m
      rgamma(1, shape = shape, scale = scale)
    }, df$conc, error_sd)
  }
  
  # 數值修整 (保留小數點後 3 位)
  df$y <- round(df$y, 3)
  
  # 整理並回傳指定欄位: lot, day, conc, y [1]
  return(df[, c("lot", "day", "conc", "y")])
}

# ================= Step 3: 執行與匯出 =================
# 執行生成函數
lod_df <- generate_lod_data(
  levels = target_concentrations,
  n_lot = n_lots,
  n_day = n_days,
  n_rep = n_reps,
  error_sd = noise_sd,
  mode = distribution_type
)

# 預覽前幾筆資料，確保包含 conc 與 y 欄位
print(head(lod_df))

# 根據分佈模式命名檔案並匯出 Excel [1]
file_name <- paste0("lod_classical_sample_data_", distribution_type, ".xlsx")
write_xlsx(lod_df, path = file_name)

message(paste("檢測極限分析資料已生成並儲存為:", file_name))
