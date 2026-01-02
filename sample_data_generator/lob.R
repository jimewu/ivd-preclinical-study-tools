# ================= Step 0: 環境設定 =================
# 參考提供的範本，設定鏡像站並載入必要套件 [1]
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(writexl, dplyr) # 加入 dplyr 以便整理欄位

# ================= Step 1: 參數設定 =================
# 1. 資料分佈模式設定
# 選項: "parametric" (常態分佈) 或 "non_parametric" (偏態分佈，如 Gamma 分佈)
distribution_type <- "non_parametric" 

# 2. 實驗設計參數
n_lots <- 2       # 試劑批號數量
n_days <- 3       # 每批號的天數
n_reps <- 10       # 每天的重複測量次數 (Replicates)

# 3. 統計數值設定 (根據分佈模式調整)
# 空白樣品通常接近 0，設定預期平均值與標準差
target_mean <- 1.5
target_sd   <- 0.5

# ================= Step 2: 定義生成函數 =================
generate_blank_data <- function(dist_mode, mu, sigma) {
  
  # 建立實驗架構 grid (批號 x 天數 x 重複次數) [1]
  df <- expand.grid(day = 1:n_days, replicate = 1:n_reps, lot = 1:n_lots)
  
  # 設定隨機種子以確保可重現性
  set.seed(123)
  
  n <- nrow(df)
  
  if (dist_mode == "parametric") {
    # --- 參數化模式 (常態分佈) ---
    # 使用 rnorm 生成對稱分佈的資料
    # 注意：若 mean 接近 0 且 SD 較大，可能會出現負值
    df$y <- rnorm(n, mean = mu, sd = sigma)
    
  } else {
    # --- 無母數模式 (偏態分佈) ---
    # 使用 Gamma 分佈模擬右偏的空白訊號 (常見於實際儀器空白雜訊)
    # 將 mean/sd 轉換為 Gamma 分佈所需的 shape 與 scale 參數
    shape_param <- (mu / sigma)^2
    scale_param <- (sigma^2) / mu
    df$y <- rgamma(n, shape = shape_param, scale = scale_param)
  }
  
  # 數值修整 (例如保留小數點後 3 位)
  df$y <- round(df$y, 3)
  
  # 整理欄位順序 (符合 lot, day, y 的需求)
  # 雖然包含 replicate 欄位在結構中，但輸出時可依需求選取
  return(df[, c("lot", "day", "y")])
}

# ================= Step 3: 執行與匯出 =================
# 執行生成函數
final_df <- generate_blank_data(distribution_type, target_mean, target_sd)

# 預覽前幾筆資料
print(head(final_df))

# 根據分佈模式命名檔案並匯出 [1]
file_name <- paste0("lob_data_", distribution_type, ".xlsx")
write_xlsx(final_df, path = file_name)

message(paste("資料已生成並儲存為:", file_name))
