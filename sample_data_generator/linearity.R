# ================= Step 0: 環境設定 =================

# 1. 強制設定鏡像站為 Global CDN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件
# openxlsx 用於匯出多工作表 Excel, dplyr 用於資料整理 (若未安裝會自動安裝)
pacman::p_load(openxlsx, dplyr)

# ================= Step 1: 參數設定 =================

# --- 線性分析核心參數 ---
CONC_LVS <- 9 # 濃度水平數量
REPS <- 4 # 每個濃度水平的重複測量次數

# --- 資料生成參數 ---
# 定義測量範圍與變異性
y_ref_min <- 10.0 # 最低濃度的真值
y_ref_max <- 200.0 # 最高濃度的真值
cv_percent <- 0.03 # 測量變異係數 (控制測量值圍繞真值的變異程度)

# ================= Step 2: 定義生成函數 =================

generate_linearity_data <- function(conc_levels, reps, y_min, y_max, cv) {
    # 設定隨機種子以確保結果可重現
    set.seed(42)

    # --- 生成相對濃度 (rc) ---
    # 在 0~1 之間均勻分佈，不包含 0
    # 使用 seq() 產生均勻間隔的濃度水平
    rc_values <- seq(from = 1 / conc_levels, to = 1, length.out = conc_levels)

    # --- 生成真值 (y_ref) ---
    # 根據相對濃度線性映射到實際濃度範圍
    y_ref_values <- y_min + (y_max - y_min) * rc_values

    # --- 建立完整資料框架 ---
    # 每個濃度水平重複 reps 次
    df <- data.frame()

    for (i in 1:conc_levels) {
        # 當前濃度水平的相對濃度與真值
        current_rc <- rc_values[i]
        current_y_ref <- y_ref_values[i]

        # 計算標準差 (SD = Mean * CV)
        sigma <- current_y_ref * cv

        # 生成重複測量值 (圍繞真值產生，包含隨機變異)
        y_measurements <- rnorm(reps, mean = current_y_ref, sd = sigma)

        # 建立當前濃度水平的資料
        temp_df <- data.frame(
            rc = rep(current_rc, reps), # 相對濃度 (所有重複測量相同)
            y_ref = rep(current_y_ref, reps), # 真值 (所有重複測量相同)
            y = y_measurements # 測量值 (每次重複不同)
        )

        # 合併到主資料框
        df <- rbind(df, temp_df)
    }

    # --- 數值修整 ---
    # 將相對濃度四捨五入到 4 位小數
    df$rc <- round(df$rc, 4)
    # 將真值與測量值四捨五入到 2 位小數
    df$y_ref <- round(df$y_ref, 2)
    df$y <- round(df$y, 2)

    return(df)
}

# ================= Step 3: 執行資料生成 =================

# 呼叫生成函數產生線性分析資料
linearity_data <- generate_linearity_data(
    conc_levels = CONC_LVS,
    reps = REPS,
    y_min = y_ref_min,
    y_max = y_ref_max,
    cv = cv_percent
)

# 顯示前幾行預覽
cat("=== 產生的線性分析資料預覽 ===\n")
print(head(linearity_data, 12)) # 顯示前 3 個濃度水平的資料
cat("\n總共產生", nrow(linearity_data), "筆資料\n")
cat("濃度水平數:", CONC_LVS, "\n")
cat("每個濃度重複次數:", REPS, "\n\n")

# ================= Step 4: 匯出 Excel 檔案 =================

# 建立空白資料框 (僅包含欄位名稱，無數值資料)
empty_data <- data.frame(
    rc = numeric(0),
    y_ref = numeric(0),
    y = numeric(0)
)

# 設定輸出檔案名稱
filename <- "linearity_data.xlsx"

# 建立工作簿物件
wb <- createWorkbook()

# 新增第一個工作表: sample (包含完整隨機產生的資料)
addWorksheet(wb, "sample")
writeData(wb, sheet = "sample", x = linearity_data)

# 新增第二個工作表: data (僅包含欄位名稱，無數值)
addWorksheet(wb, "data")
writeData(wb, sheet = "data", x = empty_data)

# 儲存 Excel 檔案
saveWorkbook(wb, filename, overwrite = TRUE)

cat("=== Excel 檔案已成功輸出 ===\n")
cat("檔案名稱:", filename, "\n")
cat("工作表 1: sample (包含", nrow(linearity_data), "筆隨機資料)\n")
cat("工作表 2: data (僅包含欄位名稱)\n")
