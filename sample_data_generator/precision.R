# ================= Step 0: 環境設定 =================

# 1. 強制設定鏡像站為 Global CDN (避免跳出詢問視窗)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件 (pacman 會自動判斷安裝)
pacman::p_load(writexl)

# ================= Step 1: 參數設定 =================
days <- 1:20
runs <- 1:2
reps <- 1:3

# 定義多組資料
settings <- data.frame(
  conc_label  = c("Placebo", "Low_Dose", "High_Dose", "Positive_Ctrl"),
  target_mean = c(1.0,       5.5,        12.0,        15.5),
  target_sd   = c(0.2,       1.5,         2.0,         0.5)
)

# ================= Step 2: 定義生成函數 =================
generate_one_set <- function(label, mu, sigma) {
  df <- expand.grid(day = days, run = runs, replicate = reps)

  # 使用 label 生成種子，確保每組雜訊不同但可重現
  set.seed(sum(utf8ToInt(label)))

  df$y <- rnorm(nrow(df), mean = mu, sd = sigma)
  df$y <- round(df$y, 2)
  df$conc <- label

  return(df[, c("conc", "day", "run", "replicate", "y")])
}

# ================= Step 3: 執行與匯出 =================
data_list <- lapply(1:nrow(settings), function(i) {
  generate_one_set(settings$conc_label[i], settings$target_mean[i], settings$target_sd[i])
})

final_df <- do.call(rbind, data_list)

write_xlsx(final_df, path = "precision_sample_data.xlsx")
