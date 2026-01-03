source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_loq.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    test_name = "Subject Device",
    allowable_perc_te = 21.6,
    te_method = "Westgard model"
)

# 設定檔案路徑

file_path <- "loq_sample_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
# if (!require("patchwork")) install.packages("patchwork")
# if (!require("purrr")) install.packages("purrr")

# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        lot = factor(lot), # 將 conc 欄位轉為 sample 欄位
        day = factor(day),
        conc = as.numeric(conc),
        y = as.numeric(y)
    ) %>%
    group_by(lot, day, conc) %>%
    mutate(
        replicate = row_number()
    ) %>%
    ungroup() %>%
    arrange(
        conc, lot, day
    ) %>%
    mutate(
        replicate = as.factor(replicate)
    ) %>%
    tidyr::nest(
        raw = colnames(.)
    ) %>%
    mutate_raw2ft(unit = params$unit) %>%
    mutate_raw2plot(
        test_name = params$test_name,
        unit = params$unit
    )

# * Stage 2: Tidy
stage2_tidy <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "lot"]
    )

# * Stage 3: Analysis
stage3_analysis <- stage2_tidy %>%
    mutate_raw2total_error(
        loq_type = "lloq",
        allowable_perc_te = as.numeric(
            params$allowable_perc_te
        ),
        te_method = params$te_method
    ) %>%
    mutate_total_error2ft(
        unit = params$unit,
        te_method = params$te_method
    )
