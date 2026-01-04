source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_lod_precision_profile.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    test_name = "Subject Device",
    lob = "0.1"
)

# 設定檔案路徑
file_path <- "template_lod_precision_profile.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("purrr")) install.packages("purrr")

# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        dev_lot = factor(dev_lot),
        sample = factor(sample),
        mean = as.numeric(mean),
        sd_wl = as.numeric(sd_wl),
        n_test = as.numeric(n_test)
    ) %>%
    arrange(
        dev_lot,
        sample
    ) %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate_raw2ft(
        unit = params$unit,
        test_name = params$test_name
    ) %>%
    mutate_raw2plot(
        unit = params$unit,
        test_name = params$test_name
    )

# * Stage 2: Analyze
stage2_analyze <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "dev_lot"]
    ) %>%
    mutate_raw2lot_summary() %>%
    tidyr::unnest(lot_summary) %>%
    mutate_raw2fit() %>%
    mutate_fit2lod(lob = as.numeric(params$lob))


# * Stage 3: Share
stage3_share <- stage2_analyze %>%
    select(
        dev_lot,
        lot_k,
        lot_n_tot,
        lot_cp
    ) %>%
    tidyr::nest(
        analyze = everything()
    ) %>%
    mutate_analyze2ft()


stage3_share_fit1 <- stage2_analyze %>%
    select(
        dev_lot,
        fit1_summary_tidy,
        lod1
    ) %>%
    tidyr::unnest(fit1_summary_tidy) %>%
    tidyr::nest(
        fit_summary_tidy = everything()
    ) %>%
    mutate_fit_summary_tidy2ft()

stage3_share_fit2 <- stage2_analyze %>%
    select(
        dev_lot,
        fit2_summary_tidy,
        lod2
    ) %>%
    tidyr::unnest(fit2_summary_tidy) %>%
    tidyr::nest(
        fit_summary_tidy = everything()
    ) %>%
    mutate_fit_summary_tidy2ft()

stage3_share_fit3 <- stage2_analyze %>%
    select(
        dev_lot,
        fit3_summary_tidy,
        lod2
    ) %>%
    tidyr::unnest(fit3_summary_tidy) %>%
    tidyr::nest(
        fit_summary_tidy = everything()
    ) %>%
    mutate_fit_summary_tidy2ft()
