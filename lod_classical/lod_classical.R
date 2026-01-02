source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_lod_classical.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    test_name = "Subject Device",
    lob = "0"
)

# 設定檔案路徑
file_path <- "lod_classical_sample_data_parametric.xlsx" # 請修改為您的檔案名稱
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
        conc = as.numeric(conc),
        lot = factor(lot),
        day = factor(day),
        y = as.numeric(y)
    ) %>%
    arrange(
        conc,
        lot,
        day
    ) %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate(
        summary = purrr::map(
            raw,
            function(raw) {
                nday <- raw$day %>%
                    factor() %>%
                    levels() %>%
                    length()

                nlot <- raw$lot %>%
                    factor() %>%
                    levels() %>%
                    length()

                nconc <- raw$conc %>%
                    factor() %>%
                    levels() %>%
                    length()

                ntest <- nrow(raw)

                replicate <- ntest / (nday * nlot * nconc)

                data.frame(
                    nlot,
                    nday,
                    nconc,
                    ntest,
                    replicate
                ) %>%
                    return()
            }
        )
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
    mutate(
        ## ntest: 不分批次總測試數(相當於CLSI中的L)
        ntest = nrow(.)
    ) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "lot"]
    ) %>%
    mutate_raw2lot_summary() %>%
    select(-raw) %>%
    mutate_lot_summary2lot_sd_lod(lob = params$lob) %>%
    tidyr::unnest(lot_sd_lod) %>%
    mutate(
        final_lod = max(lot_lod)
    )

stage2A_analyze_summary <- stage2_analyze %>%
    tidyr::unnest(lot_summary) %>%
    select(-ntest, -nconc_lot) %>%
    tidyr::nest(
        summary = everything()
    ) %>%
    mutate_lod_summary2ft(unit = params$unit)
