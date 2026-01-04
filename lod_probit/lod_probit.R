source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_lod_probit.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    unit = "mg/dL",
    test_name = "Subject Device",
    beta = 0.05
)

# 設定檔案路徑
file_path <- "lod_probit_sample_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("patchwork")) install.packages("patchwork")

# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        dev_lot = factor(dev_lot),
        day = factor(day),
        conc = as.numeric(conc),
        test_hit = as.numeric(test_hit),
        test_total = as.numeric(test_total)
    ) %>%
    arrange(
        dev_lot,
        conc,
        day
    ) %>%
    relocate(
        conc,
        .before = "day"
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

                nlot <- raw$dev_lot %>%
                    factor() %>%
                    levels() %>%
                    length()

                nconc <- raw$conc %>%
                    factor() %>%
                    levels() %>%
                    length()

                data.frame(
                    nlot,
                    nday,
                    nconc
                ) %>%
                    return()
            }
        )
    ) %>%
    mutate(
        raw = purrr::map(
            raw,
            function(df_raw) {
                df_raw %>%
                    mutate(
                        hit_rate_daily = test_hit / test_total
                    )
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
    mutate(
        raw = purrr::map(
            raw,
            function(df_raw) {
                df_raw %>%
                    group_by(
                        dev_lot,
                        conc
                    ) %>%
                    summarize(
                        test_hit_sum = sum(test_hit),
                        test_total_sum = sum(test_total)
                    ) %>%
                    mutate(
                        hit_rate = test_hit_sum / test_total_sum
                    )
            }
        )
    ) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "dev_lot"]
    ) %>%
    mutate_raw2fit() %>%
    mutate_fit2goodness_of_fit() %>%
    mutate_fit2lod(beta = params$beta)


stage3_share_regression <- stage2_analyze %>%
    select(
        dev_lot,
        fit_summary_tidy,
        goodness_of_fit,
        lot_lod
    ) %>%
    tidyr::unnest(fit_summary_tidy) %>%
    tidyr::nest(
        fit_summary_tidy = everything()
    ) %>%
    mutate_fitsummary2ft()

stage3_share_fitplot <- stage2_analyze %>%
    select(
        dev_lot,
        raw
    ) %>%
    mutate_raw2fitplot()

stage3_share_fitplot <- patchwork::wrap_plots(stage3_share_fitplot$fitplot, ncol = 1)
