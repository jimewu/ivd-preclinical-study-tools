source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_stability.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    test_name = "Subject Device",
    perc_allowable_drift = 5
)

# 設定檔案路徑

file_path <- "stability_sample_data_anchored.xlsx"
# file_path <- "stability_sample_data_non_anchored.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("broom")) install.packages("broom")
# if (!require("purrr")) install.packages("purrr")


# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        dev_lot = factor(dev_lot), # 將 conc 欄位轉為 sample 欄位
        sample = factor(sample),
        day = as.numeric(day),
        y = as.numeric(y),
        y_anchor = as.numeric(y_anchor)
    ) %>%
    mutate(
        any_has_no_anchor = any(is.na(.$y_anchor))
    ) %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate(
        summary = purrr::map(
            raw,
            function(raw) {
                nday <- raw$day %>%
                    max()

                nsample <- raw$sample %>%
                    factor() %>%
                    levels() %>%
                    length()

                nlot_dev <- raw$dev_lot %>%
                    factor() %>%
                    levels() %>%
                    length()

                data.frame(
                    nday,
                    nsample,
                    nlot_dev
                ) %>%
                    return()
            }
        )
    ) %>%
    mutate_raw2ft(unit = params$unit) %>%
    mutate_raw2plot(
        unit = params$unit,
        test_name = params$test_name
    )

# * Stage 2: Analyze
stage2_analyze <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[!colnames(.) %in% c("sample", "dev_lot", "any_has_no_anchor")]
    ) %>%
    arrange(
        dev_lot,
        sample
    ) %>%
    mutate_raw2fit() %>%
    mutate_fit2predict() %>%
    mutate_predict2drift(perc_allowable_drift = params$perc_allowable_drift)

# * Stage 3: Share
stage3_share_regression <- stage2_analyze %>%
    select(
        dev_lot,
        sample,
        fit_summary_tidy,
        any_has_no_anchor,
        equal_replicate
    ) %>%
    mutate(
        regression_method = ifelse(
            equal_replicate,
            "Ordinary Linear Regression",
            "Weighted Least Squares Regression"
        ),
        y = ifelse(
            any_has_no_anchor,
            "Mean of Measurement Value",
            "%Difference"
        )
    ) %>%
    select(
        -any_has_no_anchor,
        -equal_replicate
    ) %>%
    relocate(
        regression_method,
        .after = sample
    ) %>%
    relocate(
        y,
        .before = fit_summary_tidy
    ) %>%
    tidyr::unnest(fit_summary_tidy) %>%
    mutate_regression2ft()

stage3_share_drift_vs_z0 <- stage2_analyze %>%
    select(
        dev_lot,
        sample,
        perc_drift_vs_z0
    ) %>%
    tidyr::unnest(perc_drift_vs_z0) %>%
    mutate_drift2ft()

stage3_share_drift_vs_intercept <- stage2_analyze %>%
    select(
        dev_lot,
        sample,
        perc_drift_vs_intercept
    ) %>%
    tidyr::unnest(perc_drift_vs_intercept) %>%
    mutate_drift2ft()
