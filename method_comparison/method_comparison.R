source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_method_comparison.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    loq = "1.5",
    ref_name = "Reference Method",
    test_name = "Subject Device",
    err_ratio = 1,
    alpha = 0.05,
    conc_crit = 50
)

# 設定檔案路徑
file_path <- "method_comparison_sample_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("EnvStats")) install.packages("EnvStats")
if (!require("mcr")) install.packages("mcr")

# * Stage 1: Import
stage1_import <- readxl::read_excel(
    path = file_path,
    sheet = sheet_name,
    col_names = TRUE
) %>%
    # 確保資料為數值型態
    mutate(
        across(everything(), as.numeric)
    ) %>%
    # 將資料 nest 進 'raw' 欄位，以符合後續 summary 及 mapping 的結構要求 [2]
    tidyr::nest(
        raw = everything()
    ) %>%
    raw2sum(loq = params$loq) %>%
    raw2ft(
        loq = params$loq,
        ref_name = params$ref_name,
        test_name = params$test_name,
        unit = params$unit
    ) %>%
    raw2plot(
        ref_name = params$ref_name,
        test_name = params$test_name,
        unit = params$unit
    ) %>%
    raw2_difference_plot(
        ref_name = params$ref_name,
        unit = params$unit
    ) %>%
    raw2perc_difference_plot(
        ref_name = params$ref_name
    )

# * Stage 2: QC
stage2_qc <-
    stage1_import %>%
    select(-raw_ft, -raw_plot) %>%
    raw2rosner_test_ft(alpha = params$alpha) %>%
    select(rosner_test_ft)

# * Stage 3: Analyze
stage3_analyze <- stage1_import %>%
    select(raw) %>%
    mutate(
        regression_method_full = "",
        regression_method = "",
        ci_method = ""
    ) %>%
    tidyr::nest(
        regression_parm = c(
            "regression_method_full",
            "regression_method",
            "ci_method"
        )
    ) %>%
    mutate(
        regression_parm = purrr::map(
            regression_parm,
            function(x) {
                result <- tibble(
                    regression_method_full = c(
                        "Ordinary Linear Regression",
                        "Deming Regression",
                        "Weighted Ordinary Linear Regression",
                        "Weighted Deming Regression",
                        "Passing-Bablok Regression"
                    ),
                    regression_method = c(
                        "LinReg",
                        "Deming",
                        "WLinReg",
                        "WDeming",
                        "PaBa"
                    ),
                    ci_method = c(
                        "bootstrap",
                        "analytical",
                        "bootstrap",
                        "bootstrap",
                        "bootstrap"
                    )
                )

                x <- result


                return(x)
            }
        )
    ) %>%
    tidyr::unnest(regression_parm) %>%
    ## 列出CLSI EP9中的regression方法以及其計算CI的方法
    tidyr::nest(
        regression_parm = c(
            "regression_method",
            "ci_method"
        )
    ) %>%
    ## mcreg: 以各種regression方法進行分析
    raw2mcreg(
        err_ratio = params$err_ratio,
        alpha = params$alpha,
        ref_name = params$ref_name,
        test_name = params$test_name
    ) %>%
    ## getcoef: 取出各項regression之結果表
    mutate(
        mcreg_coef = purrr::map(
            mcreg,
            function(x) {
                mcr::getCoefficients(x) %>%
                    as.data.frame() %>%
                    select(-SE) %>%
                    cbind(
                        item = rownames(.),
                        .
                    ) %>%
                    return()
            }
        )
    ) %>%
    mcreg_coef2ft(ncol_extra = 0) %>%
    ## if.viable: 比較各種regression結果中
    ## intercept是否通過0，且
    ## slope是否通過1
    mutate(
        if_viable = purrr::map_lgl(
            mcreg_coef,
            function(x) {
                if_intercept <-
                    all(
                        x["Intercept", "LCI"] < 0,
                        x["Intercept", "UCI"] > 0
                    )

                if_slope <-
                    all(
                        x["Slope", "LCI"] < 1,
                        x["Slope", "UCI"] > 1
                    )

                return(
                    all(if_intercept, if_slope)
                )
            }
        )
    ) %>%
    mutate(
        ci_area = purrr::map_dbl(
            mcreg_coef,
            function(x) {
                len_intercept <-
                    x["Intercept", "UCI"] - x["Intercept", "LCI"]

                len_slope <-
                    x["Slope", "UCI"] - x["Slope", "LCI"]

                ci_area <- len_intercept * len_slope

                return(ci_area)
            }
        )
    ) %>%
    arrange(
        desc(if_viable),
        ci_area
    ) %>%
    cbind(
        .,
        if_best = c(
            TRUE,
            rep(FALSE, 4)
        )
    ) %>%
    mcreg2bias(
        conc_crit = params$conc_crit
    ) %>%
    mutate(
        bias_ft = purrr::map(
            bias,
            function(bias) {
                if (is.data.frame(bias)) {
                    bias %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            "Prop.bias(%)" = "Proportional Bias (%)",
                            LCI = "95% CI Lwr.",
                            UCI = "95% CI Upr."
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        return()
                } else {
                    return(NA)
                }
            }
        )
    ) %>%
    ## predict: 取出以regression結果反算之數據
    mutate(
        predict = purrr::map2(
            .x = mcreg,
            .y = if_best,
            function(mcreg, if_best) {
                if (if_best) {
                    mcr::MCResult.getFitted(mcreg) %>%
                        as.data.frame() %>%
                        arrange(x_hat) %>%
                        return()
                } else {
                    return(NA)
                }
            }
        )
    )

stage3A_analyze_compare_fit_plot <- function(
    df = stage3_analyze) {
    mcr::compareFit(
        df$mcreg[5][[1]],
        df$mcreg[4][[1]],
        df$mcreg[3][[1]],
        df$mcreg[2][[1]],
        df$mcreg[1][[1]]
    ) %>%
        return()
}

stage3B_analyze_best_regression_plot <- function(
    df = stage3_analyze,
    ref_name,
    test_name) {
    mcr::MCResult.plot(
        df$mcreg[1][[1]],
        add.legend = FALSE,
        x.lab = ref_name,
        y.lab = test_name
    )
}

stage3C_analyzie_regression <- stage3_analyze %>%
    select(
        regression_method_full,
        mcreg_coef
    ) %>%
    tidyr::unnest(mcreg_coef) %>%
    tidyr::nest(
        mcreg_coef = colnames(.)
    ) %>%
    mcreg_coef2ft(ncol_extra = 1)
