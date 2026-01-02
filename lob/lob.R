source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("general_lob.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL",
    test_name = "Subject Device"
)

# 設定檔案路徑
file_path <- "lob_sample_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("patchwork")) install.packages("patchwork")
if (!require("purrr")) install.packages("purrr")

# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        lot = factor(lot), # 將 conc 欄位轉為 sample 欄位
        day = factor(day),
        y = as.numeric(y)
    ) %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate(
        summary = purrr::map(
            raw,
            function(raw) {
                nlot <- raw$lot %>%
                    factor() %>%
                    levels() %>%
                    length()

                nday <- raw$day %>%
                    factor() %>%
                    levels() %>%
                    length()

                replicate <- nrow(raw) / (nlot * nday)

                data.frame(
                    nlot,
                    nday,
                    replicate
                ) %>%
                    return()
            }
        )
    ) %>%
    mutate_raw2ft(
        test_name = params$test_name,
        unit = params$unit
    ) %>%
    mutate_raw2plot(
        test_name = params$test_name,
        unit = params$unit
    )

# * Stage 2: Analyze
stage2_analyze <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "lot"]
    ) %>%
    mutate(
        shapiro_test = purrr::map(
            raw,
            function(raw) {
                shapiro.test(raw$y) %>%
                    return()
            }
        )
    ) %>%
    mutate(
        shapiro_p = purrr::map_dbl(
            shapiro_test,
            function(df) {
                return(
                    round(
                        df$p.value,
                        digits = 3
                    )
                )
            }
        )
    ) %>%
    mutate(
        if_norm = purrr::map_lgl(
            shapiro_test,
            function(x) {
                if (x$p.value >= 0.05) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }
        )
    ) %>%
    cbind(
        .,
        all_norm = all(.$if_norm)
    ) %>%
    mutate(
        lob_summary = purrr::map2(
            .x = raw,
            .y = all_norm,
            function(raw, all_norm) {
                if (all_norm) {
                    # Parametric算法
                    raw %>%
                        summarize(
                            mean_blank = mean(y),
                            sd_blank = sd(y),
                            cp = 1.645 / (1 - (1 / (4 * (nrow(raw) - 5)))),
                            lot_lob = mean_blank + cp * sd_blank
                        ) %>%
                        return()
                } else {
                    # Non-parametric算法
                    raw %>%
                        arrange(y) %>%
                        summarize(
                            rank_pos = 0.5 + 0.95 * length(y),
                            lot_lob = mean(
                                y[ceiling(rank_pos)],
                                y[floor(rank_pos)]
                            )
                        ) %>%
                        return()
                }
            }
        )
    ) %>%
    mutate(
        lot_lob = purrr::map_dbl(
            lob_summary,
            function(df) {
                return(df$lot_lob)
            }
        )
    ) %>%
    cbind(
        .,
        final_lob = max(.$lot_lob)
    )

stage2A_analyze_lob_summary <- stage2_analyze %>%
    select(
        lot,
        shapiro_p,
        if_norm,
        lob_summary,
        final_lob
    ) %>%
    tidyr::unnest(lob_summary) %>%
    tidyr::nest(
        lob_summary = everything()
    ) %>%
    mutate_lob_summary2ft(
        unit = params$unit
    )
