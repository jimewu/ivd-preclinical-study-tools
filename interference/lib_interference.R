source("conf_toolkit/lib_load_package.R")

source("conf_toolkit/lib_officeverse.R")

source("conf_toolkit/lib_format_flextable.R")

source("lib_general_interference.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    unit = "mg/dL",
    acceptance_criteria_diff = 3,
    acceptance_criteria_perc_diff = 5
)

# 設定檔案路徑
file_path <- "interference_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
# if (!require("VCA")) install.packages("VCA")

# * Stage 1: Import
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    group_by(
        analyte_conc,
        interferent,
        interferent_conc
    ) %>%
    mutate(replicate = row_number()) %>%
    ungroup() %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate_raw2wide() %>%
    mutate_raw_wide2ft(
        ncol_extra = 1,
        replicate = max(replicate),
        unit = params$unit
    ) %>%
    mutate(
        raw_plot = purrr::map(
            raw,
            function(raw) {
                raw <- raw %>%
                    mutate(
                        label_x = paste(
                            "Analyte Conc.:",
                            analyte_conc
                        )
                    ) %>%
                    arrange(
                        interferent,
                        analyte_conc
                    )

                result <- raw %>%
                    ggplot(
                        aes(
                            x = interferent_conc,
                            y = y,
                            color = factor(replicate)
                        )
                    ) +
                    geom_point(
                        alpha = 0.7
                    ) +
                    facet_wrap(
                        interferent ~ label_x,
                        scales = "free_x"
                    ) +
                    labs(
                        x = "Interferent Conc.",
                        y = paste(
                            "Measured Analyte Values (",
                            params$unit,
                            ")",
                            sep = ""
                        ),
                        color = "Replicate"
                    )
            }
        )
    )

# * Stage 2: Analysis
stage2_analyze <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = -analyte_conc
    ) %>%
    mutate(
        paired_difference = purrr::map(
            raw,
            function(x) {
                x %>%
                    group_by(
                        interferent,
                        interferent_conc
                    ) %>%
                    summarize(
                        mean = mean(y)
                    ) %>%
                    group_by(interferent) %>%
                    mutate(
                        mean_con = mean[1],
                        diff = mean - mean_con,
                        perc_diff = 100 * diff / mean_con
                    ) %>%
                    ungroup() %>%
                    select(-mean_con) %>%
                    return()
            }
        )
    ) %>%
    mutate(
        paired_difference_ft = purrr::map2(
            .x = paired_difference,
            .y = analyte_conc,
            function(x, y) {
                result_df <- x %>%
                    cbind(
                        analyte_conc = y,
                        .
                    ) %>%
                    mutate_if(
                        is.numeric,
                        function(x) {
                            round(x, digits = 3) %>%
                                return()
                        }
                    )

                result_ft <- result_df %>%
                    flextable() %>%
                    set_header_labels(
                        analyte_conc = "Analyte\nConc.",
                        interferent = "Name",
                        interferent_conc = "Conc.",
                        mean = "Mean",
                        diff = "Difference",
                        perc_diff = "%Difference"
                    ) %>%
                    add_header_row(
                        top = TRUE,
                        values = c(
                            "Analyte\nConc.",
                            "Interferent",
                            "Result"
                        ),
                        colwidths = c(
                            1,
                            2,
                            3
                        )
                    ) %>%
                    merge_v(
                        part = "header"
                    ) %>%
                    merge_v(
                        part = "body",
                        j = c(
                            "analyte_conc",
                            "interferent"
                        )
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = c(
                            "analyte_conc"
                        ),
                        value = as_paragraph(
                            paste(
                                "unit of value:",
                                params$unit,
                                sep = " "
                            )
                        ),
                        ref_symbols = letters[1]
                    ) %>%
                    footnote(
                        part = "header",
                        i = 2,
                        j = c(
                            "mean",
                            "diff"
                        ),
                        value = as_paragraph(
                            paste(
                                "unit of value:",
                                params$unit,
                                sep = " "
                            )
                        ),
                        ref_symbols = letters[2]
                    ) %>%
                    width_ratio(
                        width_col_ratio = c(
                            1,
                            2,
                            2,
                            rep(1, 3)
                        ),
                        width_visible = 7
                    )

                return(result_ft)
            }
        )
    )

stage3_dose_responsoe <- stage2_analyze %>%
    select(
        analyte_conc,
        paired_difference
    ) %>%
    tidyr::unnest(
        paired_difference
    ) %>%
    tidyr::nest(
        paired_difference = c(
            "interferent_conc",
            "mean",
            "diff",
            "perc_diff"
        )
    ) %>%
    mutate(
        interferent_max_dose_by_diff = purrr::map_dbl(
            paired_difference,
            function(pair_diff) {
                pair_diff <- pair_diff %>%
                    mutate(
                        diff = abs(diff),
                        perc_diff = abs(perc_diff)
                    )

                if (
                    max(pair_diff$diff) <= params$acceptance_criteria_diff
                ) {
                    result <- max(
                        as.numeric(pair_diff$interferent_conc)
                    )
                } else {
                    # 1. 找出小於目標值，且最大的那一列
                    lower <- pair_diff %>%
                        filter(
                            diff <= params$acceptance_criteria_diff
                        ) %>%
                        slice_max(diff, n = 1) %>%
                        slice_max(interferent_conc, n = 1)

                    # 2. 找出大於目標值，且最小的那一列
                    upper <- pair_diff %>%
                        filter(
                            diff > params$acceptance_criteria_diff
                        ) %>%
                        slice_min(diff, n = 1) %>%
                        slice_min(interferent_conc, n = 1)

                    slope <- (
                        upper$diff - lower$diff
                    ) / (
                        upper$interferent_conc - lower$interferent_conc
                    )

                    result <- lower$interferent_conc + (params$acceptance_criteria_diff - lower$diff) / slope
                }

                return(result)
            }
        )
    ) %>%
    mutate(
        interferent_max_dose_by_perc_diff = purrr::map_dbl(
            paired_difference,
            function(pair_diff) {
                pair_diff <- pair_diff %>%
                    mutate(
                        diff = abs(diff),
                        perc_diff = abs(perc_diff)
                    )

                if (
                    max(pair_diff$perc_diff) <= params$acceptance_criteria_perc_diff
                ) {
                    result <- max(
                        as.numeric(pair_diff$interferent_conc)
                    )
                } else {
                    # 1. 找出小於目標值，且最大的那一列
                    lower <- pair_diff %>%
                        filter(
                            perc_diff <= params$acceptance_criteria_perc_diff
                        ) %>%
                        slice_max(perc_diff, n = 1) %>%
                        slice_max(interferent_conc, n = 1)

                    # 2. 找出大於目標值，且最小的那一列
                    upper <- pair_diff %>%
                        filter(
                            perc_diff > params$acceptance_criteria_perc_diff
                        ) %>%
                        slice_min(perc_diff, n = 1) %>%
                        slice_min(interferent_conc, n = 1)

                    slope <- (
                        upper$perc_diff - lower$perc_diff
                    ) / (
                        upper$interferent_conc - lower$interferent_conc
                    )

                    result <- lower$interferent_conc + (params$acceptance_criteria_perc_diff - lower$perc_diff) / slope
                }

                return(result)
            }
        )
    ) %>%
    select(-paired_difference) %>%
    relocate(
        interferent,
        .before = analyte_conc
    ) %>%
    arrange(
        interferent,
        analyte_conc
    ) %>%
    tidyr::nest(
        interferent_max_dose = everything()
    ) %>%
    mutate(
        interferent_max_dose_ft = purrr::map(
            interferent_max_dose,
            function(df) {
                df %>%
                    mutate_if(
                        is.numeric,
                        function(x) {
                            result <- round(
                                x,
                                digits = 3
                            ) %>%
                                return()
                        }
                    ) %>%
                    flextable() %>%
                    set_header_labels(
                        interferent = "Interferent",
                        analyte_conc = "Analyte Conc.",
                        interferent_max_dose_by_diff = "Max. Interferent Conc by Diffence",
                        interferent_max_dose_by_perc_diff = "Max. Interferent Conc by %Diffence"
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    merge_v(
                        part = "body",
                        j = "interferent"
                    )
            }
        )
    )
