source("conf_toolkit/lib_load_package.R")

source("conf_toolkit/lib_officeverse.R")

source("conf_toolkit/lib_format_flextable.R")

source("lib_general_linearity.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    unit = "mg/dL"
)

# 設定檔案路徑
file_path <- "template_linearity.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
# if (!require("VCA")) install.packages("VCA")

# * Stage 1: Import (and Tidy)
stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    group_by(rc, y_ref) %>%
    mutate(replicate = row_number()) %>%
    ungroup() %>%
    tidyr::nest(
        raw = everything()
    ) %>%
    mutate_raw2wide() %>%
    mutate_raw_wide2ft(
        ncol_extra = 1,
        replicate = max(relpicate),
        unit = params$unit
    )

# * Stage 2: QC
stage2_qc <- stage1_import %>%
    select(
        raw
    ) %>%
    mutate(
        raw_plot = purrr::map(
            raw,
            function(x) {
                result <- x %>%
                    ggplot(
                        aes(
                            x = rc,
                            y = y,
                            color = factor(replicate)
                        )
                    ) +
                    geom_point() +
                    labs(
                        x = "Relative Concentration (RC)",
                        y = paste(
                            "Measured",
                            params$unit,
                            sep = " "
                        ),
                        color = "Replicate"
                    ) %>%
                    return()
            }
        ),
        raw_plot_facet = purrr::map(
            raw_plot,
            function(x) {
                result <- x +
                    facet_wrap(
                        ~rc,
                        scales = "free"
                    ) %>%
                    return()
            }
        )
    ) %>%
    mutate(
        summary = purrr::map(
            raw,
            function(x) {
                result <- x %>%
                    group_by(
                        rc
                    ) %>%
                    summarize(
                        measured_value = mean(y),
                        sd = sd(y),
                        cv = 100 * sd / measured_value,
                        var = var(y),
                        n = n(),
                        weight = n / var
                    ) %>%
                    arrange(
                        desc(rc)
                    )

                high <- result %>%
                    filter(
                        rc == 1
                    ) %>%
                    .$measured_value

                result <- result %>%
                    mutate(
                        expected_value = high * rc
                    ) %>%
                    relocate(
                        expected_value,
                        .after = measured_value
                    ) %>%
                    return()
            }
        )
    ) %>%
    mutate_summary2ft(
        ncol_extra = 2,
        unit = params$unit
    ) %>%
    mutate_summary2sd_cv_plot(unit = params$unit)

# * Stage 3: Analysis
stage3_analysis_linearity <- stage2_qc %>%
    select(
        raw,
        summary
    ) %>%
    mutate(
        fit = purrr::map(
            summary,
            function(x) {
                result <- lm(
                    formula = "measured_value ~ expected_value",
                    data = x,
                    weights = weight
                ) %>%
                    return()
            }
        ),
        fit_summary = purrr::map(
            fit,
            function(x) {
                result <- summary(x) %>%
                    return()
            }
        ),
        fit_summary_tidy = purrr::map(
            fit_summary,
            function(x) {
                result <- broom::tidy(x) %>%
                    mutate(
                        overall_p_value = p.value[2]
                    ) %>%
                    select(-p.value) %>%
                    return()
            }
        ),
        fit_summary_ft = purrr::map(
            fit_summary_tidy,
            function(x) {
                result <- x %>%
                    select(-statistic) %>%
                    mutate(
                        if_significant = ifelse(
                            overall_p_value < 0.05,
                            "Y",
                            "N"
                        ),
                        term = c(
                            "(Intercept)",
                            "Expected Value"
                        ),
                        estimate = round(
                            estimate,
                            digits = 3
                        ),
                        std.error = round(
                            std.error,
                            digits = 3
                        ),
                        overall_p_value = format(
                            overall_p_value,
                            digits = 3,
                            scientific = TRUE
                        )
                    ) %>%
                    flextable() %>%
                    set_header_labels(
                        term = "Item",
                        estimate = "Value",
                        std.error = "Standard Error",
                        overall_p_value = "Regression p-value",
                        if_significant = "Significant (Y) or Not (N)?"
                    ) %>%
                    merge_v(
                        part = "body",
                        j = 4:5
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    width_ratio(
                        width_col_ratio = c(
                            1.5,
                            rep(1, 4)
                        )
                    )
            }
        ),
        predict = purrr::map2(
            .x = fit,
            .y = summary,
            function(x, y) {
                expected_value_ori <- y$expected_value

                result <- tibble(
                    expected_value = expected_value_ori,
                    predicted_value = predict(
                        x,
                        newdata = data.frame(
                            expected_value = expected_value_ori
                        )
                    )
                ) %>%
                    return()
            }
        ),
        merge = purrr::map2(
            .x = summary,
            .y = predict,
            function(x, y) {
                result <- merge(
                    x,
                    y,
                    by = "expected_value"
                ) %>%
                    mutate(
                        deviation = measured_value - predicted_value,
                        perc_deviation = 100 * deviation / predicted_value
                    ) %>%
                    return()
            }
        ),
        merge_ft = purrr::map(
            merge,
            function(x) {
                x <- x %>%
                    select(
                        rc,
                        measured_value,
                        expected_value,
                        predicted_value,
                        deviation,
                        perc_deviation
                    )

                result <- x %>%
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
                    arrange(
                        desc(rc)
                    ) %>%
                    flextable() %>%
                    set_header_labels(
                        rc = "RC",
                        measured_value = "Measured Value",
                        expected_value = "Expected Value",
                        predicted_value = "Predicted Value",
                        deviation = "Deviation",
                        perc_deviation = "%Deviation"
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = which(
                            colnames(x) == "measured_value"
                        ),
                        value = as_paragraph(
                            "Mean of replicates"
                        ),
                        ref_symbols = letters[1]
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = which(
                            colnames(x) == "expected_value"
                        ),
                        value = as_paragraph(
                            "RC * Measured Value of HIGH"
                        ),
                        ref_symbols = letters[2]
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = which(
                            colnames(x) == "deviation"
                        ),
                        value = as_paragraph(
                            "Measured Value - Predicted Value"
                        ),
                        ref_symbols = letters[3]
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = which(
                            colnames(x) == "perc_deviation"
                        ),
                        value = as_paragraph(
                            "100 * Deviation / (Predicted Value)"
                        ),
                        ref_symbols = letters[4]
                    ) %>%
                    return()
            }
        )
    )

stage3A_analysis_recovery <- stage1_import %>%
    select(raw) %>%
    mutate(
        recovery = purrr::map(
            raw,
            function(x) {
                result <- x %>%
                    group_by(rc, y_ref) %>%
                    summarize(
                        measured_value = mean(y)
                    ) %>%
                    mutate(
                        recovery = 100 * measured_value / y_ref
                    ) %>%
                    arrange(
                        desc(rc)
                    ) %>%
                    return()
            }
        ),
        recovery_ft = purrr::map(
            recovery,
            function(recovery) {
                result <- recovery %>%
                    relocate(
                        measured_value,
                        .after = y_ref
                    ) %>%
                    mutate_if(
                        is.numeric,
                        function(x) {
                            result <- x %>%
                                round(
                                    digits = 3
                                ) %>%
                                return()
                        }
                    ) %>%
                    flextable() %>%
                    set_header_labels(
                        rc = "RC",
                        y_ref = "Ref.",
                        measured_value = "Measured Value",
                        recovery = "%Recovery"
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    footnote(
                        part = "header",
                        i = 1,
                        j = c(
                            "y_ref",
                            "measured_value"
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
                    return()
            }
        )
    )
