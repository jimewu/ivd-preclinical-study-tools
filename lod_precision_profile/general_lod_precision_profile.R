mutate_raw2ft <- function(df, test_name, unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(raw) {
                    raw %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            dev_lot = "Device Lot",
                            mean = "Mean",
                            sd_wl = "Within-lab SD",
                            n_test = "Test Number"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                rep(
                                    1,
                                    ncol(raw)
                                )
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            j = c(
                                ncol(raw) - 2,
                                ncol(raw) - 1
                            ),
                            value = as_paragraph(
                                paste(
                                    "unit of value:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[1]
                        ) %>%
                        return()
                }
            )
        )
}

mutate_raw2plot <- function(df, test_name, unit) {
    df %>%
        mutate(
            raw_plot = purrr::map(
                raw,
                function(raw) {
                    # 1. 原本的散佈圖 (Jitter Plot)
                    result <- raw %>%
                        ggplot(
                            aes(
                                x = mean,
                                y = sd_wl,
                                color = dev_lot
                            )
                        ) +
                        geom_jitter() +
                        labs(
                            x = paste(
                                "Mean",
                                paste(
                                    "(",
                                    unit,
                                    ")"
                                )
                            ),
                            y = paste(
                                "Within-lab SD",
                                paste(
                                    sep = "",
                                    "(",
                                    unit,
                                    ")"
                                )
                            ),
                            color = "Lot",
                            title = "Trend over Mean"
                        )

                    return(result)
                }
            )
        ) %>%
        return()
}

mutate_raw2lot_summary <- function(df) {
    df %>%
        mutate(
            lot_summary = purrr::map(
                raw,
                function(raw) {
                    raw %>%
                        summarize(
                            lot_k = n(),
                            lot_n_tot = sum(n_test)
                        ) %>%
                        mutate(
                            lot_cp = 1.645 / (1 - (1 / (4 * (lot_n_tot - lot_k))))
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}

mutate_raw2fit <- function(df) {
    df %>%
        mutate(
            fit1 = purrr::map(
                raw,
                function(df_raw) {
                    model <- lm(
                        sd_wl ~ mean,
                        data = df_raw
                    )

                    return(model)
                }
            ),
            fit2 = purrr::map(
                raw,
                function(df_raw) {
                    df_raw <- df_raw %>%
                        mutate(
                            sd_wl0.5 = sd_wl^0.5
                        )

                    model <- lm(
                        sd_wl0.5 ~ mean,
                        data = df_raw
                    )

                    return(model)
                }
            ),
            fit3 = purrr::map(
                raw,
                function(df_raw) {
                    df_raw <- df_raw %>%
                        mutate(
                            sd_wl0.3 = sd_wl^(1 / 3)
                        )

                    model <- lm(
                        sd_wl0.3 ~ mean,
                        data = df_raw
                    )

                    return(model)
                }
            )
        ) %>%
        mutate(
            fit1_summary = purrr::map(
                fit1,
                summary
            ),
            fit2_summary = purrr::map(
                fit2,
                summary
            ),
            fit3_summary = purrr::map(
                fit3,
                summary
            )
        ) %>%
        mutate(
            fit1_summary_tidy = purrr::map(
                fit1_summary,
                function(x) {
                    result <- broom::tidy(x) %>%
                        mutate(
                            overall_p_value = p.value[2]
                        ) %>%
                        select(-p.value) %>%
                        mutate(
                            if_significant = ifelse(
                                overall_p_value < 0.05,
                                "Y",
                                "N"
                            )
                        )
                    return(result)
                }
            ),
            fit2_summary_tidy = purrr::map(
                fit2_summary,
                function(x) {
                    result <- broom::tidy(x) %>%
                        mutate(
                            overall_p_value = p.value[2]
                        ) %>%
                        select(-p.value) %>%
                        mutate(
                            if_significant = ifelse(
                                overall_p_value < 0.05,
                                "Y",
                                "N"
                            )
                        )
                    return(result)
                }
            ),
            fit3_summary_tidy = purrr::map(
                fit3_summary,
                function(x) {
                    result <- broom::tidy(x) %>%
                        mutate(
                            overall_p_value = p.value[2]
                        ) %>%
                        select(-p.value) %>%
                        mutate(
                            if_significant = ifelse(
                                overall_p_value < 0.05,
                                "Y",
                                "N"
                            )
                        )
                    return(result)
                }
            )
        )
}

# 定義一個安全的求解函數
solve_LoD_safe <- function(target_func, LoB) {
    # 1. 設定範圍: LoB 到 10倍 LoB
    range_lower <- LoB
    range_upper <- 10 * LoB

    # 2. 設定精度: 為 LoB 的萬分之一 (可依需求調整，例如 1e-6)
    precision <- LoB * 1e-4

    # 3. 使用 tryCatch 進行求解
    tryCatch(
        {
            # 嘗試求解
            result <- uniroot(target_func,
                interval = c(range_lower, range_upper),
                tol = precision
            )
            return(result$root)
        },
        error = function(e) {
            # 如果無解 (例如: 兩端點同號) 或 出錯，回傳 NA
            return(NA)
        }
    )
}

mutate_fit2lod <- function(df, lob) {
    df %>%
        mutate(
            lod1 = purrr::map2_dbl(
                fit1,
                lot_cp,
                function(fit1,
                         lot_cp) {
                    # 提取係數
                    b0 <- coef(fit1)[1]
                    b1 <- coef(fit1)[2]

                    # 直接計算
                    lod <- (lob + lot_cp * b0) / (1 - lot_cp * b1)

                    return(lod)
                }
            ),
            lod2 = purrr::map2_dbl(
                fit2,
                lot_cp,
                function(fit2,
                         lot_cp) {
                    # 提取係數
                    b0 <- coef(fit2)[1]
                    b1 <- coef(fit2)[2]

                    # 定義目標函數: (LoB + Cp * SD) - Mean = 0
                    # SD = (b0 + b1 * x)^2
                    func_model2 <- function(x) {
                        predicted_SD <- (b0 + b1 * x)^2
                        return((lob + lot_cp * predicted_SD) - x)
                    }

                    # 執行求解
                    lod <- solve_LoD_safe(func_model2, lob)

                    return(lod)
                }
            ),
            lod3 = purrr::map2_dbl(
                fit3,
                lot_cp,
                function(fit3,
                         lot_cp) {
                    # 提取係數
                    b0 <- coef(fit3)[1]
                    b1 <- coef(fit3)[2]

                    # 定義目標函數: (LoB + Cp * SD) - Mean = 0
                    # SD = (b0 + b1 * x)^3
                    func_model3 <- function(x) {
                        predicted_SD <- (b0 + b1 * x)^3
                        return((lob + lot_cp * predicted_SD) - x)
                    }

                    # 執行求解
                    lod <- solve_LoD_safe(func_model3, lob)

                    return(lod)
                }
            )
        )
}


mutate_analyze2ft <- function(df) {
    df %>% mutate(
        ft = purrr::map(
            analyze,
            function(df_analyze) {
                df_analyze <- df_analyze %>%
                    mutate_if(
                        is.numeric,
                        function(x) {
                            round(x, digits = 3) %>%
                                return()
                        }
                    )

                ft <- df_analyze %>%
                    flextable() %>%
                    set_header_labels(
                        dev_lot = "Device Lot",
                        lot_k = "Lot K",
                        lot_n_tot = "Lot n",
                        lot_cp = "Lot Cp"
                    ) %>%
                    align(
                        align = "center",
                        part = "all"
                    ) %>%
                    width_ratio(
                        width_col_ratio = rep(1, ncol(df_analyze)),
                        width_visible = 7
                    )
            }
        )
    )
}

mutate_fit_summary_tidy2ft <- function(df) {
    df %>%
        mutate(
            ft = purrr::map(
                fit_summary_tidy,
                function(x) {
                    x <- x %>%
                        select(-statistic) %>%
                        mutate_if(
                            is.numeric,
                            function(y) {
                                round(y, digits = 3) %>%
                                    return()
                            }
                        )

                    x %>%
                        flextable() %>%
                        set_header_labels(
                            dev_lot = "Device Lot",
                            term = "Item",
                            estimate = "Value",
                            std.error = "Standard Error",
                            overall_p_value = "Regression p-value",
                            if_significant = "Significant (Y) or Not (N)?",
                            lod1 = "LoD"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        merge_v(
                            part = "body"
                        )
                }
            )
        )
}
