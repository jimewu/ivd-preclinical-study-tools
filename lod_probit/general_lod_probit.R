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
                            day = "Day",
                            conc = "Sample Conc.",
                            test_hit = "Npos",
                            test_total = "Ntot",
                            hit_rate_daily = "Daily Hit Rate"
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
                            j = 2,
                            value = as_paragraph(
                                paste(
                                    "unit of value:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[1]
                        )
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
                                x = day,
                                y = hit_rate_daily,
                                color = dev_lot
                            )
                        ) +
                        geom_jitter() +
                        labs(
                            x = "Day",
                            y = "Daily Hit Rate",
                            color = "Lot",
                            title = "Trend over Day"
                        ) +
                        facet_wrap(~conc)

                    return(result)
                }
            )
        ) %>%
        return()
}

mutate_raw2fit <- function(df) {
    df %>%
        mutate(
            fit = purrr::map(
                raw,
                function(df_raw) {
                    df_raw <- df_raw %>%
                        filter(conc > 0)

                    # 公式：cbind(陽性數, 陰性數) ~ log10(濃度)
                    model <- glm(cbind(test_hit_sum, test_total_sum - test_hit_sum) ~ log10(conc),
                        family = binomial(link = "probit"),
                        data = df_raw
                    )

                    return(model)
                }
            )
        ) %>%
        mutate(
            fit_summary = purrr::map(
                fit,
                summary
            )
        ) %>%
        mutate(
            fit_summary_tidy = purrr::map(
                fit,
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

# 基於CLSI EP17 p.37要求:
# Deviance statistic (or Pearson chi-square statistic) to the quantile of chi-square distribution
# 拿regression的總誤差(deviance)跟自由度相比，若顯著不同表示regression不好。
mutate_fit2goodness_of_fit <- function(df) {
    df %>%
        mutate(
            goodness_of_fit = purrr::map_dbl(
                fit,
                function(fit) {
                    p_value_gof <- pchisq(
                        fit$deviance,
                        fit$df.residual,
                        lower.tail = FALSE
                    )

                    return(p_value_gof)
                }
            )
        )
}

mutate_fit2lod <- function(df, beta) {
    df %>%
        mutate(
            lot_lod = purrr::map_dbl(
                fit,
                function(fit) {
                    z_target <- qnorm(1 - beta)

                    # 提取係數
                    beta0 <- coef(fit)[1] # 截距 (Intercept)
                    beta1 <- coef(fit)[2] # 斜率 (Slope of log10_conc)

                    # 反推濃度
                    # Z = beta0 + beta1 * log10(LoD)
                    # log10(LoD) = (Z - beta0) / beta1
                    log_lod <- (z_target - beta0) / beta1
                    lod <- 10^log_lod

                    return(lod)
                }
            )
        )
}

mutate_fitsummary2ft <- function(df) {
    df %>%
        mutate(
            fit_summary_tidy_ft = purrr::map(
                fit_summary_tidy,
                function(fit_summary_tidy) {
                    fit_summary_tidy <- fit_summary_tidy %>%
                        select(-statistic) %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        )

                    fit_summary_tidy_ft <- fit_summary_tidy %>%
                        flextable() %>%
                        set_header_labels(
                            dev_lot = "Device Lot",
                            sample = "Sample",
                            term = "Item",
                            estimate = "Value",
                            std.error = "Standard Error",
                            overall_p_value = "Regression p-value",
                            if_significant = "Significant (Y) or Not (N)?",
                            goodness_of_fit = "Goodness of Fit (Good: ≥0.05)",
                            lot_lod = "Lot LoD"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        footnote(
                            part = "header",
                            j = ncol(fit_summary_tidy) - 1,
                            value = as_paragraph(
                                paste(
                                    "p-value of the chi-square test on model deviance"
                                )
                            ),
                            ref_symbols = letters[1]
                        )

                    return(fit_summary_tidy_ft)
                }
            )
        )
}

mutate_raw2fitplot <- function(df) {
    df %>%
        mutate(
            fitplot = purrr::map2(
                raw,
                dev_lot,
                function(df_raw, dev_lot) {
                    df_raw_no0 <- df_raw %>%
                        filter(conc != 0)


                    plot <- ggplot(df_raw_no0, aes(x = conc, y = hit_rate)) +
                        geom_point(size = 3) +
                        geom_smooth(
                            data = df_raw_no0, # 再次確保這裡使用的是無 0 的資料
                            method = "glm",

                            # 【關鍵修正 1】公式改為 y ~ x
                            # 因為 scale_x_log10 已經把 x 轉成 log 了，
                            # 這裡的線性關係 (y ~ x) 在數學上就等同於 Probit = a + b * log(conc)
                            formula = y ~ x,

                            # 設定 GLM 為 Probit 模型
                            method.args = list(family = binomial(link = "probit")),

                            # 設定權重 (分母)
                            aes(weight = test_total_sum),
                            se = TRUE,
                            color = "blue"
                        ) +
                        scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4)) +
                        theme_bw() +
                        labs(
                            title = paste(
                                "Probit Analysis of Device Lot",
                                dev_lot
                            ),
                            x = "log10(Conc.)",
                            y = "Hit Rate"
                        )
                }
            )
        )
}
