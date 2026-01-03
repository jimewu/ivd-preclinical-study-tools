mutate_raw2ft <- function(df, unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(raw) {
                    is_all_na <- all(is.na(raw$y_anchor))

                    if (is_all_na) {
                        raw <- raw %>%
                            select(-y_anchor)
                    }

                    df <- raw %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        )

                    ft <- df %>%
                        flextable() %>%
                        set_header_labels(
                            dev_lot = "Device Lot",
                            sample = "Sample",
                            day = "Day",
                            y = "On-test Measurement Value",
                            y_anchor = "Reference-anchored Measurement Value"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = rep(1, ncol(df)),
                            width_visible = 7
                        ) %>%
                        footnote(
                            part = "header",
                            j = c(
                                (ncol(df) - 1):ncol(df)
                            ),
                            value = as_paragraph(
                                paste(
                                    "unit of value:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[1]
                        )

                    return(ft)
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
                    if (raw$any_has_no_anchor[1]) {
                        plot <- ggplot(
                            raw,
                            aes(
                                x = day
                            )
                        ) +
                            geom_point(
                                aes(
                                    y = y
                                )
                            ) +
                            facet_grid(
                                dev_lot ~ sample
                            )
                    } else {
                        plot <- ggplot(
                            raw,
                            aes(
                                x = day
                            )
                        ) +
                            geom_point(
                                aes(
                                    y = y,
                                    color = "On-test"
                                )
                            ) +
                            geom_point(
                                aes(
                                    y = y_anchor,
                                    color = "Reference-Anchored"
                                ),
                                alpha = 0.2
                            ) +
                            facet_grid(
                                dev_lot ~ sample
                            ) +
                            # 3. 使用 scale_color_manual 設定顏色對應與圖例標題
                            scale_color_manual(
                                name = "Color", # 這是圖例的總標題 (可省略)
                                values = c(
                                    "On-test" = "#005CAF", # 名稱 A 對應 藍色
                                    "Reference-Anchored" = "red" # 名稱 B 對應 紅色
                                )
                            )
                    }

                    plot <- plot +
                        labs(
                            x = "Day",
                            y = paste(
                                "Measurement Values of",
                                test_name,
                                paste(
                                    sep = "",
                                    "(",
                                    unit,
                                    ")"
                                )
                            )
                        )

                    return(plot)
                }
            )
        )
}

# mutate_mean_daily2ft <- function(df) {
#     df %>%
#         mutate(
#             mean_daily_ft = purrr::map(
#                 mean_daily,
#                 function(mean_daily) {
#                     mean_daily %>%
#                         mutate_if(
#                             is.numeric,
#                             function(x) {
#                                 round(x, digits = 3) %>%
#                                     return()
#                             }
#                         ) %>%
#                         flextable() %>%
#                         set_header_labels(
#                             dev_lot = "Device Lot",
#                             sample = "Sample"
#                         ) %>%
#                         align(
#                             align = "center",
#                             part = "all"
#                         ) %>%
#                         merge_v(
#                             part = "body",
#                             j = 1:2
#                         ) %>%
#                         width_ratio(
#                             width_col_ratio = c(
#                                 1, 2,
#                                 rep(
#                                     1,
#                                     ncol(mean_daily) - 2
#                                 )
#                             )
#                         ) %>%
#                         footnote(
#                             part = "header",
#                             j = 3:ncol(mean_daily),
#                             value = as_paragraph(
#                                 paste(
#                                     "unit of value:",
#                                     basic$unit3,
#                                     sep = " "
#                                 )
#                             ),
#                             ref_symbols = letters[1]
#                         ) %>%
#                         return()
#                 }
#             )
#         )
# }

mutate_raw2fit <- function(df) {
    df %>%
        mutate(
            equal_replicate = purrr::map_lgl(
                raw,
                function(df_raw) {
                    sum <- df_raw %>%
                        group_by(day) %>%
                        summarize(n = n())

                    rep <- sum$n %>%
                        factor() %>%
                        levels() %>%
                        length()

                    if (rep == 1) {
                        return(TRUE)
                    } else {
                        return(FALSE)
                    }
                }
            )
        ) %>%
        mutate(
            raw_ext = purrr::map2(
                raw,
                any_has_no_anchor,
                function(df_raw, any_has_no_anchor) {
                    if (any_has_no_anchor) {
                        # 沒有y_anchor時分析y_mean
                        ## 預留se可以做weighted
                        result <- df_raw %>%
                            group_by(day) %>%
                            summarize(
                                z = mean(y),
                                se = sd(y) / sqrt(n())
                            )
                    } else {
                        # 有y_anchor時分析percent difference
                        ## 預留se可以做weighted
                        result <- df_raw %>%
                            mutate(
                                y_perc_diff_abs = 100 * abs(y - y_anchor) / y_anchor
                            ) %>%
                            group_by(day) %>%
                            summarize(
                                z = mean(y_perc_diff_abs),
                                se = sd(y_perc_diff_abs) / sqrt(n())
                            )
                    }

                    return(result)
                }
            )
        ) %>%
        mutate(
            z0 = purrr::map_dbl(
                raw_ext,
                function(df_raw) {
                    df_raw <- df_raw %>%
                        arrange(day)

                    df_raw$z[1] %>%
                        return()
                }
            )
        ) %>%
        mutate(
            fit = purrr::map2(
                raw_ext,
                equal_replicate,
                function(df_raw, equal_replicate) {
                    if (equal_replicate) {
                        # 若為 TRUE: 執行 Ordinary Linear Regression
                        # 不需要 weights 參數
                        model <- lm(z ~ day, data = df_raw)
                        print("正在執行: Ordinary Linear Regression")
                    } else {
                        # 若為 FALSE: 執行 Weighted Least Squares Regression
                        # weights 設定為 1 / se^2 (方差的倒數，代表精確度越高的點權重越大)
                        model <- lm(z ~ day, data = df_raw, weights = 1 / se^2)
                        print("正在執行: Weighted Least Squares Regression")
                    }

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
                fit_summary,
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
            fit_summary_tidy = purrr::map2(
                fit_summary_tidy,
                raw_ext,
                function(x, y) {
                    cbind(
                        x,
                        duration = max(y$day)
                    ) %>%
                        as_tibble() %>%
                        return()
                }
            )
        ) %>%
        mutate(
            intercept = purrr::map_dbl(
                fit_summary_tidy,
                function(x) {
                    x$estimate[1] %>%
                        return()
                }
            )
        ) %>%
        mutate(
            if_significant = purrr::map_lgl(
                fit_summary_tidy,
                function(x) {
                    if (x$if_significant[1] == "Y") {
                        return(TRUE)
                    } else {
                        return(FALSE)
                    }
                }
            )
        )

    #     predict = purrr::map2(
    #         .x = fit,
    #         .y = fit_summary_tidy,
    #         function(x, y) {
    #             if (y$overall_p_value[1] < 0.05) {
    #                 range_day <- 1:(y$duration * 2)

    #                 result <- data.frame(
    #                     day = range_day,
    #                     mean_predict = predict(
    #                         x,
    #                         newdata = data.frame(
    #                             day = range_day
    #                         ),
    #                         interval = "confidence",
    #                         level = 0.95
    #                     )
    #                 ) %>%
    #                     mutate(
    #                         mean_predict = case_when(
    #                             y$estimate[2] < 0 ~ mean_predict.lwr,
    #                             TRUE ~ mean_predict.upr
    #                         )
    #                     )
    #             } else {
    #                 result <- NA
    #             }

    #             return(result)
    #         }
    #     ),
    #     maxday = purrr::map2_int(
    #         .x = raw_ext,
    #         .y = predict,
    #         function(x, y) {
    #             max_test_day <- x$duration[1]

    #             if (!is.data.frame(y)) {
    #                 result <- max_test_day
    #             } else if (y$mean_predict[nrow(y)] / y$mean_predict[1] < 1) {
    #                 result <- y %>%
    #                     filter(mean_predict >= x$limit_lwr)
    #             } else {
    #                 result <- y %>%
    #                     filter(mean_predict <= x$limit_upr)
    #             }

    #             if (is.data.frame(result)) {
    #                 result <- case_when(
    #                     nrow(result) == 0 ~ 0,
    #                     TRUE ~ max(result$day)
    #                 )
    #             }

    #             if (result > max_test_day) {
    #                 result <- max_test_day
    #             }

    #             return(result)
    #         }
    #     )
    # ) %>%
    # mutate(
    #     stability = min(maxday)
    # )
}

mutate_fit2predict <- function(df) {
    df %>%
        mutate(
            predict = purrr::map2(
                fit,
                raw_ext,
                function(fit, raw_ext) {
                    result <- predict(
                        fit,
                        newdata = data.frame(
                            day = raw_ext$day
                        ),
                        interval = "confidence",
                        level = 0.95
                    )

                    cbind(
                        day = raw_ext$day,
                        as_tibble(result)
                    )
                }
            )
        )
}

mutate_predict2drift <- function(df, perc_allowable_drift) {
    df %>%
        mutate(
            perc_drift_vs_z0 = purrr::pmap(
                list(
                    predict,
                    any_has_no_anchor,
                    z0
                ),
                function(predict, any_has_no_anchor, base) {
                    predict <- predict %>%
                        as_tibble()

                    if (any_has_no_anchor) {
                        result <- predict %>%
                            mutate(
                                perc_drift = 100 * (fit - base) / base,
                                drift_upper = 100 * (upr - base) / base,
                                drift_lower = 100 * (lwr - base) / base
                            )
                    } else {
                        # 有y_anchor的狀況: 預測值就是漂移
                        result <- predict %>%
                            mutate(
                                perc_drift = fit,
                                perc_drift_upper = upr,
                                perc_drift_lower = lwr
                            )
                    }

                    result <- result %>%
                        select(
                            -fit, -lwr, -upr
                        ) %>%
                        mutate(
                            acceptable = (abs(perc_drift) < perc_allowable_drift)
                        )

                    return(result)
                }
            )
        ) %>%
        mutate(
            perc_drift_vs_intercept = purrr::pmap(
                list(
                    predict,
                    any_has_no_anchor,
                    intercept
                ),
                function(predict, any_has_no_anchor, base) {
                    predict <- predict %>%
                        as_tibble()

                    if (any_has_no_anchor) {
                        result <- predict %>%
                            mutate(
                                perc_drift = 100 * (fit - base) / base,
                                drift_upper = 100 * (upr - base) / base,
                                drift_lower = 100 * (lwr - base) / base
                            )
                    } else {
                        # 有y_anchor的狀況: 預測值就是漂移
                        result <- predict %>%
                            mutate(
                                perc_drift = fit,
                                perc_drift_upper = upr,
                                perc_drift_lower = lwr
                            )
                    }

                    result <- result %>%
                        select(
                            -fit, -lwr, -upr
                        ) %>%
                        mutate(
                            acceptable = (abs(perc_drift) < perc_allowable_drift)
                        )

                    return(result)
                }
            )
        )
}

mutate_regression2ft <- function(df) {
    df <- df %>%
        select(-duration, -statistic)

    ft <- df %>%
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
            sample = "Sample",
            regression_method = "Regression Method",
            y = "y Used in Regression",
            term = "Item",
            estimate = "Value",
            std.error = "Standard Error",
            overall_p_value = "Regression p-value",
            if_significant = "Significant (Y) or Not (N)?"
        ) %>%
        align(
            align = "center",
            part = "all"
        ) %>%
        width_ratio(
            width_col_ratio = c(
                rep(1, 2),
                2,
                rep(
                    1,
                    ncol(df) - 3
                )
            )
        )

    return(ft)
}

mutate_drift2ft <- function(df) {
    df %>%
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
            sample = "Sample",
            day = "Day",
            perc_drift = "%Drift",
            perc_drift_upper = "%Drift 95%CI: Upr.",
            perc_drift_lower = "%Drift 95%CI: Lwr.",
            acceptable = "Pass"
        ) %>%
        align(
            align = "center",
            part = "all"
        ) %>%
        width_ratio(
            width_col_ratio = c(
                rep(1, ncol(df))
            )
        )
}

# ! 前一版, 以非平均值進行fit
raw2stability <- function(data, perc_allowable_drift) {
    stability <- data %>%
        mutate(
            raw_ext = purrr::map(
                raw,
                function(x) {
                    ep25_acceptance_criteria <- as.numeric(perc_allowable_drift)

                    ymean <- x %>%
                        group_by(day) %>%
                        summarize(
                            mean = mean(y)
                        ) %>%
                        arrange(day)

                    result <- cbind(
                        x,
                        y0 = ymean$mean[1]
                    ) %>%
                        mutate(
                            limit_upr = y0 * (1 + ep25_acceptance_criteria / 100),
                            limit_lwr = y0 * (1 - ep25_acceptance_criteria / 100)
                        )

                    return(result)
                }
            ),
            fit = purrr::map(
                raw,
                function(x) {
                    result <- lm(
                        formula = "y ~ day",
                        data = x
                    )

                    return(result)
                }
            ),
            fit_summary = purrr::map(
                fit,
                function(x) {
                    result <- summary(x)
                    return(result)
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
            predict = purrr::map2(
                .x = fit,
                .y = fit_summary_tidy,
                function(x, y) {
                    if (y$overall_p_value[1] < 0.05) {
                        range_day <- 1:999

                        result <- data.frame(
                            day = range_day,
                            y_predict = predict(
                                x,
                                newdata = data.frame(
                                    day = range_day
                                ),
                                interval = "confidence",
                                level = 0.95
                            )
                        ) %>%
                            mutate(
                                y_predict = case_when(
                                    y$estimate[2] < 0 ~ y_predict.lwr,
                                    TRUE ~ y_predict.upr
                                )
                            )
                    } else {
                        result <- NA
                    }

                    return(result)
                }
            ),
            maxday = purrr::map2_int(
                .x = raw_ext,
                .y = predict,
                function(x, y) {
                    max_test_day <- max(
                        x$day
                    )

                    if (!is.data.frame(y)) {
                        result <- max_test_day
                    } else if (y$y_predict[nrow(y)] / y$y_predict[1] < 1) {
                        result <- y %>%
                            filter(y_predict >= x$limit_lwr)
                    } else {
                        result <- y %>%
                            filter(y_predict <= x$limit_upr)
                    }

                    if (is.data.frame(result)) {
                        result <- case_when(
                            nrow(result) == 0 ~ 0,
                            TRUE ~ max(result$day)
                        )
                    }

                    if (result > max_test_day) {
                        result <- max_test_day
                    }

                    return(result)
                }
            )
        )

    return(stability)
}


# ! old version

data2stability <- function(data, perc_allowable_drift) {
    stability <- data %>%
        tidyr::nest(
            data = c(
                "day",
                "replicate",
                "y"
            )
        ) %>%
        mutate(
            data = purrr::map(
                data,
                function(x) {
                    ep25_acceptance_criteria <- as.numeric(perc_allowable_drift)

                    ymean <- x %>%
                        group_by(day) %>%
                        summarize(
                            mean = mean(y)
                        ) %>%
                        arrange(day)

                    result <- cbind(
                        x,
                        y0 = ymean$mean[1]
                    ) %>%
                        mutate(
                            limit_upr = y0 * (1 + ep25_acceptance_criteria / 100),
                            limit_lwr = y0 * (1 - ep25_acceptance_criteria / 100)
                        )

                    return(result)
                }
            ),
            fit = purrr::map(
                data,
                function(x) {
                    result <- lm(
                        formula = "y ~ day",
                        data = x
                    )

                    return(result)
                }
            ),
            fit_summary = purrr::map(
                fit,
                function(x) {
                    result <- summary(x)
                    return(result)
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
            predict = purrr::map2(
                .x = fit,
                .y = fit_summary_tidy,
                function(x, y) {
                    if (y$overall_p_value[1] < 0.05) {
                        range_day <- 1:300

                        result <- data.frame(
                            day = range_day,
                            y_predict = predict(
                                x,
                                newdata = data.frame(
                                    day = range_day
                                ),
                                interval = "confidence",
                                level = 0.95
                            )
                        ) %>%
                            mutate(
                                y_predict = case_when(
                                    y$estimate[2] < 0 ~ y_predict.lwr,
                                    TRUE ~ y_predict.upr
                                )
                            )
                    } else {
                        result <- NA
                    }

                    return(result)
                }
            ),
            maxday = purrr::map2(
                .x = data,
                .y = predict,
                function(x, y) {
                    max_test_day <- max(
                        x$day
                    )

                    if (!is.data.frame(y)) {
                        result <- max_test_day
                    } else if (y$y_predict[nrow(y)] / y$y_predict[1] < 1) {
                        result <- y %>%
                            filter(y_predict >= x$limit_lwr)
                    } else {
                        result <- y %>%
                            filter(y_predict <= x$limit_upr)
                    }

                    if (is.data.frame(result)) {
                        result <- case_when(
                            nrow(result) == 0 ~ 0,
                            TRUE ~ max(result$day)
                        )
                    }

                    if (result > max_test_day) {
                        result <- max_test_day
                    }

                    return(result)
                }
            )
        )

    return(stability)
}
