mutate_raw2ft <- function(df, test_name, unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(raw) {
                    result <- raw %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        )

                    result <- result %>%
                        flextable() %>%
                        set_header_labels(
                            lot = "Device Lot",
                            day = "Day",
                            y = paste(
                                "Measurement Value of",
                                test_name
                            )
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = rep(1, ncol(result)),
                            width_visible = 7
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = ncol(result),
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
        ) %>%
        return()
}

mutate_raw2plot <- function(df, test_name, unit) {
    df %>%
        mutate(
            raw_plot = purrr::map(
                raw,
                function(raw) {
                    # 1. 原本的散佈圖 (Jitter Plot)
                    p1 <- raw %>%
                        ggplot(
                            aes(
                                x = day,
                                y = y,
                                color = lot
                            )
                        ) +
                        geom_jitter() +
                        labs(
                            x = "Day",
                            y = paste(
                                "Measurement Value of ",
                                test_name,
                                paste(
                                    sep = "",
                                    "(",
                                    unit,
                                    ")"
                                )
                            ),
                            color = "Lot",
                            title = "Trend over Day"
                        )

                    # 2. 新增的分佈圖 (Distribution Plot)
                    # 使用 geom_density 呈現 y 的分佈，並以 lot 區分顏色
                    p2 <- raw %>%
                        ggplot(
                            aes(
                                x = y,
                                fill = lot, # 使用 fill 填滿顏色較易觀察分佈
                                color = lot
                            )
                        ) +
                        geom_density(alpha = 0.5) + # 設定透明度
                        labs(
                            x = paste(
                                "Measurement Value of ",
                                test_name,
                                paste(
                                    sep = "",
                                    "(",
                                    unit,
                                    ")"
                                )
                            ),
                            y = "Density",
                            fill = "Lot",
                            color = "Lot",
                            title = "Distribution of Y"
                        )

                    # 3. 組裝圖表並回傳
                    # 使用 patchwork 語法：
                    # "+" 代表左右並排, "/" 代表上下堆疊
                    # plot_layout(guides = "collect") 可以合併相同的圖例
                    combined_plot <- p1 / p2 + plot_layout(guides = "collect")

                    return(combined_plot)
                }
            )
        ) %>%
        return()
}


mutate_lob_summary2ft <- function(df, unit) {
    df %>%
        mutate(
            lob_summary_ft = purrr::map(
                lob_summary,
                function(df) {
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
                            lot = "Device Lot",
                            shapiro_p = "p-value of Shapiro-Wilk Test",
                            if_norm = "Normal Distribution",
                            rank_pos = "Rank Position",
                            mean_blank = "Mean",
                            sd_blank = "SD",
                            cp = "Cp",
                            lot_lob = "Lot LoB",
                            final_lob = "Final LoB"
                        ) %>%
                        merge_v(
                            part = "body"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = rep(
                                1, ncol(df)
                            ),
                            width_visible = 7
                        ) %>%
                        footnote(
                            part = "header",
                            j = "shapiro_p",
                            value = as_paragraph(
                                "p<0.05: 表示跟常態分佈有顯著差異(此時認為非常態分佈)"
                            ),
                            ref_symbols = letters[1]
                        ) %>%
                        footnote(
                            part = "header",
                            j = (ncol(df) - 1):ncol(df),
                            value = as_paragraph(
                                paste(
                                    "unit of value:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[2]
                        ) %>%
                        footnote(
                            part = "header",
                            j = (ncol(df) - 1):ncol(df),
                            value = as_paragraph(
                                "LoB: limit of blank"
                            ),
                            ref_symbols = letters[3]
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}
