mutate_raw2ft <- function(df, unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(x) {
                    df <- x %>%
                        tidyr::pivot_wider(
                            names_from = "replicate",
                            names_prefix = "Replicate ",
                            values_from = "y"
                        ) %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        ) %>%
                        relocate(
                            conc,
                            .before = "day"
                        ) %>%
                        arrange(
                            lot,
                            conc,
                            day
                        )

                    ft <- df %>%
                        flextable() %>%
                        set_header_labels(
                            lot = "Device Lot",
                            day = "Day",
                            conc = "Analyte Level"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                rep(
                                    1,
                                    ncol(df)
                                )
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = c(
                                2,
                                4:ncol(df)
                            ),
                            value = as_paragraph(
                                paste(
                                    "unit of values:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[1]
                        )

                    return(ft)
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
                    result <- raw %>%
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
                        ) +
                        facet_wrap(~conc)

                    return(result)
                }
            )
        ) %>%
        return()
}

mutate_raw2total_error <- function(df, loq_type, allowable_perc_te, te_method = "Westgard model") {
    df %>%
        mutate(
            total_error = purrr::map(
                raw,
                function(x) {
                    result <- x %>%
                        group_by(conc) %>%
                        summarize(
                            n = n(),
                            mean = mean(y),
                            sd = sd(y)
                        ) %>%
                        mutate(
                            bias = mean - conc
                        )

                    if (te_method == "Westgard model") {
                        result <- result %>%
                            mutate(
                                total_error = abs(bias) + 2 * sd
                            )
                    } else {
                        result <- result %>%
                            mutate(
                                total_error = sqrt(
                                    bias^2 + sd^2
                                )
                            )
                    }

                    result <- result %>%
                        mutate(
                            perc_total_error = 100 * total_error / conc
                        ) %>%
                        arrange(
                            conc
                        )

                    lot_loq <- result %>%
                        filter(perc_total_error <= allowable_perc_te) %>%
                        .$conc

                    if (loq_type == "lloq") {
                        lot_loq <- min(lot_loq)
                    } else if (loq_type == "hloq") {
                        lot_loq <- max(lot_loq)
                    } else {
                        lot_loq <- "LoQ Type Error"
                    }

                    result <- result %>%
                        cbind(
                            .,
                            lot_loq = lot_loq
                        )

                    return(result)
                }
            )
        ) %>%
        select(-raw) %>%
        tidyr::unnest(total_error) %>%
        tidyr::nest(
            total_error = colnames(.)
        ) %>%
        mutate(
            total_error = purrr::map(
                total_error,
                function(x) {
                    if (loq_type == "lloq") {
                        result <- x %>%
                            mutate(
                                final_loq = .$lot_loq %>% max()
                            )
                    } else if (loq_type == "hloq") {
                        result <- x %>%
                            mutate(
                                final_loq = .$lot_loq %>% min()
                            )
                    } else {
                        result <- x %>%
                            mutate(
                                final_loq = "LoQ Type Error"
                            )
                    }

                    return(result)
                }
            )
        ) %>%
        return()
}


mutate_total_error2ft <- function(df, unit, te_method = "Westgard model") {
    df %>%
        mutate(
            total_error_ft = purrr::map(
                total_error,
                function(x) {
                    result <- x %>%
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
                            conc = "Analyte Level",
                            mean = "Mean (X)",
                            sd = "SD",
                            bias = "Bias",
                            total_error = "TE",
                            perc_total_error = "%TE",
                            lot_loq = "Lot LoQ",
                            final_loq = "Final LoQ"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                1,
                                1.8,
                                0.8,
                                rep(1, 3),
                                1.3,
                                rep(1, 3)
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = c(2, 4:7, 9:10),
                            value = as_paragraph(
                                paste(
                                    "unit of values:",
                                    unit
                                )
                            ),
                            ref_symbols = letters[1]
                        ) %>%
                        merge_v(
                            part = "body",
                            j = c(1, ncol(x))
                        )

                    if (te_method == "Westgard model") {
                        te_formula <- "|Bias| + 2 * SD"
                    } else {
                        te_formula <- "(Bias^2 + SD^2)^0.5"
                    }

                    header_footnote_df <- data.frame(
                        i = 1,
                        j = c(6, 7),
                        value = c(
                            "X - R",
                            te_formula
                        )
                    )

                    for (each in 1:nrow(header_footnote_df)) {
                        result <- result %>%
                            footnote(
                                part = "header",
                                i = header_footnote_df$i[each],
                                j = header_footnote_df$j[each],
                                value = as_paragraph(
                                    header_footnote_df$value[each]
                                ),
                                ref_symbols = letters[each + 1]
                            )
                    }

                    merge_by_dev_lot <- x %>%
                        group_by(lot) %>%
                        summarize(n = n()) %>%
                        mutate(
                            start = 1 + dplyr::lag(n, n = 1),
                            end = n + dplyr::lag(n, n = 1)
                        )

                    merge_by_dev_lot$start[1] <- 1
                    merge_by_dev_lot$end[1] <- merge_by_dev_lot$n[1]

                    for (each in 1:nrow(merge_by_dev_lot)) {
                        result <- result %>%
                            merge_at(
                                j = ncol(x) - 1,
                                i = merge_by_dev_lot$start[each]:merge_by_dev_lot$end[each]
                            )
                    }

                    return(result)
                }
            )
        ) %>%
        return()
}
