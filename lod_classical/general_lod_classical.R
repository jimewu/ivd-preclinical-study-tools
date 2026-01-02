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
                            lot = "Device Lot",
                            day = "Day",
                            conc = "Sample Conc.",
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
                                ncol(raw) - 1,
                                ncol(raw)
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

mutate_raw2lot_summary <- function(df) {
    df %>%
        mutate(
            lot_summary = purrr::map(
                raw,
                function(raw) {
                    raw %>%
                        group_by(
                            conc,
                            ntest
                        ) %>%
                        summarize(
                            ni = length(y),
                            sdi = sd(y)
                        ) %>%
                        ungroup() %>%
                        return()
                }
            )
        ) %>%
        return()
}

mutate_lot_summary2lot_sd_lod <- function(df, lob) {
    df %>%
        mutate(
            lot_sd_lod = purrr::map(
                lot_summary,
                function(summary) {
                    summary %>%
                        mutate(
                            numerator = (ni - 1) * sdi^2,
                            denominator = (ni - 1)
                        ) %>%
                        group_by(ntest) %>%
                        summarize(
                            sd_lot = sqrt(
                                sum(numerator) / sum(denominator)
                            ),
                            ## nconc_lot: 不同濃度種類數(相當於CLSI中的J)
                            nconc_lot = length(
                                levels(
                                    factor(conc)
                                )
                            )
                        ) %>%
                        mutate(
                            cp = 1.645 / (1 - 1 / (4 * (ntest - nconc_lot))),
                            lot_lod = as.numeric(lob) + cp * sd_lot
                        ) %>%
                        select(-ntest) %>%
                        return()
                }
            )
        ) %>%
        return()
}


mutate_lod_summary2ft <- function(df, unit) {
    df %>%
        mutate(
            summary_ft = purrr::map(
                summary,
                function(summary) {
                    summary %>%
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
                            sample = "Sample Conc.",
                            ni = "n",
                            sdi = "Sample SD (SDi)",
                            sd_lot = "Lot SD (SDL)",
                            cp = "Cp",
                            lot_lod = "Lot LoD",
                            final_lod = "Final LoD"
                        ) %>%
                        flextable::compose(
                            part = "header",
                            j = "sdi",
                            value = as_paragraph(
                                "SD",
                                as_sub("i")
                            )
                        ) %>%
                        flextable::compose(
                            part = "header",
                            j = "cp",
                            value = as_paragraph(
                                "C",
                                as_sub("p")
                            )
                        ) %>%
                        merge_v(
                            part = "body",
                            j = c(1, 5:8)
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        align(
                            align = "right",
                            part = "footer"
                        ) %>%
                        width_ratio(
                            width_col_ratio = rep(
                                1, ncol(summary)
                            ),
                            width_visible = 7
                        ) %>%
                        footnote(
                            part = "header",
                            j = c(2, 4, 5, 7, 8),
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
