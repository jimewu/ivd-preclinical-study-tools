mutate_raw2wide <- function(df) {
    df %>%
        mutate(
            raw_wide = purrr::map(
                raw,
                function(x) {
                    x %>%
                        pivot_wider(
                            names_from = replicate,
                            names_prefix = "replicate_",
                            values_from = y
                        ) %>%
                        return()
                }
            )
        )
}

mutate_raw_wide2ft <- function(df, ncol_extra, replicate, unit) {
    result <- df %>%
        mutate(
            raw_ft = purrr::map(
                raw_wide,
                function(raw) {
                    result <- raw %>%
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
                        rename_with(
                            ~ stringr::str_replace(.x, "replicate_", "Replicate "),
                            starts_with("replicate_")
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            rc = "RC",
                            y_ref = "Ref."
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        add_footer_lines(
                            values = paste(
                                "unit of replicates:",
                                unit,
                                sep = " "
                            )
                        )
                    # %>%
                    # width_ratio(
                    #     width_col_ratio = c(
                    #         rep(1, ncol_extra),
                    #         1.3,
                    #         rep(1, replicate)
                    #     )
                    # )

                    # if (ncol_extra == 2) {
                    #     result <- result %>%
                    #         merge_v(
                    #             part = "body",
                    #             j = 1:ncol_extra
                    #         )
                    # }

                    return(result)
                }
            )
        )
}

mutate_summary2ft <- function(df, ncol_extra, unit) {
    df %>%
        mutate(
            summary_ft = purrr::map(
                summary,
                function(x) {
                    result <- x %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(
                                    x,
                                    digits = 3
                                ) %>%
                                    return()
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            rc = "RC",
                            measured_value = "Measured Value",
                            expected_value = "Expected Value",
                            sd = "SD",
                            cv = "%CV",
                            var = "Var",
                            n = "Replicate",
                            weight = "Weight"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        )
                    # %>%
                    # width_ratio(
                    #     width_col_ratio = c(
                    #         rep(0.6, ncol_extra),
                    #         rep(1.3, 2),
                    #         rep(0.5, 3),
                    #         0.8,
                    #         0.7
                    #     ),
                    #     width_visible = 7.5
                    # )

                    col_unit <- which(
                        colnames(x) %in% c(
                            "measured_value",
                            "expected_value",
                            "sd"
                        )
                    )

                    result <- result %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = col_unit,
                            value = as_paragraph(
                                paste(
                                    "unit of values:",
                                    unit,
                                    sep = " "
                                )
                            ),
                            ref_symbols = letters[1]
                        )

                    if (ncol_extra > 0) {
                        result <- result %>%
                            merge_v(
                                part = "body",
                                j = 1:2
                            )
                    }

                    footnote_df <- data.frame(
                        i = 1,
                        j = c(
                            "measured_value",
                            "weight"
                        ),
                        value = c(
                            "Mean of replicates",
                            "Replicate / Var"
                        )
                    )

                    for (x in 1:nrow(footnote_df)) {
                        result <- result %>%
                            footnote(
                                part = "header",
                                i = footnote_df$i[x],
                                j = footnote_df$j[x],
                                value = as_paragraph(
                                    footnote_df$value[x]
                                ),
                                ref_symbols = letters[1 + x]
                            )
                    }

                    return(result)
                }
            )
        ) %>%
        return()
}

mutate_summary2sd_cv_plot <- function(df, unit) {
    df %>%
        mutate(
            sd_plot = purrr::map(
                summary,
                function(x) {
                    result <- ggplot(
                        x,
                        aes(
                            x = measured_value,
                            y = sd
                        )
                    ) +
                        geom_point() +
                        labs(
                            x = paste(
                                "Measured",
                                unit,
                                sep = " "
                            ),
                            y = paste(
                                "SD",
                                unit
                            )
                        ) %>%
                        return()
                }
            ),
            cv_plot = purrr::map(
                summary,
                function(x) {
                    result <- ggplot(
                        x,
                        aes(
                            x = measured_value,
                            y = cv
                        )
                    ) +
                        geom_point() +
                        labs(
                            x = paste(
                                "Measured",
                                unit,
                                sep = " "
                            ),
                            y = "%CV"
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}


mutate_fit_summary_tidy2ft <- function(df, ncol_extra) {
    result <- df %>%
        mutate(
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
                            term = rep(
                                c(
                                    "(Intercept)",
                                    "Expected Value"
                                ),
                                nrow(.) / 2
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
                        )

                    if (ncol_extra > 0) {
                        result <- result %>%
                            arrange(
                                desc(1),
                                2
                            )
                    }

                    result <- result %>%
                        flextable() %>%
                        set_header_labels(
                            matrix = "Sample Type",
                            group = "Group",
                            term = "Item",
                            estimate = "Value",
                            std.error = "Standard Error",
                            overall_p_value = "Regression p-value",
                            if_significant = "Significant (Y) or Not (N)?"
                        ) %>%
                        merge_v(
                            part = "body",
                            j = 4 + ncol_extra
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                rep(
                                    1.2,
                                    ncol_extra
                                ),
                                1.5,
                                rep(1, 4)
                            )
                        )

                    if (ncol_extra > 0) {
                        result <- result %>%
                            add_header_row(
                                top = TRUE,
                                values = c(
                                    "Sample Type",
                                    "Group",
                                    "Regression"
                                ),
                                colwidths = c(1, 1, 5)
                            ) %>%
                            merge_v(
                                part = "header"
                            ) %>%
                            merge_v(
                                part = "body",
                                j = 1:ncol_extra
                            )
                    }

                    for (x in 1:(nrow(x) / 2)) {
                        result <-
                            result %>%
                            merge_at(
                                part = "body",
                                j = 5 + ncol_extra,
                                i = c(
                                    2 * x - 1,
                                    2 * x
                                )
                            )
                    }


                    return(result)
                }
            )
        )
}


mutate_merge2ft <- function(
    df,
    ncol_extra) {
    result <- df %>%
        mutate(
            merge_ft = purrr::map(
                merge,
                function(x) {
                    result <- x %>%
                        select(
                            -sd, -cv, -var, -n, -weight
                        ) %>%
                        relocate(
                            expected_value,
                            .before = measured_value
                        ) %>%
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
                            matrix = "Sample Type",
                            group = "Group",
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
                        width_ratio(
                            width_col_ratio = rep(
                                1, 5 + ncol_extra
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = 1:4 + ncol_extra,
                            value = as_paragraph(
                                paste(
                                    "unit of values:",
                                    var$dev_unit3,
                                    sep = " "
                                )
                            ),
                            ref_symbols = letters[1]
                        )

                    if (ncol_extra > 0) {
                        result <- result %>%
                            merge_v(
                                part = "body",
                                j = 1:ncol_extra
                            )
                    }

                    header_footnote_df <- data.frame(
                        i = c(1, 1, 1, 1),
                        j = c(1, 3, 4, 5),
                        value = c(
                            "Mean of replicates",
                            "Predicted Value is derived from applying the Expected Value to respective regression formula.",
                            "Measured Value - Predicted Value",
                            "100 * Deviation / (Predicted Value)"
                        )
                    )

                    for (x in 1:nrow(header_footnote_df)) {
                        result <- result %>%
                            footnote(
                                part = "header",
                                i = header_footnote_df$i[x],
                                j = header_footnote_df$j[x] + ncol_extra,
                                value = as_paragraph(
                                    header_footnote_df$value[x]
                                ),
                                ref_symbols = letters[1 + x]
                            )
                    }

                    return(result)
                }
            )
        )
}
