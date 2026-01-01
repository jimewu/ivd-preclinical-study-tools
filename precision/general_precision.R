mutate_raw2ft <- function(df, dev_unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(x) {
                    nreplicate <- x$replicate %>%
                        factor() %>%
                        levels() %>%
                        length()

                    result <- x %>%
                        tidyr::pivot_wider(
                            names_from = "replicate",
                            names_prefix = "Replicate ",
                            values_from = "y"
                        ) %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                result <- round(
                                    x,
                                    digits = 3
                                )
                                return(result)
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            sample = "Sample",
                            site = "Site",
                            lot = "Lot",
                            operator = "Operator",
                            day = "Day",
                            run = "Run"
                        ) %>%
                        merge_v(
                            part = "body",
                            j = 1:2
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                rep(1, 3),
                                rep(1.2, nreplicate)
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = 3 + 1:nreplicate,
                            value = as_paragraph(
                                paste(
                                    "unit of values:",
                                    dev_unit
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

mutate_raw2plot <- function(df, analyte_name, dev_unit) {
    df %>%
        mutate(
            raw_plot = purrr::map(
                raw,
                function(x) {
                    x %>%
                        ggplot(
                            aes(
                                x = day,
                                y = y,
                                color = run
                            )
                        ) +
                        geom_point() +
                        labs(
                            x = "Day",
                            y = paste(
                                analyte_name,
                                " (",
                                dev_unit,
                                ")",
                                sep = ""
                            ),
                            title = "Measurement results",
                            color = "Run"
                        ) +
                        theme(
                            plot.title = element_text(hjust = 0.5)
                        ) +
                        facet_wrap(
                            ~sample,
                            scales = "free",
                            ncol = 1
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}

mutate_VCA2df <- function(df) {
    df %>%
        mutate(
            VCA_df = purrr::map(
                VCA,
                function(x) {
                    x %>%
                        as.matrix() %>%
                        as.data.frame() %>%
                        cbind(
                            item = tools::toTitleCase(
                                rownames(.)
                            ),
                            .
                        ) %>%
                        return()
                }
            )
        )
}

mutate_VCA_df2ft <- function(df) {
    df %>%
        mutate(
            VCA_ft = purrr::map(
                VCA_df,
                function(x) {
                    x %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                result <- x %>%
                                    round(
                                        digits = 3
                                    )

                                return(result)
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            sample = "Sample",
                            item = "Item"
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        merge_v(
                            part = "body",
                            j = 1
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                1.73,
                                1.37,
                                0.4,
                                0.72,
                                rep(0.55, 2),
                                0.72,
                                0.55,
                                0.72
                            )
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}
