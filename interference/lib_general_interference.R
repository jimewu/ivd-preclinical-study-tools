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
                            ~ .x %>%
                                stringr::str_replace_all("_", " ") %>% # 步驟1: 底線替換為空格
                                stringr::str_to_title() # 步驟2: 首字母大寫
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
