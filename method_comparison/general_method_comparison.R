raw2sum <- function(df, loq) {
    df %>%
        mutate(
            summary = purrr::map(
                raw,
                function(raw) {
                    data.frame(
                        nrow = nrow(raw),
                        max_mref = max(raw$y_ref),
                        min_mref = min(raw$y_ref),
                        nrow_smaller_than_loq = length(
                            raw$y_test[raw$y_test < loq]
                        )
                    ) %>%
                        return()
                }
            )
        ) %>%
        return()
}

raw2ft <- function(df, loq, ref_name, test_name, unit) {
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(df) {
                    df %>%
                        mutate(
                            y_test = case_when(
                                y_test < loq ~ -999,
                                TRUE ~ y_test
                            )
                        ) %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        ) %>%
                        arrange(
                            y_ref
                        ) %>%
                        mutate(
                            y_test = case_when(
                                y_test == -999 ~ "<LoQ",
                                TRUE ~ as.character(y_test)
                            )
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            y_ref = paste(
                                "Measurement Value of",
                                ref_name,
                                paste(
                                    "(",
                                    unit,
                                    ")",
                                    sep = ""
                                )
                            ),
                            y_test = paste(
                                "Measurement Value of",
                                test_name,
                                paste(
                                    "(",
                                    unit,
                                    ")",
                                    sep = ""
                                )
                            )
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = rep(1, 2),
                            width_visible = 7
                        ) %>%
                        footnote(
                            part = "header",
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

raw2plot <- function(df, ref_name, test_name, unit) {
    df %>%
        mutate(
            raw_plot = purrr::map(
                raw,
                function(x) {
                    x %>%
                        ggplot(
                            aes(
                                x = y_ref,
                                y = y_test
                            )
                        ) +
                        geom_point() +
                        labs(
                            x = paste(
                                "Measurement Value of",
                                ref_name,
                                paste(
                                    "(",
                                    unit,
                                    ")",
                                    sep = ""
                                )
                            ),
                            y = paste(
                                "Measurement Value of",
                                test_name,
                                paste(
                                    "(",
                                    unit,
                                    ")",
                                    sep = ""
                                )
                            )
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}

raw2_difference_plot <- function(df, ref_name, unit) {
    df %>%
        mutate(
            raw_difference_plot = purrr::map(
                raw,
                function(raw) {
                    raw %>%
                        mutate(
                            difference = y_test - y_ref
                        ) %>%
                        ggplot(
                            aes(
                                x = y_ref,
                                y = difference
                            )
                        ) +
                        geom_point() +
                        geom_hline(
                            yintercept = 0,
                            linetype = 2,
                            color = "red"
                        ) +
                        labs(
                            title = "Unit Difference Plot",
                            x = paste(
                                "Measurement Value of",
                                ref_name
                            ),
                            y = paste(
                                "Difference",
                                paste(
                                    "(",
                                    unit,
                                    ")",
                                    sep = ""
                                )
                            )
                        ) %>%
                        return()
                }
            )
        )
}

raw2perc_difference_plot <- function(df, ref_name) {
    df %>%
        mutate(
            raw_perc_difference_plot = purrr::map(
                raw,
                function(raw) {
                    raw %>%
                        mutate(
                            difference = y_test - y_ref,
                            perc_difference = 100 * difference / y_ref
                        ) %>%
                        ggplot(
                            aes(
                                x = y_ref,
                                y = perc_difference
                            )
                        ) +
                        geom_point() +
                        geom_hline(
                            yintercept = 0,
                            linetype = 2,
                            color = "red"
                        ) +
                        labs(
                            title = "%Difference Plot",
                            x = paste(
                                "Measurement Value of",
                                ref_name
                            ),
                            y = paste(
                                "%Difference"
                            )
                        ) %>%
                        return()
                }
            )
        )
}


raw2rosner_test_ft <- function(df, alpha) {
    df %>%
        mutate(
            rosner_test_ft = purrr::map(
                raw,
                function(x) {
                    result <- EnvStats::rosnerTest(
                        x$y_test,
                        k = floor(
                            nrow(x) * as.numeric(alpha)
                        ),
                        alpha = as.numeric(alpha)
                    )

                    result$all.stats %>%
                        flextable() %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        add_footer_lines(
                            values = c(
                                "i: 當前移除的資料筆數",
                                "Mean.i: 當前平均",
                                "SD.i: 當前SD",
                                "Value, Obs.Num, R.i+1: 當前懷疑的數值大小是[Value], 在資料中是第[Obs.Num]筆，距離平均[R.i+1]個標準差",
                                "lambda.i+1: 當數值為常態分佈且資料筆數為當前數量時，不應該(機率低於α)看到超過[lambda.i+1]個標準差的數值",
                                "Outlier: 判定是否為outlier"
                            )
                        ) %>%
                        return()
                }
            )
        )
}

raw2mcreg <- function(df, err_ratio, alpha, ref_name, test_name) {
    df %>%
        mutate(
            mcreg = purrr::map2(
                .x = raw,
                .y = regression_parm,
                function(raw, parm) {
                    mcr::mcreg(
                        x = raw$y_ref,
                        y = raw$y_test,
                        error.ratio = as.numeric(err_ratio),
                        alpha = as.numeric(alpha),
                        method.reg = parm$regression_method,
                        method.ci = parm$ci_method,
                        mref.name = ref_name,
                        mtest.name = test_name,
                        na.rm = TRUE
                    ) %>%
                        return()
                }
            )
        )
}

mcreg_coef2ft <- function(df, ncol_extra) {
    df %>%
        mutate(
            mcreg_coef_ft = purrr::map(
                mcreg_coef,
                function(coef) {
                    coef %>%
                        mutate_if(
                            is.numeric,
                            function(x) {
                                round(x, digits = 3) %>%
                                    return()
                            }
                        ) %>%
                        flextable() %>%
                        set_header_labels(
                            regression_method_full = "Regression Method",
                            item = "Item",
                            EST = "Value",
                            LCI = "95% CI Lower",
                            UCI = "95% CI Upper"
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
                                rep(2, ncol_extra),
                                rep(1, 4)
                            ),
                            width_visible = 7
                        ) %>%
                        return()
                }
            )
        )
}


mcreg2bias <- function(df, conc_crit) {
    df %>%
        ## pbias_best: 計算出最佳回歸的proportional percentage bias
        mutate(
            bias = purrr::map2(
                .x = mcreg,
                .y = if_best,
                function(mcreg, if_best) {
                    if (if_best) {
                        mcr::calcBias(
                            mcreg,
                            type = "proportional",
                            percent = TRUE,
                            x.levels = as.numeric(conc_crit),
                            alpha = 0.05
                        ) %>%
                            as.data.frame() %>%
                            select(-SE) %>%
                            return()
                    } else {
                        return(NA) # nolint: error.
                    }
                }
            )
        )
}
