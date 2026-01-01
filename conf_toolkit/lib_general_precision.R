mutate_raw2ft <- function(df, dev_unit, group_var = "run") {
    # 自動將 group_var 首字大寫作為表格 Header (例如 site -> Site)
    group_label <- tools::toTitleCase(group_var)

    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(x) {
                    nreplicate <- x$replicate %>%
                        factor() %>%
                        levels() %>%
                        length()

                    # 建立動態 header 設定
                    header_settings <- list(
                        sample = "Sample",
                        day = "Day"
                    )
                    # 動態加入分組變數的標籤 (例如 run = "Run" 或 site = "Site")
                    header_settings[[group_var]] <- group_label

                    result <- x %>%
                        tidyr::pivot_wider(
                            names_from = "replicate",
                            names_prefix = "Replicate ",
                            values_from = "y"
                        ) %>%
                        mutate_if(
                            is.numeric,
                            function(x) round(x, digits = 3)
                        ) %>%
                        flextable() %>%
                        # 使用 do.call 將動態 list 傳入 set_header_labels
                        do.call(set_header_labels, args = list(.x = ., values = header_settings)) %>%
                        merge_v(
                            part = "body",
                            j = 1:2 # 合併 Sample 和 Group Variable
                        ) %>%
                        align(
                            align = "center",
                            part = "all"
                        ) %>%
                        width_ratio(
                            width_col_ratio = c(
                                rep(1, 3), # Adjust based on cols
                                rep(1.2, nreplicate)
                            )
                        ) %>%
                        footnote(
                            part = "header",
                            i = 1,
                            j = 3 + 1:nreplicate,
                            value = as_paragraph(paste("unit of values:", dev_unit)),
                            ref_symbols = letters[1]
                        ) %>%
                        return()
                }
            )
        ) %>%
        return()
}

mutate_raw2plot <- function(df, analyte_name, dev_unit, group_var = "run") {
    # 自動將 group_var 首字大寫作為圖例標題
    group_label <- tools::toTitleCase(group_var)

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
                                color = .data[[group_var]] # 動態選擇顏色分組欄位
                            )
                        ) +
                        geom_point() +
                        labs(
                            x = "Day",
                            y = paste(analyte_name, " (", dev_unit, ")", sep = ""),
                            title = "Measurement results",
                            color = group_label # 動態圖例名稱
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

# 下方函數維持不變
mutate_VCA2df <- function(df) {
    df %>%
        mutate(
            VCA_df = purrr::map(
                VCA,
                function(x) {
                    x %>%
                        as.matrix() %>%
                        as.data.frame() %>%
                        cbind(item = tools::toTitleCase(rownames(.)), .) %>%
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
                        mutate_if(is.numeric, function(x) round(x, digits = 3)) %>%
                        flextable() %>%
                        set_header_labels(sample = "Sample", item = "Item") %>%
                        align(align = "center", part = "all") %>%
                        merge_v(part = "body", j = 1) %>%
                        width_ratio(width_col_ratio = c(1.73, 1.37, 0.4, 0.72, rep(0.55, 2), 0.72, 0.55, 0.72)) %>%
                        return()
                }
            )
        ) %>%
        return()
}
