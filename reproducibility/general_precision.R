# lib_general_precision.R

# 1. 表格生成函數 (穩定版)
mutate_raw2ft <- function(df, dev_unit, group_var = "run") {
    
    # 準備顯示標籤 (例如 "site" -> "Site")
    group_label <- tools::toTitleCase(group_var)
    
    df %>%
        mutate(
            raw_ft = purrr::map(
                raw,
                function(x) {
                    # 1. 轉置資料 (Pivot)
                    wide_df <- x %>%
                        tidyr::pivot_wider(
                            names_from = "replicate",
                            names_prefix = "Replicate ",
                            values_from = "y"
                        ) 
                    
                    # 2. 強制欄位排序 (Sample -> Group -> Day -> Replicates)
                    # 這能確保表格邏輯正確，無論 Excel 欄位順序為何
                    wide_df <- wide_df %>%
                        select(
                            sample, 
                            all_of(group_var), 
                            day, 
                            starts_with("Replicate")
                        )

                    # 3. 準備表頭對照表 (使用 List)
                    # 這樣做比 do.call 穩定很多
                    header_map <- list(
                        sample = "Sample",
                        day = "Day"
                    )
                    header_map[[group_var]] <- group_label # 動態加入 (例如 site="Site")

                    # 4. 產生 Flextable
                    ft <- wide_df %>%
                        # 四捨五入數值
                        mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
                        flextable() %>%
                        # 設定表頭 (直接傳入 list，不使用 do.call)
                        set_header_labels(values = header_map) %>%
                        # 合併重複的文字 (Sample 和 Group)
                        merge_v(part = "body", j = 1:2) %>%
                        align(align = "center", part = "all") %>%
                        # !關鍵修正!: 使用 autofit 自動調整寬度，避免計算錯誤
                        autofit() %>%
                        # 註解: 固定加在 "Day" 欄位 (第3欄)，因為我們有強制 select 排序，所以是安全的
                        footnote(
                            part = "header", i = 1, j = 3,
                            value = as_paragraph(paste("unit:", dev_unit)),
                            ref_symbols = letters[1]
                        )
                    
                    return(ft)
                }
            )
        ) %>%
        return()
}

# 2. 繪圖函數
mutate_raw2plot <- function(df, analyte_name, dev_unit, group_var = "run") {
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
                                color = .data[[group_var]] # 動態指定顏色
                            )
                        ) +
                        geom_point() +
                        labs(
                            x = "Day",
                            y = paste(analyte_name, " (", dev_unit, ")", sep = ""),
                            title = "Measurement results",
                            color = group_label
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

# 3. VCA 結果整理
mutate_VCA2df <- function(df) {
    df %>%
        mutate(
            VCA_df = purrr::map(
                VCA,
                function(x) {
                    if(is.null(x)) return(data.frame(item = "Error", other = "Calculation Failed"))
                    
                    x %>%
                        as.matrix() %>%
                        as.data.frame() %>%
                        cbind(item = tools::toTitleCase(rownames(.)), .) %>%
                        return()
                }
            )
        )
}

# 4. VCA 表格輸出 (穩定版)
mutate_VCA_df2ft <- function(df) {
    df %>%
        mutate(
            VCA_ft = purrr::map(
                VCA_df,
                function(x) {
                    x %>%
                        mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
                        flextable() %>%
                        set_header_labels(sample = "Sample", item = "Item") %>%
                        align(align = "center", part = "all") %>%
                        merge_v(part = "body", j = 1) %>%
                        autofit() %>% # 同樣改用 autofit
                        return()
                }
            )
        ) %>%
        return()
}