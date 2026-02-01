# ============================================================
# EP07 Interference Analysis Shiny App
# ============================================================

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_load_package.R")

pkg_lst <- c(
    "shiny",
    "readxl",
    "dplyr",
    "tidyr",
    "ggplot2",
    "flextable",
    "purrr",
    "tibble",
    "officer"
)

pacman::p_load(char = pkg_lst)

source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")
source("lib_general_interference.R")

# --- 2. 使用者定義變數 ---
# 分頁標題
TAB_TITLE_1_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_2_RAW_PLOT <- "Raw Data Plot"
TAB_TITLE_3_PAIRED_DIFF <- "Paired Difference Analysis"
TAB_TITLE_4_DOSE_RESPONSE <- "Dose Response Analysis"
TAB_TITLE_5_REFERENCES <- "References"

# GitHub 模板連結
GITHUB_TEMPLATE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_interference.xlsx"

# 預設參數值
DEFAULT_UNIT <- "mg/dL"
DEFAULT_ACCEPTANCE_CRITERIA_DIFF <- 3
DEFAULT_ACCEPTANCE_CRITERIA_PERC_DIFF <- 5

# 下載檔案名稱前綴
DOWNLOAD_PREFIX_RAW_TABLE <- "raw_data_table"
DOWNLOAD_PREFIX_RAW_PLOT <- "raw_data_plot"
DOWNLOAD_PREFIX_PAIRED_DIFF <- "paired_difference"
DOWNLOAD_PREFIX_DOSE_RESPONSE <- "dose_response"
DOWNLOAD_PREFIX_REFERENCES <- "references"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("EP07 Interference Analysis"),
    sidebarLayout(
        sidebarPanel(
            # 3.1 GitHub 模板連結
            tags$a(
                href = GITHUB_TEMPLATE_LINK,
                target = "_blank",
                class = "btn btn-info",
                "下載 Excel Template"
            ),
            br(), br(),

            # 3.2 檔案上傳
            fileInput(
                "file_upload",
                "上傳 Excel 檔案 (Sheet: data)",
                accept = c(".xlsx")
            ),

            # 3.3 參數輸入
            textInput(
                "input_unit",
                "單位 (Unit)",
                value = DEFAULT_UNIT,
                placeholder = "例如：mg/dL, mmol/L, U/L"
            ),
            helpText(
                "請輸入測量值的單位，此單位將顯示於所有圖表和表格中。",
                "常見單位：mg/dL (毫克/分升)、mmol/L (毫莫耳/升)、U/L (單位/升)"
            ),
            numericInput(
                "input_acceptance_criteria_diff",
                "允收Difference (Acceptance Criteria - Difference)",
                value = DEFAULT_ACCEPTANCE_CRITERIA_DIFF,
                min = 0,
                step = 0.1
            ),
            helpText(
                "設定可接受的絕對差異值上限。",
                "此值用於判斷干擾物質的最大可接受濃度。",
                "例如：3 表示差異不超過 3 個單位"
            ),
            numericInput(
                "input_acceptance_criteria_perc_diff",
                "允收%Difference (Acceptance Criteria - % Difference)",
                value = DEFAULT_ACCEPTANCE_CRITERIA_PERC_DIFF,
                min = 0,
                max = 100,
                step = 0.1
            ),
            helpText(
                "設定可接受的百分比差異值上限。",
                "此值用於判斷干擾物質的最大可接受濃度。",
                "例如：5 表示差異不超過 5%"
            ),

            # 3.4 執行按鈕
            hr(),
            actionButton(
                "run_analysis",
                "執行分析",
                class = "btn-primary"
            )
        ),
        mainPanel(
            tabsetPanel(
                # Tab 1: 原始資料表
                tabPanel(
                    TAB_TITLE_1_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # Tab 2: 原始資料圖
                tabPanel(
                    TAB_TITLE_2_RAW_PLOT,
                    br(),
                    downloadButton("dl_raw_plot", "下載圖表 (.png)"),
                    br(), br(),
                    plotOutput("out_raw_plot", height = "600px")
                ),

                # Tab 3: Paired Difference 分析結果
                tabPanel(
                    TAB_TITLE_3_PAIRED_DIFF,
                    br(),
                    uiOutput("out_paired_diff_tables")
                ),

                # Tab 4: Dose Response 分析結果
                tabPanel(
                    TAB_TITLE_4_DOSE_RESPONSE,
                    br(),
                    downloadButton("dl_dose_response_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_dose_response_ft")
                ),

                # Tab 5: References
                tabPanel(
                    TAB_TITLE_5_REFERENCES,
                    br(),
                    downloadButton("dl_ref_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_ref_ft")
                )
            )
        )
    )
)

# --- 4. Server 邏輯 ---
server <- function(input, output, session) {
    # 4.1 核心分析邏輯
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 驗證檔案格式
        validate(
            need(input$file_upload, "請上傳 Excel 檔案"),
            need(
                tools::file_ext(input$file_upload$name) == "xlsx",
                "請上傳 .xlsx 格式的檔案"
            )
        )

        # 驗證參數合理性
        validate(
            need(
                input$input_acceptance_criteria_diff > 0,
                "差異接受標準必須大於 0"
            ),
            need(
                input$input_acceptance_criteria_perc_diff > 0 &&
                    input$input_acceptance_criteria_perc_diff <= 100,
                "百分比差異接受標準必須介於 0 到 100 之間"
            )
        )

        # 準備參數
        params <- list(
            unit = input$input_unit,
            acceptance_criteria_diff = input$input_acceptance_criteria_diff,
            acceptance_criteria_perc_diff = input$input_acceptance_criteria_perc_diff
        )

        # 讀取檔案
        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # 驗證 sheet 是否存在
        tryCatch(
            {
                sheets <- readxl::excel_sheets(file_path)
                validate(
                    need("data" %in% sheets, "Excel 檔案中找不到 'data' 工作表")
                )
            },
            error = function(e) {
                validate(need(FALSE, paste("讀取檔案時發生錯誤：", e$message)))
            }
        )

        # --- Stage 1: Import ---
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            group_by(
                analyte_conc,
                interferent,
                interferent_conc
            ) %>%
            mutate(replicate = row_number()) %>%
            ungroup() %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate_raw2wide() %>%
            mutate_raw_wide2ft(
                ncol_extra = 1,
                replicate = max(replicate),
                unit = params$unit
            ) %>%
            mutate(
                raw_plot = purrr::map(
                    raw,
                    function(raw) {
                        raw <- raw %>%
                            mutate(
                                label_x = paste(
                                    "Analyte Conc.:",
                                    analyte_conc
                                )
                            ) %>%
                            arrange(
                                interferent,
                                analyte_conc
                            )

                        result <- raw %>%
                            ggplot(
                                aes(
                                    x = interferent_conc,
                                    y = y,
                                    color = factor(replicate)
                                )
                            ) +
                            geom_point(
                                alpha = 0.7
                            ) +
                            facet_wrap(
                                interferent ~ label_x,
                                scales = "free_x"
                            ) +
                            labs(
                                x = "Interferent Conc.",
                                y = paste(
                                    "Measured Analyte Values (",
                                    params$unit,
                                    ")",
                                    sep = ""
                                ),
                                color = "Replicate"
                            )
                    }
                )
            )

        # --- Stage 2: Analysis ---
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = -analyte_conc
            ) %>%
            mutate(
                paired_difference = purrr::map(
                    raw,
                    function(x) {
                        x %>%
                            group_by(
                                interferent,
                                interferent_conc
                            ) %>%
                            summarize(
                                mean = mean(y),
                                .groups = "drop"
                            ) %>%
                            group_by(interferent) %>%
                            mutate(
                                mean_con = mean[1],
                                diff = mean - mean_con,
                                perc_diff = 100 * diff / mean_con
                            ) %>%
                            ungroup() %>%
                            select(-mean_con) %>%
                            return()
                    }
                )
            ) %>%
            mutate(
                paired_difference_ft = purrr::map2(
                    .x = paired_difference,
                    .y = analyte_conc,
                    function(x, y) {
                        result_df <- x %>%
                            cbind(
                                analyte_conc = y,
                                .
                            ) %>%
                            mutate_if(
                                is.numeric,
                                function(x) {
                                    round(x, digits = 3) %>%
                                        return()
                                }
                            )

                        result_ft <- result_df %>%
                            flextable() %>%
                            set_header_labels(
                                analyte_conc = "Analyte\nConc.",
                                interferent = "Name",
                                interferent_conc = "Conc.",
                                mean = "Mean",
                                diff = "Difference",
                                perc_diff = "%Difference"
                            ) %>%
                            add_header_row(
                                top = TRUE,
                                values = c(
                                    "Analyte\nConc.",
                                    "Interferent",
                                    "Result"
                                ),
                                colwidths = c(
                                    1,
                                    2,
                                    3
                                )
                            ) %>%
                            merge_v(
                                part = "header"
                            ) %>%
                            merge_v(
                                part = "body",
                                j = c(
                                    "analyte_conc",
                                    "interferent"
                                )
                            ) %>%
                            align(
                                align = "center",
                                part = "all"
                            ) %>%
                            footnote(
                                part = "header",
                                i = 1,
                                j = c(
                                    "analyte_conc"
                                ),
                                value = as_paragraph(
                                    paste(
                                        "unit of value:",
                                        params$unit,
                                        sep = " "
                                    )
                                ),
                                ref_symbols = letters[1]
                            ) %>%
                            footnote(
                                part = "header",
                                i = 2,
                                j = c(
                                    "mean",
                                    "diff"
                                ),
                                value = as_paragraph(
                                    paste(
                                        "unit of value:",
                                        params$unit,
                                        sep = " "
                                    )
                                ),
                                ref_symbols = letters[2]
                            ) %>%
                            width_ratio(
                                width_col_ratio = c(
                                    1,
                                    2,
                                    2,
                                    rep(1, 3)
                                ),
                                width_visible = 7
                            )

                        return(result_ft)
                    }
                )
            )

        # --- Stage 3: Dose Response ---
        stage3_dose_response <- stage2_analyze %>%
            select(
                analyte_conc,
                paired_difference
            ) %>%
            tidyr::unnest(
                paired_difference
            ) %>%
            tidyr::nest(
                paired_difference = c(
                    "interferent_conc",
                    "mean",
                    "diff",
                    "perc_diff"
                )
            ) %>%
            mutate(
                interferent_max_dose_by_diff = purrr::map_dbl(
                    paired_difference,
                    function(pair_diff) {
                        pair_diff <- pair_diff %>%
                            mutate(
                                diff = abs(diff),
                                perc_diff = abs(perc_diff)
                            )

                        if (
                            max(pair_diff$diff) <= params$acceptance_criteria_diff
                        ) {
                            result <- max(
                                as.numeric(pair_diff$interferent_conc)
                            )
                        } else {
                            # 1. 找出小於目標值，且最大的那一列
                            lower <- pair_diff %>%
                                filter(
                                    diff <= params$acceptance_criteria_diff
                                ) %>%
                                slice_max(diff, n = 1) %>%
                                slice_max(interferent_conc, n = 1)

                            # 2. 找出大於目標值，且最小的那一列
                            upper <- pair_diff %>%
                                filter(
                                    diff > params$acceptance_criteria_diff
                                ) %>%
                                slice_min(diff, n = 1) %>%
                                slice_min(interferent_conc, n = 1)

                            slope <- (
                                upper$diff - lower$diff
                            ) / (
                                upper$interferent_conc - lower$interferent_conc
                            )

                            result <- lower$interferent_conc + (params$acceptance_criteria_diff - lower$diff) / slope
                        }

                        return(result)
                    }
                )
            ) %>%
            mutate(
                interferent_max_dose_by_perc_diff = purrr::map_dbl(
                    paired_difference,
                    function(pair_diff) {
                        pair_diff <- pair_diff %>%
                            mutate(
                                diff = abs(diff),
                                perc_diff = abs(perc_diff)
                            )

                        if (
                            max(pair_diff$perc_diff) <= params$acceptance_criteria_perc_diff
                        ) {
                            result <- max(
                                as.numeric(pair_diff$interferent_conc)
                            )
                        } else {
                            # 1. 找出小於目標值，且最大的那一列
                            lower <- pair_diff %>%
                                filter(
                                    perc_diff <= params$acceptance_criteria_perc_diff
                                ) %>%
                                slice_max(perc_diff, n = 1) %>%
                                slice_max(interferent_conc, n = 1)

                            # 2. 找出大於目標值，且最小的那一列
                            upper <- pair_diff %>%
                                filter(
                                    perc_diff > params$acceptance_criteria_perc_diff
                                ) %>%
                                slice_min(perc_diff, n = 1) %>%
                                slice_min(interferent_conc, n = 1)

                            slope <- (
                                upper$perc_diff - lower$perc_diff
                            ) / (
                                upper$interferent_conc - lower$interferent_conc
                            )

                            result <- lower$interferent_conc + (params$acceptance_criteria_perc_diff - lower$perc_diff) / slope
                        }

                        return(result)
                    }
                )
            ) %>%
            select(-paired_difference) %>%
            relocate(
                interferent,
                .before = analyte_conc
            ) %>%
            arrange(
                interferent,
                analyte_conc
            ) %>%
            tidyr::nest(
                interferent_max_dose = everything()
            ) %>%
            mutate(
                interferent_max_dose_ft = purrr::map(
                    interferent_max_dose,
                    function(df) {
                        df %>%
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
                                interferent = "Interferent",
                                analyte_conc = "Analyte Conc.",
                                interferent_max_dose_by_diff = "Max. Interferent Conc by Difference",
                                interferent_max_dose_by_perc_diff = "Max. Interferent Conc by Percent Difference"
                            ) %>%
                            align(
                                align = "center",
                                part = "all"
                            ) %>%
                            merge_v(
                                part = "body",
                                j = "interferent"
                            )
                    }
                )
            )

        # --- Stage 4: References ---
        pkg_list <- c(
            "base", "shiny", "readxl", "ggplot2", "flextable",
            "dplyr", "tidyr", "purrr", "tibble", "officer"
        )

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                cit <- citation(pkg)
                if (length(cit) > 0) {
                    paste0(format(cit[[1]], style = "text"), collapse = " ")
                } else {
                    "No citation available"
                }
            })
        )

        ref_ft <- ref_df %>%
            flextable() %>%
            width(j = 1, width = 1.5) %>%
            width(j = 2, width = 6) %>%
            set_header_labels(Package = "R Package", Citation = "Citation Reference") %>%
            theme_box()

        # 回傳所有結果
        list(
            raw_ft = stage1_import$raw_ft[[1]],
            raw_plot = stage1_import$raw_plot[[1]],
            paired_difference_ft_list = stage2_analyze$paired_difference_ft,
            analyte_conc_list = stage2_analyze$analyte_conc,
            dose_response_ft = stage3_dose_response$interferent_max_dose_ft[[1]],
            ref_ft = ref_ft
        )
    })

    # --- 4.2 輸出渲染 ---

    # Output 1: 原始資料表
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })

    # Output 2: 原始資料圖
    output$out_raw_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    # Output 3: Paired Difference 動態表格
    output$out_paired_diff_tables <- renderUI({
        req(analysis_results())

        # 取得所有 paired_difference_ft
        ft_list <- analysis_results()$paired_difference_ft_list
        analyte_conc_list <- analysis_results()$analyte_conc_list

        # 動態產生每個表格的 UI
        table_ui_list <- lapply(seq_along(ft_list), function(i) {
            tagList(
                h4(paste("Analyte Concentration:", analyte_conc_list[i])),
                downloadButton(
                    paste0("dl_paired_diff_", i),
                    paste0("下載表格 ", i, " (.docx)")
                ),
                br(), br(),
                ft_list[[i]] %>% flextable::htmltools_value(),
                hr()
            )
        })

        do.call(tagList, table_ui_list)
    })

    # Output 4: Dose Response 分析結果
    output$out_dose_response_ft <- renderUI({
        req(analysis_results())
        analysis_results()$dose_response_ft %>% flextable::htmltools_value()
    })

    # Output 5: References
    output$out_ref_ft <- renderUI({
        req(analysis_results())
        analysis_results()$ref_ft %>% flextable::htmltools_value()
    })

    # --- 4.3 下載處理 ---

    # Download 1: 原始資料表
    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0(DOWNLOAD_PREFIX_RAW_TABLE, "_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    # Download 2: 原始資料圖
    output$dl_raw_plot <- downloadHandler(
        filename = function() {
            paste0(DOWNLOAD_PREFIX_RAW_PLOT, "_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(
                file,
                plot = analysis_results()$raw_plot,
                width = 10,
                height = 8,
                dpi = 300
            )
        }
    )

    # Download 3: Paired Difference 動態下載處理器
    observe({
        req(analysis_results())

        ft_list <- analysis_results()$paired_difference_ft_list
        analyte_conc_list <- analysis_results()$analyte_conc_list

        lapply(seq_along(ft_list), function(i) {
            local({
                my_i <- i
                output[[paste0("dl_paired_diff_", my_i)]] <- downloadHandler(
                    filename = function() {
                        paste0(
                            DOWNLOAD_PREFIX_PAIRED_DIFF,
                            "_analyte_", analyte_conc_list[my_i],
                            "_", Sys.Date(), ".docx"
                        )
                    },
                    content = function(file) {
                        save_as_docx(ft_list[[my_i]], path = file)
                    }
                )
            })
        })
    })

    # Download 4: Dose Response 分析結果
    output$dl_dose_response_ft <- downloadHandler(
        filename = function() {
            paste0(DOWNLOAD_PREFIX_DOSE_RESPONSE, "_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$dose_response_ft, path = file)
        }
    )

    # Download 5: References
    output$dl_ref_ft <- downloadHandler(
        filename = function() {
            paste0(DOWNLOAD_PREFIX_REFERENCES, "_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$ref_ft, path = file)
        }
    )
}

# --- 5. 啟動 App ---
shinyApp(
    ui = ui,
    server = server,
    options = list(
        host = "0.0.0.0",
        port = 80
    )
)
