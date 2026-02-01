# ============================================================
# EP06 Linearity and Recovery Shiny App
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
    "broom",
    "purrr",
    "tibble",
    "officer"
)

pacman::p_load(char = pkg_lst)

source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")
source("lib_general_linearity.R")

# --- 2. 使用者定義變數 ---
# 分頁標題（繁體中文）
TAB_TITLE_1_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_2_RAW_PLOT <- "Raw Data Plot"
TAB_TITLE_3_RAW_PLOT_FACET <- "Raw Data Plot: by RC"
TAB_TITLE_4_SUMMARY_TABLE <- "Summary"
TAB_TITLE_5_SD_PLOT <- "SD Plot"
TAB_TITLE_6_CV_PLOT <- "%CV Plot"
TAB_TITLE_7_FIT_SUMMARY <- "Linear Regression"
TAB_TITLE_8_LINEARITY_RESULT <- "Linearity Analysis"
TAB_TITLE_9_RECOVERY_RESULT <- "Recovery Analysis"
TAB_TITLE_10_REFERENCES <- "References"

# GitHub 模板連結
GITHUB_FILE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_linearity.xlsx"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Linearity and Recovery Analysis"),
    sidebarLayout(
        sidebarPanel(
            # 3.1 GitHub 模板連結
            tags$a(
                href = GITHUB_FILE_LINK,
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
                value = "mg/dL",
                placeholder = "例如：mg/dL, mmol/L, U/L"
            ),
            helpText(
                "請輸入測量值的單位，此單位將顯示於所有圖表和表格中。",
                "常見單位：mg/dL (毫克/分升)、mmol/L (毫莫耳/升)、U/L (單位/升)"
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

                # Tab 2: 原始資料散點圖
                tabPanel(
                    TAB_TITLE_2_RAW_PLOT,
                    br(),
                    downloadButton("dl_raw_plot", "下載圖表 (.png)"),
                    br(), br(),
                    plotOutput("out_raw_plot", height = "600px")
                ),

                # Tab 3: 原始資料分面圖
                tabPanel(
                    TAB_TITLE_3_RAW_PLOT_FACET,
                    br(),
                    downloadButton("dl_raw_plot_facet", "下載圖表 (.png)"),
                    br(), br(),
                    plotOutput("out_raw_plot_facet", height = "600px")
                ),

                # Tab 4: 統計摘要表
                tabPanel(
                    TAB_TITLE_4_SUMMARY_TABLE,
                    br(),
                    downloadButton("dl_summary_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_summary_ft")
                ),

                # Tab 5: 標準差圖
                tabPanel(
                    TAB_TITLE_5_SD_PLOT,
                    br(),
                    downloadButton("dl_sd_plot", "下載圖表 (.png)"),
                    br(), br(),
                    plotOutput("out_sd_plot", height = "600px")
                ),

                # Tab 6: 變異係數圖
                tabPanel(
                    TAB_TITLE_6_CV_PLOT,
                    br(),
                    downloadButton("dl_cv_plot", "下載圖表 (.png)"),
                    br(), br(),
                    plotOutput("out_cv_plot", height = "600px")
                ),

                # Tab 7: 線性回歸結果
                tabPanel(
                    TAB_TITLE_7_FIT_SUMMARY,
                    br(),
                    downloadButton("dl_fit_summary_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_fit_summary_ft")
                ),

                # Tab 8: 線性度分析結果
                tabPanel(
                    TAB_TITLE_8_LINEARITY_RESULT,
                    br(),
                    downloadButton("dl_linearity_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_linearity_ft")
                ),

                # Tab 9: 回收率分析結果
                tabPanel(
                    TAB_TITLE_9_RECOVERY_RESULT,
                    br(),
                    downloadButton("dl_recovery_ft", "下載表格 (.docx)"),
                    br(), br(),
                    uiOutput("out_recovery_ft")
                ),

                # Tab 10: 參考文獻
                tabPanel(
                    TAB_TITLE_10_REFERENCES,
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

        # 準備參數
        params <- list(
            unit = input$input_unit
        )

        # 讀取檔案
        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # --- Stage 1: Import and Tidy ---
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            group_by(rc, y_ref) %>%
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
            )

        # --- Stage 2: QC ---
        stage2_qc <- stage1_import %>%
            select(
                raw
            ) %>%
            mutate(
                raw_plot = purrr::map(
                    raw,
                    function(x) {
                        result <- x %>%
                            ggplot(
                                aes(
                                    x = rc,
                                    y = y,
                                    color = factor(replicate)
                                )
                            ) +
                            geom_point() +
                            labs(
                                x = "Relative Concentration (RC)",
                                y = paste(
                                    "Measured",
                                    params$unit,
                                    sep = " "
                                ),
                                color = "Replicate"
                            ) %>%
                            return()
                    }
                ),
                raw_plot_facet = purrr::map(
                    raw_plot,
                    function(x) {
                        result <- x +
                            facet_wrap(
                                ~rc,
                                scales = "free"
                            ) %>%
                            return()
                    }
                )
            ) %>%
            mutate(
                summary = purrr::map(
                    raw,
                    function(x) {
                        result <- x %>%
                            group_by(
                                rc
                            ) %>%
                            summarize(
                                measured_value = mean(y),
                                sd = sd(y),
                                cv = 100 * sd / measured_value,
                                var = var(y),
                                n = n(),
                                weight = n / var
                            ) %>%
                            arrange(
                                desc(rc)
                            )

                        high <- result %>%
                            filter(
                                rc == 1
                            ) %>%
                            .$measured_value

                        result <- result %>%
                            mutate(
                                expected_value = high * rc
                            ) %>%
                            relocate(
                                expected_value,
                                .after = measured_value
                            ) %>%
                            return()
                    }
                )
            ) %>%
            mutate_summary2ft(
                ncol_extra = 2,
                unit = params$unit
            ) %>%
            mutate_summary2sd_cv_plot(unit = params$unit)

        # --- Stage 3: Linearity Analysis ---
        stage3_analysis_linearity <- stage2_qc %>%
            select(
                raw,
                summary
            ) %>%
            mutate(
                fit = purrr::map(
                    summary,
                    function(x) {
                        result <- lm(
                            formula = "measured_value ~ expected_value",
                            data = x,
                            weights = weight
                        ) %>%
                            return()
                    }
                ),
                fit_summary = purrr::map(
                    fit,
                    function(x) {
                        result <- summary(x) %>%
                            return()
                    }
                ),
                fit_summary_tidy = purrr::map(
                    fit_summary,
                    function(x) {
                        result <- broom::tidy(x) %>%
                            mutate(
                                overall_p_value = p.value[2]
                            ) %>%
                            select(-p.value) %>%
                            return()
                    }
                ),
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
                                term = c(
                                    "(Intercept)",
                                    "Expected Value"
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
                            ) %>%
                            flextable() %>%
                            set_header_labels(
                                term = "Item",
                                estimate = "Value",
                                std.error = "Standard Error",
                                overall_p_value = "Regression p-value",
                                if_significant = "Significant (Y) or Not (N)?"
                            ) %>%
                            merge_v(
                                part = "body",
                                j = 4:5
                            ) %>%
                            align(
                                align = "center",
                                part = "all"
                            ) %>%
                            width_ratio(
                                width_col_ratio = c(
                                    1.5,
                                    rep(1, 4)
                                )
                            )
                    }
                ),
                predict = purrr::map2(
                    .x = fit,
                    .y = summary,
                    function(x, y) {
                        expected_value_ori <- y$expected_value

                        result <- tibble(
                            expected_value = expected_value_ori,
                            predicted_value = predict(
                                x,
                                newdata = data.frame(
                                    expected_value = expected_value_ori
                                )
                            )
                        ) %>%
                            return()
                    }
                ),
                merge = purrr::map2(
                    .x = summary,
                    .y = predict,
                    function(x, y) {
                        result <- merge(
                            x,
                            y,
                            by = "expected_value"
                        ) %>%
                            mutate(
                                deviation = measured_value - predicted_value,
                                perc_deviation = 100 * deviation / predicted_value
                            ) %>%
                            return()
                    }
                ),
                merge_ft = purrr::map(
                    merge,
                    function(x) {
                        x <- x %>%
                            select(
                                rc,
                                measured_value,
                                expected_value,
                                predicted_value,
                                deviation,
                                perc_deviation
                            )

                        result <- x %>%
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
                            arrange(
                                desc(rc)
                            ) %>%
                            flextable() %>%
                            set_header_labels(
                                rc = "RC",
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
                            footnote(
                                part = "header",
                                i = 1,
                                j = which(
                                    colnames(x) == "measured_value"
                                ),
                                value = as_paragraph(
                                    "Mean of replicates"
                                ),
                                ref_symbols = letters[1]
                            ) %>%
                            footnote(
                                part = "header",
                                i = 1,
                                j = which(
                                    colnames(x) == "expected_value"
                                ),
                                value = as_paragraph(
                                    "RC * Measured Value of HIGH"
                                ),
                                ref_symbols = letters[2]
                            ) %>%
                            footnote(
                                part = "header",
                                i = 1,
                                j = which(
                                    colnames(x) == "deviation"
                                ),
                                value = as_paragraph(
                                    "Measured Value - Predicted Value"
                                ),
                                ref_symbols = letters[3]
                            ) %>%
                            footnote(
                                part = "header",
                                i = 1,
                                j = which(
                                    colnames(x) == "perc_deviation"
                                ),
                                value = as_paragraph(
                                    "100 * Deviation / (Predicted Value)"
                                ),
                                ref_symbols = letters[4]
                            ) %>%
                            return()
                    }
                )
            )

        # --- Stage 3A: Recovery Analysis ---
        stage3A_analysis_recovery <- stage1_import %>%
            select(raw) %>%
            mutate(
                recovery = purrr::map(
                    raw,
                    function(x) {
                        result <- x %>%
                            group_by(rc, y_ref) %>%
                            summarize(
                                measured_value = mean(y),
                                .groups = "drop"
                            ) %>%
                            mutate(
                                recovery = 100 * measured_value / y_ref
                            ) %>%
                            arrange(
                                desc(rc)
                            ) %>%
                            return()
                    }
                ),
                recovery_ft = purrr::map(
                    recovery,
                    function(recovery) {
                        result <- recovery %>%
                            relocate(
                                measured_value,
                                .after = y_ref
                            ) %>%
                            mutate_if(
                                is.numeric,
                                function(x) {
                                    result <- x %>%
                                        round(
                                            digits = 3
                                        ) %>%
                                        return()
                                }
                            ) %>%
                            flextable() %>%
                            set_header_labels(
                                rc = "RC",
                                y_ref = "Ref.",
                                measured_value = "Measured Value",
                                recovery = "%Recovery"
                            ) %>%
                            align(
                                align = "center",
                                part = "all"
                            ) %>%
                            footnote(
                                part = "header",
                                i = 1,
                                j = c(
                                    "y_ref",
                                    "measured_value"
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
                            return()
                    }
                )
            )

        # --- Stage 4: References ---
        pkg_list <- c(
            "base", "shiny", "readxl", "ggplot2", "flextable",
            "dplyr", "tidyr", "purrr", "broom", "tibble", "officer"
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
            raw_plot = stage2_qc$raw_plot[[1]],
            raw_plot_facet = stage2_qc$raw_plot_facet[[1]],
            summary_ft = stage2_qc$summary_ft[[1]],
            sd_plot = stage2_qc$sd_plot[[1]],
            cv_plot = stage2_qc$cv_plot[[1]],
            fit_summary_ft = stage3_analysis_linearity$fit_summary_ft[[1]],
            linearity_ft = stage3_analysis_linearity$merge_ft[[1]],
            recovery_ft = stage3A_analysis_recovery$recovery_ft[[1]],
            ref_ft = ref_ft
        )
    })

    # --- 4.2 Output Render (10 個) ---

    # Output 1: 原始資料表
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })

    # Output 2: 原始資料散點圖
    output$out_raw_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    # Output 3: 原始資料分面圖
    output$out_raw_plot_facet <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot_facet
    })

    # Output 4: 統計摘要表
    output$out_summary_ft <- renderUI({
        req(analysis_results())
        analysis_results()$summary_ft %>% flextable::htmltools_value()
    })

    # Output 5: 標準差圖
    output$out_sd_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$sd_plot
    })

    # Output 6: 變異係數圖
    output$out_cv_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$cv_plot
    })

    # Output 7: 線性回歸結果
    output$out_fit_summary_ft <- renderUI({
        req(analysis_results())
        analysis_results()$fit_summary_ft %>% flextable::htmltools_value()
    })

    # Output 8: 線性度分析結果
    output$out_linearity_ft <- renderUI({
        req(analysis_results())
        analysis_results()$linearity_ft %>% flextable::htmltools_value()
    })

    # Output 9: 回收率分析結果
    output$out_recovery_ft <- renderUI({
        req(analysis_results())
        analysis_results()$recovery_ft %>% flextable::htmltools_value()
    })

    # Output 10: 參考文獻
    output$out_ref_ft <- renderUI({
        req(analysis_results())
        analysis_results()$ref_ft %>% flextable::htmltools_value()
    })

    # --- 4.3 Download Handlers (10 個) ---

    # Download 1: 原始資料表
    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("raw_data_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    # Download 2: 原始資料散點圖
    output$dl_raw_plot <- downloadHandler(
        filename = function() {
            paste0("raw_data_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file,
                plot = analysis_results()$raw_plot,
                width = 10, height = 8, dpi = 300
            )
        }
    )

    # Download 3: 原始資料分面圖
    output$dl_raw_plot_facet <- downloadHandler(
        filename = function() {
            paste0("raw_data_facet_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file,
                plot = analysis_results()$raw_plot_facet,
                width = 10, height = 8, dpi = 300
            )
        }
    )

    # Download 4: 統計摘要表
    output$dl_summary_ft <- downloadHandler(
        filename = function() {
            paste0("summary_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$summary_ft, path = file)
        }
    )

    # Download 5: 標準差圖
    output$dl_sd_plot <- downloadHandler(
        filename = function() {
            paste0("sd_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file,
                plot = analysis_results()$sd_plot,
                width = 10, height = 8, dpi = 300
            )
        }
    )

    # Download 6: 變異係數圖
    output$dl_cv_plot <- downloadHandler(
        filename = function() {
            paste0("cv_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file,
                plot = analysis_results()$cv_plot,
                width = 10, height = 8, dpi = 300
            )
        }
    )

    # Download 7: 線性回歸結果
    output$dl_fit_summary_ft <- downloadHandler(
        filename = function() {
            paste0("fit_summary_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$fit_summary_ft, path = file)
        }
    )

    # Download 8: 線性度分析結果
    output$dl_linearity_ft <- downloadHandler(
        filename = function() {
            paste0("linearity_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$linearity_ft, path = file)
        }
    )

    # Download 9: 回收率分析結果
    output$dl_recovery_ft <- downloadHandler(
        filename = function() {
            paste0("recovery_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$recovery_ft, path = file)
        }
    )

    # Download 10: 參考文獻
    output$dl_ref_ft <- downloadHandler(
        filename = function() {
            paste0("references_", Sys.Date(), ".docx")
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
