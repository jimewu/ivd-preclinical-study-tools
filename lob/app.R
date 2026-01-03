source("conf_toolkit/lib_load_package.R") # 載入套件管理 [4]

pkg_lst <- c(
    "shiny",
    "patchwork"
)

pacman::p_load(char = pkg_lst)

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [3]
source("general_lob.R") # 載入 LoB 相關函式 [2]

# --- 2. 使用者定義變數 (Configuration) ---

# 2.1 分頁名稱設定 (Tab Titles)
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Evaluation Plot"
TAB_TITLE_LOB_RESULT <- "LoB Analysis Result"
TAB_TITLE_REF <- "References"

# 2.2 GitHub 模板連結
# 請將此處連結修改為您實際存放 LoB 模板的網址
GITHUB_FILE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_lob.xlsx"

# 2.3 輸入欄位說明文字 (Help Text)
HELP_TEXT_UNIT <- "請輸入測量單位的字串 (e.g., mg/dL, mmol/L)"
HELP_TEXT_TEST_NAME <- "請輸入測試裝置或方法的名稱 (e.g., Subject Device)"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Limit of Blank (LoB) Analysis App"),
    sidebarLayout(
        sidebarPanel(
            # 下載範例檔連結
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download LoB Template from GitHub"
            ),
            br(), br(),

            # 檔案上傳
            fileInput("file_upload", "Upload Excel File (Sheet: data)",
                accept = c(".xlsx")
            ),
            hr(),

            # 參數輸入，附帶說明文字
            textInput("input_unit", "Unit", value = "mg/dL"),
            helpText(HELP_TEXT_UNIT),
            textInput("input_test_name", "Test Name", value = "Subject Device"),
            helpText(HELP_TEXT_TEST_NAME),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                # Tab 1: Raw Data
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # Tab 2: Plot
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),

                # Tab 3: LoB Result
                tabPanel(
                    TAB_TITLE_LOB_RESULT,
                    br(),
                    downloadButton("dl_lob_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_lob_ft")
                ),

                # Tab 4: References
                tabPanel(
                    TAB_TITLE_REF,
                    br(),
                    downloadButton("dl_ref_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_ref_ft")
                )
            )
        )
    )
)

# --- 4. Server 邏輯 ---
server <- function(input, output, session) {
    # 核心分析邏輯 (參照 lob.R [1])
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 取得使用者輸入參數
        params <- list(
            unit = input$input_unit,
            test_name = input$input_test_name
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # --- Stage 1: Import ---
        # 邏輯來自 lob.R [1]
        raw_df <- read_excel(
            path = file_path,
            sheet = sheet_name
        )

        # 格式整理與巢狀化
        stage1_import <- raw_df %>%
            transmute(
                lot = factor(lot), # 對應 lob.R 中的邏輯
                day = factor(day),
                y = as.numeric(y)
            ) %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate(
                summary = purrr::map(
                    raw,
                    function(raw) {
                        nlot <- raw$lot %>%
                            factor() %>%
                            levels() %>%
                            length()

                        nday <- raw$day %>%
                            factor() %>%
                            levels() %>%
                            length()

                        replicate <- nrow(raw) / (nlot * nday)

                        data.frame(
                            nlot,
                            nday,
                            replicate
                        ) %>%
                            return()
                    }
                )
            ) %>%
            mutate_raw2ft(
                test_name = params$test_name,
                unit = params$unit
            ) %>%
            mutate_raw2plot(
                test_name = params$test_name,
                unit = params$unit
            )

        # --- Stage 2: Analyze ---
        # 邏輯來自 lob.R [1]，進行常態性檢定與 LoB 計算
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = colnames(.)[colnames(.) != "lot"]
            ) %>%
            mutate(
                shapiro_test = purrr::map(
                    raw,
                    function(raw) {
                        shapiro.test(raw$y) %>%
                            return()
                    }
                )
            ) %>%
            mutate(
                shapiro_p = purrr::map_dbl(
                    shapiro_test,
                    function(df) {
                        return(
                            round(
                                df$p.value,
                                digits = 3
                            )
                        )
                    }
                )
            ) %>%
            mutate(
                if_norm = purrr::map_lgl(
                    shapiro_test,
                    function(x) {
                        if (x$p.value >= 0.05) {
                            return(TRUE)
                        } else {
                            return(FALSE)
                        }
                    }
                )
            ) %>%
            cbind(
                .,
                all_norm = all(.$if_norm)
            ) %>%
            mutate(
                lob_summary = purrr::map2(
                    .x = raw,
                    .y = all_norm,
                    function(raw, all_norm) {
                        if (all_norm) {
                            # Parametric 算法
                            raw %>%
                                summarize(
                                    mean_blank = mean(y),
                                    sd_blank = sd(y),
                                    cp = 1.645 / (1 - (1 / (4 * (nrow(raw) - 5)))),
                                    lot_lob = mean_blank + cp * sd_blank
                                ) %>%
                                return()
                        } else {
                            # Non-parametric 算法
                            raw %>%
                                arrange(y) %>%
                                summarize(
                                    rank_pos = 0.5 + 0.95 * length(y),
                                    lot_lob = mean(
                                        c(
                                            y[ceiling(rank_pos)],
                                            y[floor(rank_pos)]
                                        )
                                    )
                                ) %>%
                                return()
                        }
                    }
                )
            ) %>%
            mutate(
                lot_lob = purrr::map_dbl(
                    lob_summary,
                    function(df) {
                        return(df$lot_lob)
                    }
                )
            ) %>%
            cbind(
                .,
                final_lob = max(.$lot_lob)
            )

        # --- Stage 2A: Format Result Table ---
        # 整理最終 LoB 表格 (參照 lob.R [1])
        stage2A_analyze_lob_summary <- stage2_analyze %>%
            select(
                lot,
                shapiro_p,
                if_norm,
                lob_summary,
                final_lob
            ) %>%
            tidyr::unnest(lob_summary) %>%
            tidyr::nest(
                lob_summary = everything()
            ) %>%
            mutate_lob_summary2ft(
                unit = params$unit
            )

        # --- Stage 4: Reference Table Generator ---
        # 列出此 App 使用的關鍵套件
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "purrr", "patchwork")

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                if (pkg == "patchwork" && !("patchwork" %in% .packages())) library(patchwork)

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

        # 回傳運算結果 List
        list(
            raw_ft = stage1_import$raw_ft[[1]], # 原始資料表
            raw_plot = stage1_import$raw_plot[[1]], # 原始資料圖
            lob_ft = stage2A_analyze_lob_summary$lob_summary_ft[[1]], # LoB 結果表
            ref_ft = ref_ft
        )
    })

    # --- Output Render & Download Handlers ---

    # 1. Raw Data Table
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })

    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("raw_data_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    # 2. Raw Data Plot
    output$out_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    output$dl_plot <- downloadHandler(
        filename = function() {
            paste0("lob_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 12, height = 8, dpi = 300)
        }
    )

    # 3. LoB Result Table
    output$out_lob_ft <- renderUI({
        req(analysis_results())
        analysis_results()$lob_ft %>% flextable::htmltools_value()
    })

    output$dl_lob_ft <- downloadHandler(
        filename = function() {
            paste0("lob_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$lob_ft, path = file)
        }
    )

    # 4. References Table
    output$out_ref_ft <- renderUI({
        req(analysis_results())
        analysis_results()$ref_ft %>% flextable::htmltools_value()
    })

    output$dl_ref_ft <- downloadHandler(
        filename = function() {
            paste0("references_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$ref_ft, path = file)
        }
    )
}

shinyApp(
    ui = ui,
    server = server,
    options = list(
        host = "0.0.0.0",
        port = 80
    )
)
