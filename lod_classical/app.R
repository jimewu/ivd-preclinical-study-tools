source("conf_toolkit/lib_load_package.R") # [4]
library(shiny)
library(readxl)
library(ggplot2) # 用於 ggsave
library(flextable) # 用於 save_as_docx
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [3]
source("general_lod_classical.R") # [1]

# --- 2. 可配置變數 (Configurable Variables) ---
# 2.1 Tab 標題 (可事後修改)
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Raw Data Plot"
TAB_TITLE_LOD_RESULT <- "LoD Analysis Result"
TAB_TITLE_REF <- "References"

# 2.2 GitHub 範例檔案連結
GITHUB_FILE_LINK <- "https://github.com/Start-S/Precision_Shiny_App/raw/main/lod_classical_sample_data_parametric.xlsx" # 請確認此連結是否正確，或替換為正確的 LoD 範例檔連結

# 2.3 輸入欄位說明文字 (Help Texts)
HELP_FILE_UPLOAD <- "請上傳 Excel 檔案 (.xlsx)，資料需位於名為 'data' 的工作表中。"
HELP_ANALYTE <- "輸入分析物的名稱 (例如: Glucose)。"
HELP_UNIT <- "輸入濃度單位 (例如: mg/dL)。"
HELP_TEST_NAME <- "輸入受測裝置的名稱 (例如: Subject Device, New Method)。"
HELP_LOB <- "輸入空白極限 (Limit of Blank) 數值。若未知可設為 0。"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("LoD Analysis App (Classical Approach)"),
    sidebarLayout(
        sidebarPanel(
            # 1. 下載範例檔連結
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download Template from GitHub"
            ),
            br(), br(),

            # 2. 檔案上傳
            fileInput("file_upload", "Upload Excel File",
                accept = c(".xlsx")
            ),
            helpText(HELP_FILE_UPLOAD),
            hr(),

            # 3. 使用者參數輸入
            textInput("input_analyte", "Analyte Name", value = "Glucose"),
            helpText(HELP_ANALYTE),
            textInput("input_unit", "Unit", value = "mg/dL"),
            helpText(HELP_UNIT),
            textInput("input_test_name", "Test Name", value = "Subject Device"),
            helpText(HELP_TEST_NAME),
            textInput("input_lob", "LoB (Limit of Blank)", value = "0"),
            helpText(HELP_LOB),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                # 4.1 Tab 1: Raw Data Table
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # 4.2 Tab 2: Plot
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),

                # 4.3 Tab 3: LoD Analysis Result
                tabPanel(
                    TAB_TITLE_LOD_RESULT,
                    br(),
                    downloadButton("dl_lod_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_lod_ft")
                ),

                # 4.4 Tab 4: References
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
    # 核心分析邏輯 (參照 lod_classical.R [2])
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 讀取介面參數
        params <- list(
            analyte_name = input$input_analyte,
            unit = input$input_unit,
            test_name = input$input_test_name,
            lob = input$input_lob
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # 確認必要套件
        if (!require("purrr")) install.packages("purrr")

        # --- Stage 1: Import ---
        # 邏輯來自 lod_classical.R [2]
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            # 格式整理:將 conc 對應為原本邏輯中的 sample,並設定因子
            transmute(
                conc = as.numeric(conc),
                lot = factor(lot),
                day = factor(day),
                y = as.numeric(y)
            ) %>%
            arrange(
                conc,
                lot,
                day
            ) %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate(
                summary = purrr::map(
                    raw,
                    function(raw) {
                        nday <- raw$day %>%
                            factor() %>%
                            levels() %>%
                            length()
                        nlot <- raw$lot %>%
                            factor() %>%
                            levels() %>%
                            length()
                        nconc <- raw$conc %>%
                            factor() %>%
                            levels() %>%
                            length()
                        ntest <- nrow(raw)
                        replicate <- ntest / (nday * nlot * nconc)

                        data.frame(nlot, nday, nconc, ntest, replicate) %>% return()
                    }
                )
            ) %>%
            # 調用 general_lod_classical.R [1] 函數
            mutate_raw2ft(
                unit = params$unit,
                test_name = params$test_name
            ) %>%
            mutate_raw2plot(
                unit = params$unit,
                test_name = params$test_name
            )

        # --- Stage 2: Analyze ---
        # 邏輯來自 lod_classical.R [2]
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            mutate(
                # ntest: 不分批次總測試數
                ntest = nrow(.)
            ) %>%
            tidyr::nest(
                raw = colnames(.)[colnames(.) != "lot"]
            ) %>%
            mutate_raw2lot_summary() %>% # [1]
            select(-raw) %>%
            mutate_lot_summary2lot_sd_lod(lob = params$lob) %>% # [1]
            tidyr::unnest(lot_sd_lod) %>%
            mutate(
                final_lod = max(lot_lod)
            )

        # --- Stage 2A & 3: Summary Table ---
        # 邏輯來自 lod_classical.R [2]
        stage2A_analyze_summary <- stage2_analyze %>%
            tidyr::unnest(lot_summary) %>%
            select(-ntest, -nconc_lot) %>%
            tidyr::nest(
                summary = everything()
            ) %>%
            mutate_lod_summary2ft(unit = params$unit) # [1]

        # --- Stage 4: Reference Table Generator ---
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "tidyr", "purrr")

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
            theme_box() # [5] (從 lib_officeverse.R 載入)

        # 回傳結果 List，提取 [[1]] 以符合您的需求
        list(
            raw_ft = stage1_import$raw_ft[[1]], # 4.1 原始資料表
            raw_plot = stage1_import$raw_plot[[1]], # 4.2 原始資料圖
            lod_ft = stage2A_analyze_summary$summary_ft[[1]], # 4.3 LoD 結果表
            ref_ft = ref_ft # 4.4 Reference
        )
    })

    # --- Output Render & Download Handlers ---

    # 1. Raw Table Output
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

    # 2. Plot Output
    output$out_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    output$dl_plot <- downloadHandler(
        filename = function() {
            paste0("raw_data_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            # LoD plot 寬高可能需要根據 conc 數量調整，這裡設為預設值
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 8, dpi = 300)
        }
    )

    # 3. LoD Result Table Output
    output$out_lod_ft <- renderUI({
        req(analysis_results())
        analysis_results()$lod_ft %>% flextable::htmltools_value()
    })

    output$dl_lod_ft <- downloadHandler(
        filename = function() {
            paste0("lod_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$lod_ft, path = file)
        }
    )

    # 4. Reference Table Output
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

shinyApp(ui = ui, server = server)
