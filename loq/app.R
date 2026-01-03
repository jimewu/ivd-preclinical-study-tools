# app.R

# --- 1. 初始化設定與套件載入 ---
# 載入基礎套件設定
source("conf_toolkit/lib_load_package.R") # [4]

library(shiny)
library(readxl)
library(ggplot2) # 用於 ggsave
library(flextable) # 用於 save_as_docx
library(dplyr)
library(tidyr)
library(purrr)

# 載入 helper functions
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [3]
source("general_loq.R") # [1], [2]

# --- 2. 可配置變數 (Configurable Variables) ---

# 2.1 Tab 標題 (可事後修改)
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Raw Data Plot"
TAB_TITLE_LOQ_RESULT <- "LoQ Analysis Result"
TAB_TITLE_REF <- "References"

# 2.2 GitHub 範例檔案連結 (請修改為正確的 LoQ 範例檔連結)
GITHUB_FILE_LINK <- "https://github.com/Start-S/Precision_Shiny_App/raw/main/loq_sample_data.xlsx"

# 2.3 輸入欄位說明文字 (Help Texts)
HELP_FILE_UPLOAD <- "Upload Excel File (Sheet: data)"
HELP_UNIT <- "Unit"
HELP_TEST_NAME <- "Subject Device Name"
HELP_ALLOWABLE_TE <- "Allowable Total Error (%)"
HELP_TE_METHOD <- "Model for Total Error Calculation"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Limit of Qauntititation"),
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

            # 3. 使用者參數輸入 (取代原始 params)
            textInput("input_unit", "Unit", value = "mg/dL"),
            helpText(HELP_UNIT),
            textInput("input_test_name", "Test Name", value = "Subject Device"),
            helpText(HELP_TEST_NAME),
            numericInput("input_allowable_te", "Allowable % TE", value = 21.6),
            helpText(HELP_ALLOWABLE_TE),
            selectInput("input_te_method", "TE Method",
                choices = c("Westgard model", "RMS model"),
                selected = "Westgard model"
            ),
            helpText(HELP_TE_METHOD),
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

                # 4.2 Tab 2: Raw Data Plot
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),

                # 4.3 Tab 3: LoQ Analysis Result
                tabPanel(
                    TAB_TITLE_LOQ_RESULT,
                    br(),
                    downloadButton("dl_loq_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_loq_ft")
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
    # 核心分析邏輯
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 讀取介面參數
        params <- list(
            unit = input$input_unit,
            test_name = input$input_test_name,
            allowable_perc_te = input$input_allowable_te,
            te_method = input$input_te_method
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # 確認必要套件 (依照 loq.R 邏輯)
        if (!require("purrr")) install.packages("purrr")

        # --- Stage 1: Import ---
        # 邏輯參照 loq.R [1]
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            # 格式整理:將 conc 對應為原本邏輯中的 sample,並設定因子
            transmute(
                lot = factor(lot),
                day = factor(day),
                conc = as.numeric(conc),
                y = as.numeric(y)
            ) %>%
            group_by(lot, day, conc) %>%
            mutate(
                replicate = row_number()
            ) %>%
            ungroup() %>%
            arrange(
                conc, lot, day
            ) %>%
            mutate(
                replicate = as.factor(replicate)
            ) %>%
            tidyr::nest(
                raw = colnames(.)
            ) %>%
            # 調用 general_loq.R [2]
            mutate_raw2ft(unit = params$unit) %>%
            mutate_raw2plot(
                test_name = params$test_name,
                unit = params$unit
            )

        # --- Stage 2: Tidy ---
        # 邏輯參照 loq.R [1]
        stage2_tidy <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = colnames(.)[colnames(.) != "lot"]
            )

        # --- Stage 3: Analysis ---
        # 邏輯參照 loq.R [1] 與 general_loq.R [2]
        stage3_analysis <- stage2_tidy %>%
            mutate_raw2total_error(
                loq_type = "lloq", # loq.R 中固定為 lloq
                allowable_perc_te = as.numeric(
                    params$allowable_perc_te
                ),
                te_method = params$te_method
            ) %>%
            mutate_total_error2ft(
                unit = params$unit,
                te_method = params$te_method
            )

        # --- Generarte Reference Table ---
        # 邏輯參照 app.R [6]
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
            theme_box() # [5]

        # 回傳結果 List
        list(
            raw_ft = stage1_import$raw_ft[[1]], # 原始資料表
            raw_plot = stage1_import$raw_plot[[1]], # 原始資料圖
            loq_ft = stage3_analysis$total_error_ft[[1]], # LoQ 分析結果 (來自 stage3)
            ref_ft = ref_ft # 參考文獻
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
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 8, dpi = 300)
        }
    )

    # 3. LoQ Result Table Output
    output$out_loq_ft <- renderUI({
        req(analysis_results())
        analysis_results()$loq_ft %>% flextable::htmltools_value()
    })

    output$dl_loq_ft <- downloadHandler(
        filename = function() {
            paste0("loq_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$loq_ft, path = file)
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

shinyApp(
    ui = ui,
    server = server,
    options = list(
        host = "0.0.0.0",
        port = 80
    )
)
