source("conf_toolkit/lib_load_package.R") # 載入套件管理 [4]
library(shiny)
library(readxl)
library(ggplot2) # 用於 ggsave
library(flextable) # 用於 save_as_docx

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [2]
source("conf_toolkit/lib_general_precision.R") # [3]

# --- 2. 使用者定義變數 ---
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Raw Data Plot"
TAB_TITLE_VCA_TABLE <- "VCA Result Table"

GITHUB_FILE_LINK <- "https://github.com/Start-S/Precision_Shiny_App/raw/main/precision_sample_data.xlsx"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Precision Analysis App"),
    sidebarLayout(
        sidebarPanel(
            # 下載範例檔連結
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download Template from GitHub"
            ),
            br(), br(),
            fileInput("file_upload", "Upload Excel File (Sheet: data)",
                accept = c(".xlsx")
            ),
            textInput("input_analyte", "Analyte Name", value = "Glucose"),
            textInput("input_unit", "Unit", value = "mg/dL"),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                # Tab 1: Raw Data
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    # 下載按鈕 1
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # Tab 2: Plot
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    # 下載按鈕 2
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),

                # Tab 3: VCA Result
                tabPanel(
                    TAB_TITLE_VCA_TABLE,
                    br(),
                    # 下載按鈕 3
                    downloadButton("dl_vca_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_vca_ft")
                )
            )
        )
    )
)

# --- 4. Server 邏輯 ---
server <- function(input, output, session) {
    # 核心分析邏輯 (參照 precision.R [1])
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        params <- list(
            analyte_name = input$input_analyte,
            unit = input$input_unit
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # 確保 VCA 套件已載入
        if (!require("VCA")) install.packages("VCA")

        # --- Stage 1: Import & Format ---
        # 邏輯來自 precision.R [1]，呼叫 lib_general_precision.R [3] 進行格式化
        stage1_import <- read_excel(path = file_path, sheet = sheet_name) %>%
            transmute(
                sample = factor(conc),
                day = factor(day),
                run = factor(run),
                replicate = factor(replicate),
                y = as.numeric(y)
            ) %>%
            tidyr::nest(raw = everything()) %>%
            mutate_raw2ft(dev_unit = params$unit) %>%
            mutate_raw2plot(
                analyte_name = params$analyte_name,
                dev_unit = params$unit
            )

        # --- Stage 2: VCA Analysis ---
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(raw = colnames(.)[colnames(.) != "sample"]) %>%
            mutate(
                VCA = purrr::map(
                    raw,
                    function(raw) {
                        result <- VCA::anovaVCA(
                            form = y ~ day / run / replicate,
                            Data = as.data.frame(raw)
                        )
                        return(result)
                    }
                )
            ) %>%
            mutate_VCA2df()

        # --- Stage 3: Share Result ---
        stage3_share <- stage2_analyze %>%
            select(sample, VCA_df) %>%
            tidyr::unnest(VCA_df) %>%
            tidyr::nest(VCA_df = colnames(.)) %>%
            mutate_VCA_df2ft()

        list(
            raw_ft = stage1_import$raw_ft[[1]],
            raw_plot = stage1_import$raw_plot[[1]],
            vca_ft = stage3_share$VCA_ft[[1]]
        )
    })

    # --- Output Render & Download Handlers ---

    # Result 1: Raw Table
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })

    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("raw_data_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            # 為了保留 flextable 的格式，將其存為 Word
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    # Result 2: Plot
    output$out_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    output$dl_plot <- downloadHandler(
        filename = function() {
            paste0("precision_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 8, dpi = 300)
        }
    )

    # Result 3: VCA Table
    output$out_vca_ft <- renderUI({
        req(analysis_results())
        analysis_results()$vca_ft %>% flextable::htmltools_value()
    })

    output$dl_vca_ft <- downloadHandler(
        filename = function() {
            paste0("vca_result_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$vca_ft, path = file)
        }
    )
}

shinyApp(ui = ui, server = server)
