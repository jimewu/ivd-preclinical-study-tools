source("conf_toolkit/lib_load_package.R") # [4]

# 除了 lib_load_package.R 中已定義的通用套件外，載入 Shiny 相關套件
pkg_lst <- c(
    "shiny",
    "broom" # 用於回歸分析整理 [1]
)

pacman::p_load(char = pkg_lst)

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [3]
source("general_lod_precision_profile.R") # [1]

# --- 2. 使用者定義分頁名稱 (可在此處修改) ---
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Raw Data Plot"
TAB_TITLE_BASIC_STATS <- "Basic Analysis Stats"
TAB_TITLE_FIT1 <- "LoD Fit 1 (Linear)"
TAB_TITLE_FIT2 <- "LoD Fit 2 (2nd Order)"
TAB_TITLE_FIT3 <- "LoD Fit 3 (3rd Order)"
TAB_TITLE_REF <- "References"

# GitHub 模板連結
GITHUB_FILE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.3/template_lod_precision_profile.xlsx"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("LoD (Precision Profile) Analysis App"),
    sidebarLayout(
        sidebarPanel(
            # 1. GitHub 模板下載按鈕
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download Template from GitHub"
            ),
            br(), br(),

            # 2. 檔案上傳
            fileInput("file_upload", "Upload Excel File (Sheet name must be: data)",
                accept = c(".xlsx")
            ),
            hr(),

            # 3. 參數設定 (Params) 與說明文字
            h4("Analysis Parameters"),
            textInput("input_analyte", "Analyte Name", value = "Glucose"),
            helpText("Name of the substance being measured (e.g., Glucose, HbA1c)."),
            textInput("input_unit", "Unit", value = "mg/dL"),
            helpText("Unit of measurement for the analyte."),
            textInput("input_test_name", "Test Name", value = "Subject Device"),
            helpText("Name of the device or method under evaluation."),
            textInput("input_lob", "Limit of Blank (LoB)", value = "0.1"),
            helpText("The pre-determined LoB value used to calculate LoD. (Must be numeric)"),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                # 4.1 原始資料表
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # 4.2 原始資料 Plot
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),

                # 4.3 分析基本參數
                tabPanel(
                    TAB_TITLE_BASIC_STATS,
                    br(),
                    downloadButton("dl_basic_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_basic_ft")
                ),

                # 4.4 Fit 1 LoD 結果
                tabPanel(
                    TAB_TITLE_FIT1,
                    br(),
                    downloadButton("dl_fit1_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_fit1_ft")
                ),

                # 4.5 Fit 2 LoD 結果
                tabPanel(
                    TAB_TITLE_FIT2,
                    br(),
                    downloadButton("dl_fit2_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_fit2_ft")
                ),

                # 4.6 Fit 3 LoD 結果
                tabPanel(
                    TAB_TITLE_FIT3,
                    br(),
                    downloadButton("dl_fit3_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_fit3_ft")
                ),

                # 5. References Table
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
    # 核心分析邏輯 (參照 lod_precision_profile.R [2])
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 參數讀取
        params <- list(
            analyte_name = input$input_analyte,
            unit = input$input_unit,
            test_name = input$input_test_name,
            lob = input$input_lob
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # --- Stage 1: Import ---
        # 參照 script [2] 的 Stage 1 流程，並呼叫 [1] 的函數
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            transmute(
                dev_lot = factor(dev_lot),
                sample = factor(sample),
                mean = as.numeric(mean),
                sd_wl = as.numeric(sd_wl),
                n_test = as.numeric(n_test)
            ) %>%
            arrange(
                dev_lot,
                sample
            ) %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate_raw2ft(
                unit = params$unit,
                test_name = params$test_name
            ) %>%
            mutate_raw2plot(
                unit = params$unit,
                test_name = params$test_name
            )

        # --- Stage 2: Analyze ---
        # 參照 script [2] 的 Stage 2 流程，呼叫 [1] 的運算函數
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = colnames(.)[colnames(.) != "dev_lot"]
            ) %>%
            mutate_raw2lot_summary() %>%
            tidyr::unnest(lot_summary) %>%
            mutate_raw2fit() %>%
            mutate_fit2lod(lob = as.numeric(params$lob))

        # --- Stage 3: Share (Format Outputs) ---
        # 4.3 基本參數表
        stage3_share <- stage2_analyze %>%
            select(
                dev_lot,
                lot_k,
                lot_n_tot,
                lot_cp
            ) %>%
            tidyr::nest(
                analyze = everything()
            ) %>%
            mutate_analyze2ft()

        # 4.4 Fit 1 Table
        stage3_share_fit1 <- stage2_analyze %>%
            select(
                dev_lot,
                fit1_summary_tidy,
                lod1
            ) %>%
            tidyr::unnest(fit1_summary_tidy) %>%
            tidyr::nest(
                fit_summary_tidy = everything()
            ) %>%
            mutate_fit_summary_tidy2ft()

        # 4.5 Fit 2 Table
        stage3_share_fit2 <- stage2_analyze %>%
            select(
                dev_lot,
                fit2_summary_tidy,
                lod2
            ) %>%
            tidyr::unnest(fit2_summary_tidy) %>%
            tidyr::nest(
                fit_summary_tidy = everything()
            ) %>%
            mutate_fit_summary_tidy2ft()

        # 4.6 Fit 3 Table
        stage3_share_fit3 <- stage2_analyze %>%
            select(
                dev_lot,
                fit3_summary_tidy,
                lod3 # 注意: [2] 原始碼此處寫 lod2 應為筆誤，根據 [1] 邏輯修正為 lod3
            ) %>%
            tidyr::unnest(fit3_summary_tidy) %>%
            tidyr::nest(
                fit_summary_tidy = everything()
            ) %>%
            mutate_fit_summary_tidy2ft()

        # --- Stage 4: Reference Table Generator ---
        # 建立 Reference 清單，類似 [6] 的做法
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "purrr", "broom", "tidyr")

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                if (!(pkg %in% .packages())) library(pkg, character.only = TRUE)
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

        # 回傳所有需要的物件 List
        list(
            raw_ft = stage1_import$raw_ft[[1]],
            raw_plot = stage1_import$raw_plot[[1]],
            basic_ft = stage3_share$ft[[1]],
            fit1_ft = stage3_share_fit1$ft[[1]],
            fit2_ft = stage3_share_fit2$ft[[1]],
            fit3_ft = stage3_share_fit3$ft[[1]],
            ref_ft = ref_ft
        )
    })

    # --- Output Render & Download Handlers ---

    # 1. Raw Table
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })
    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("raw_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    # 2. Plot
    output$out_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })
    output$dl_plot <- downloadHandler(
        filename = function() {
            paste0("lod_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 8, dpi = 300)
        }
    )

    # 3. Basic Analysis Stats
    output$out_basic_ft <- renderUI({
        req(analysis_results())
        analysis_results()$basic_ft %>% flextable::htmltools_value()
    })
    output$dl_basic_ft <- downloadHandler(
        filename = function() {
            paste0("basic_stats_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$basic_ft, path = file)
        }
    )

    # 4. Fit 1 Table
    output$out_fit1_ft <- renderUI({
        req(analysis_results())
        analysis_results()$fit1_ft %>% flextable::htmltools_value()
    })
    output$dl_fit1_ft <- downloadHandler(
        filename = function() {
            paste0("fit1_lod_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$fit1_ft, path = file)
        }
    )

    # 5. Fit 2 Table
    output$out_fit2_ft <- renderUI({
        req(analysis_results())
        analysis_results()$fit2_ft %>% flextable::htmltools_value()
    })
    output$dl_fit2_ft <- downloadHandler(
        filename = function() {
            paste0("fit2_lod_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$fit2_ft, path = file)
        }
    )

    # 6. Fit 3 Table
    output$out_fit3_ft <- renderUI({
        req(analysis_results())
        analysis_results()$fit3_ft %>% flextable::htmltools_value()
    })
    output$dl_fit3_ft <- downloadHandler(
        filename = function() {
            paste0("fit3_lod_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$fit3_ft, path = file)
        }
    )

    # 7. References
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

