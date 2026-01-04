source("conf_toolkit/lib_load_package.R") # [4]

pkg_lst <- c(
    "shiny",
    "patchwork", # 用於拼圖 [1]
    "broom" # 用於整理模型結果 [2]
)

pacman::p_load(char = pkg_lst)

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5] (格式設定主題)
source("conf_toolkit/lib_format_flextable.R") # [3] (表格格式化工具)
source("general_lod_probit.R") # [2] (核心計算與繪圖函數)

# --- 2. 使用者定義變數 (可事後修改分頁名稱) ---
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_RAW_PLOT <- "Raw Data Plot"
TAB_TITLE_LOD_TABLE <- "LoD Analysis Result"
TAB_TITLE_FIT_PLOT <- "Regression Fit Plot"
TAB_TITLE_REF <- "References"

# Github 模板檔案連結
GITHUB_FILE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_lod_probit.xlsx"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("LoD Probit Analysis App"),
    sidebarLayout(
        sidebarPanel(
            # [Feature 1] 下載範例檔連結
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download Template from GitHub"
            ),
            br(), br(),

            # [Feature 2] 檔案上傳
            fileInput("file_upload", "Upload Excel File (Sheet: data)",
                accept = c(".xlsx")
            ),
            hr(),
            h4("Analysis Parameters"),

            # [Feature 3] 使用者輸入變數與說明文字
            textInput("input_test_name", "Test Name / Device Name", value = "Subject Device"),
            helpText("The name of the device or reagent lot being tested."),
            textInput("input_unit", "Concentration Unit", value = "mg/dL"),
            helpText("Unit of measurement for the analyte (e.g., mg/dL, copies/mL)."),
            numericInput("input_beta", "Beta (Type II Error Rate)", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
            helpText("Probability of false negative. Usually 0.05 for 95% LoD."),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                # [Feature 4.1] 原始資料表
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),

                # [Feature 4.2] 原始資料二維圖
                tabPanel(
                    TAB_TITLE_RAW_PLOT,
                    br(),
                    downloadButton("dl_raw_plot", "Download Plot (.png)"),
                    br(), br(),
                    # 注意: 根據 general_lod_probit.R 邏輯，這裡主要是 Jitter Plot
                    plotOutput("out_raw_plot", height = "600px")
                ),

                # [Feature 4.3] LoD 分析結果表
                tabPanel(
                    TAB_TITLE_LOD_TABLE,
                    br(),
                    downloadButton("dl_lod_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_lod_ft")
                ),

                # [Feature 4.4] Probit Regression 圖
                tabPanel(
                    TAB_TITLE_FIT_PLOT,
                    br(),
                    downloadButton("dl_fit_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_fit_plot", height = "800px")
                ),

                # [Feature 5] 參考文獻
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

        # 讀取使用者參數
        params <- list(
            unit = input$input_unit,
            test_name = input$input_test_name,
            beta = input$input_beta
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # 確保必要套件已載入 (如 patchwork)
        if (!require("patchwork")) install.packages("patchwork")

        # --- Stage 1: Import & Format (參照 lod_probit.R [1]) ---
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            # 格式整理: 將 conc 對應為原本邏輯中的 sample，並設定因子
            transmute(
                dev_lot = factor(dev_lot),
                day = factor(day),
                conc = as.numeric(conc),
                test_hit = as.numeric(test_hit),
                test_total = as.numeric(test_total)
            ) %>%
            arrange(
                dev_lot,
                conc,
                day
            ) %>%
            relocate(
                conc,
                .before = "day"
            ) %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate(
                # 這裡為了簡化App運算，保留 lod_probit.R 的核心結構但不一定需要 summary 輸出
                raw = purrr::map(
                    raw,
                    function(df_raw) {
                        df_raw %>%
                            mutate(
                                hit_rate_daily = test_hit / test_total
                            )
                    }
                )
            ) %>%
            # 來自 general_lod_probit.R [2]
            mutate_raw2ft(
                unit = params$unit,
                test_name = params$test_name
            ) %>%
            mutate_raw2plot(
                unit = params$unit,
                test_name = params$test_name
            )

        # --- Stage 2: Analyze (參照 lod_probit.R [1]) ---
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            mutate(
                raw = purrr::map(
                    raw,
                    function(df_raw) {
                        df_raw %>%
                            group_by(
                                dev_lot,
                                conc
                            ) %>%
                            summarize(
                                test_hit_sum = sum(test_hit),
                                test_total_sum = sum(test_total),
                                .groups = "drop" # 避免 dplyr 警告
                            ) %>%
                            mutate(
                                hit_rate = test_hit_sum / test_total_sum
                            )
                    }
                )
            ) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = colnames(.)[colnames(.) != "dev_lot"]
            ) %>%
            # 呼叫 general_lod_probit.R 中的核心運算 [2]
            mutate_raw2fit() %>%
            mutate_fit2goodness_of_fit() %>%
            mutate_fit2lod(beta = params$beta)

        # --- Stage 3: Share Result (準備輸出物件) ---

        # 3.1 LoD 結果表
        stage3_share_regression <- stage2_analyze %>%
            select(
                dev_lot,
                fit_summary_tidy,
                goodness_of_fit,
                lot_lod
            ) %>%
            tidyr::unnest(fit_summary_tidy) %>%
            tidyr::nest(
                fit_summary_tidy = everything()
            ) %>%
            mutate_fitsummary2ft() # [2]

        # 3.2 Regression Plot (包含 Probit 曲線)
        stage3_plot_data <- stage2_analyze %>%
            select(
                dev_lot,
                raw
            ) %>%
            mutate_raw2fitplot() # [2] (注意: 這裡產生的是 list of ggplots)

        # 使用 Patchwork 組合圖形 (如 lod_probit.R [1] 所示)
        final_fitplot <- patchwork::wrap_plots(stage3_plot_data$fitplot, ncol = 1)


        # --- Stage 4: References Table ---
        # 列出 LoD 分析相關套件
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "broom", "patchwork")

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
                    return("Not installed")
                }

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
            theme_box() # 使用 lib_officeverse.R 定義的主題

        # 回傳所有結果的 List
        # 注意：假設只有一組主要數據，提取 list 的第一個元素 [[1]]
        list(
            raw_ft = stage1_import$raw_ft[[1]],
            # raw_plot 在 general_lod_probit.R 回傳時通常是一個 list of plots，這裡取第一個
            raw_plot = stage1_import$raw_plot[[1]],
            lod_ft = stage3_share_regression$fit_summary_tidy_ft[[1]],
            fit_plot = final_fitplot,
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

    # 2. Raw Data Plot (Jitter Plot)
    output$out_raw_plot <- renderPlot({
        req(analysis_results())
        # 根據 general_lod_probit.R [2]，mutate_raw2plot 回傳的是 facet_wrap 圖
        analysis_results()$raw_plot
    })

    output$dl_raw_plot <- downloadHandler(
        filename = function() {
            paste0("raw_data_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 6, dpi = 300)
        }
    )

    # 3. LoD Result Table
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

    # 4. Regression Fit Plot
    output$out_fit_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$fit_plot
    })

    output$dl_fit_plot <- downloadHandler(
        filename = function() {
            paste0("probit_fit_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$fit_plot, width = 8, height = 10, dpi = 300)
        }
    )

    # 5. References Table
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
