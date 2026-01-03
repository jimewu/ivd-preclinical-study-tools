library(shiny)
library(readxl)
library(dplyr)
library(flextable)
library(ggplot2)
library(officer)
library(broom)
library(purrr)
library(tidyr)
library(tibble)

# 1. 載入外部資源
# 依照 stability.R 中的定義載入必要的函式庫與設定檔
# 請確保路徑與你的實際檔案位置相符
if (file.exists("conf_toolkit/lib_load_package.R")) source("conf_toolkit/lib_load_package.R")
if (file.exists("conf_toolkit/lib_officeverse.R")) source("conf_toolkit/lib_officeverse.R")
if (file.exists("conf_toolkit/lib_format_flextable.R")) source("conf_toolkit/lib_format_flextable.R")
if (file.exists("general_stability.R")) source("general_stability.R")

# 2. UI 定義
ui <- fluidPage(

    # 應用程式標題
    titlePanel("Stability Analysis Application"),
    sidebarLayout(
        sidebarPanel(
            # 功能 1: 連結到 GitHub 模板
            tags$div(
                style = "margin-bottom: 20px;",
                tags$a(
                    href = "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_stability.xlsx",
                    class = "btn btn-info",
                    target = "_blank",
                    "Download Data Template (Excel)"
                )
            ),

            # 功能 2: 上傳 Excel 檔案
            fileInput("file_input", "Upload Completed Data (Excel)",
                accept = c(".xlsx"),
                placeholder = "Select the data file"
            ),
            helpText("Ensure the data is in a sheet named 'data'."),
            hr(),
            h4("Parameters Settings"),

            # 功能 3: 使用者輸入變數與說明文字
            textInput("unit", "Unit", value = "mg/dL"),
            helpText("Measurement unit for the assay (e.g., mg/dL, mmol/L)."),
            textInput("test_name", "Test Name", value = "Subject Device"),
            helpText("Label for the device or method under test."),
            numericInput("perc_allowable_drift", "Allowable Drift (%)", value = 5, step = 0.5),
            helpText("The maximum percentage drift allowed from the baseline."),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            # 功能 4, 5, 6: 結果分頁
            tabsetPanel(
                id = "main_tabs",

                # 4.1 原始資料表
                tabPanel(
                    "Raw Data List",
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("raw_ft_ui")
                ),

                # 4.2 原始資料圖
                tabPanel(
                    "Raw Data Plot",
                    br(),
                    downloadButton("dl_raw_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("raw_plot_output", height = "600px")
                ),

                # 4.3 Regression 結果表
                tabPanel(
                    "Regression Analysis",
                    br(),
                    downloadButton("dl_reg_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("regression_ft_ui")
                ),

                # 4.4 Drift vs Day 1 表
                tabPanel(
                    "Drift Analysis (vs Day 1)",
                    br(),
                    downloadButton("dl_drift_z0_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("drift_z0_ft_ui")
                ),

                # 4.5 Drift vs Intercept 表
                tabPanel(
                    "Drift Analysis (vs Intercept)",
                    br(),
                    downloadButton("dl_drift_int_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("drift_int_ft_ui")
                ),

                # 5. Reference List (Modified to table)
                tabPanel(
                    "References (Packages)",
                    br(),
                    downloadButton("dl_ref_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("package_refs_ui")
                )
            )
        )
    )
)

# 3. Server 定義
server <- function(input, output, session) {
    # Reactive block: 執行主要分析運算
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_input)

        # 讀取參數
        params <- list(
            unit = input$unit,
            test_name = input$test_name,
            perc_allowable_drift = input$perc_allowable_drift
        )

        file_path <- input$file_input$datapath
        sheet_name <- "data"

        # * Stage 1: Import
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name
        ) %>%
            transmute(
                dev_lot = factor(dev_lot),
                sample = factor(sample),
                day = as.numeric(day),
                y = as.numeric(y),
                y_anchor = as.numeric(y_anchor)
            ) %>%
            mutate(
                any_has_no_anchor = any(is.na(.$y_anchor))
            ) %>%
            tidyr::nest(
                raw = everything()
            ) %>%
            mutate(
                summary = purrr::map(
                    raw,
                    function(raw) {
                        nday <- max(raw$day)
                        nsample <- length(levels(factor(raw$sample)))
                        nlot_dev <- length(levels(factor(raw$dev_lot)))
                        data.frame(nday, nsample, nlot_dev)
                    }
                )
            ) %>%
            mutate_raw2ft(unit = params$unit) %>%
            mutate_raw2plot(
                unit = params$unit,
                test_name = params$test_name
            )

        # * Stage 2: Analyze
        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(
                raw = colnames(.)[!colnames(.) %in% c("sample", "dev_lot", "any_has_no_anchor")]
            ) %>%
            arrange(dev_lot, sample) %>%
            mutate_raw2fit() %>%
            mutate_fit2predict() %>%
            mutate_predict2drift(perc_allowable_drift = params$perc_allowable_drift)

        # * Stage 3: Share (Formatting output)

        # Regression table
        stage3_share_regression <- stage2_analyze %>%
            select(dev_lot, sample, fit_summary_tidy, any_has_no_anchor, equal_replicate) %>%
            mutate(
                regression_method = ifelse(
                    equal_replicate,
                    "Ordinary Linear Regression",
                    "Weighted Least Squares Regression"
                ),
                y = ifelse(
                    any_has_no_anchor,
                    "Mean of Measurement Value",
                    "%Difference"
                )
            ) %>%
            select(-any_has_no_anchor, -equal_replicate) %>%
            relocate(regression_method, .after = sample) %>%
            relocate(y, .before = fit_summary_tidy) %>%
            tidyr::unnest(fit_summary_tidy) %>%
            mutate_regression2ft()

        # Drift vs z0 table
        stage3_share_drift_vs_z0 <- stage2_analyze %>%
            select(dev_lot, sample, perc_drift_vs_z0) %>%
            tidyr::unnest(perc_drift_vs_z0) %>%
            mutate_drift2ft()

        # Drift vs intercept table
        stage3_share_drift_vs_intercept <- stage2_analyze %>%
            select(dev_lot, sample, perc_drift_vs_intercept) %>%
            tidyr::unnest(perc_drift_vs_intercept) %>%
            mutate_drift2ft()

        # * Stage 4: References Table Generator
        # Create citation flextable similar to sample.R logic [2]
        pkg_list <- c(
            "shiny", "readxl", "dplyr", "flextable", "ggplot2",
            "officer", "broom", "purrr", "tidyr", "tibble"
        )

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                if (requireNamespace(pkg, quietly = TRUE)) {
                    cit <- citation(pkg)
                    if (length(cit) > 0) {
                        # Format citation to text logic
                        paste0(format(cit[[1]], style = "text"), collapse = " ")
                    } else {
                        "No citation available"
                    }
                } else {
                    "Package not loaded"
                }
            })
        )

        # Check if theme_box exists (from lib_format_flextable.R), otherwise use default
        ref_ft <- ref_df %>%
            flextable() %>%
            width(j = 1, width = 1.5) %>%
            width(j = 2, width = 6) %>%
            set_header_labels(Package = "R Package", Citation = "Citation Reference")

        # Apply formatting if custom function exists, else standard fit
        if (exists("theme_box")) {
            ref_ft <- ref_ft %>% theme_box()
        } else {
            ref_ft <- ref_ft %>% autofit()
        }

        # 回傳所有結果的 List
        list(
            stage1 = stage1_import,
            regression = stage3_share_regression,
            drift_z0 = stage3_share_drift_vs_z0,
            drift_int = stage3_share_drift_vs_intercept,
            ref_ft = ref_ft
        )
    })

    # --- Outputs Rendering & Downloads ---

    # 4.1 stage1_import$raw_ft (原始資料表)
    output$raw_ft_ui <- renderUI({
        res <- analysis_results()
        ft <- res$stage1$raw_ft[[1]]
        htmltools_value(ft)
    })

    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("raw_data_", Sys.Date(), ".docx")
        },
        content = function(file) {
            req(analysis_results())
            # Save flextable to docx
            ft <- analysis_results()$stage1$raw_ft[[1]]
            save_as_docx(ft, path = file)
        }
    )

    # 4.2 stage1_import$raw_plot (原始資料圖)
    output$raw_plot_output <- renderPlot({
        res <- analysis_results()
        plot_obj <- res$stage1$raw_plot[[1]]
        print(plot_obj)
    })

    output$dl_raw_plot <- downloadHandler(
        filename = function() {
            paste0("raw_data_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            req(analysis_results())
            # Save ggplot to png
            plot_obj <- analysis_results()$stage1$raw_plot[[1]]
            ggsave(file, plot = plot_obj, width = 10, height = 8, dpi = 300)
        }
    )

    # 4.3 Regression Table
    output$regression_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$regression)
    })

    output$dl_reg_ft <- downloadHandler(
        filename = function() {
            paste0("regression_analysis_", Sys.Date(), ".docx")
        },
        content = function(file) {
            req(analysis_results())
            save_as_docx(analysis_results()$regression, path = file)
        }
    )

    # 4.4 Drift vs Day 1 Table
    output$drift_z0_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$drift_z0)
    })

    output$dl_drift_z0_ft <- downloadHandler(
        filename = function() {
            paste0("drift_diff_analysis_", Sys.Date(), ".docx")
        },
        content = function(file) {
            req(analysis_results())
            save_as_docx(analysis_results()$drift_z0, path = file)
        }
    )

    # 4.5 Drift vs Intercept Table
    output$drift_int_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$drift_int)
    })

    output$dl_drift_int_ft <- downloadHandler(
        filename = function() {
            paste0("drift_intercept_analysis_", Sys.Date(), ".docx")
        },
        content = function(file) {
            req(analysis_results())
            save_as_docx(analysis_results()$drift_int, path = file)
        }
    )

    # 5. Reference List (Flextable with Download)
    output$package_refs_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$ref_ft)
    })

    output$dl_ref_ft <- downloadHandler(
        filename = function() {
            paste0("references_", Sys.Date(), ".docx")
        },
        content = function(file) {
            req(analysis_results())
            save_as_docx(analysis_results()$ref_ft, path = file)
        }
    )
}

# 啟動 Shiny App
shinyApp(
    ui = ui,
    server = server,
    options = list(
        host = "0.0.0.0",
        port = 80
    )
)
