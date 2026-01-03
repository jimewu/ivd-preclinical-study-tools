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
# 依照 stability.R 中的定義載入必要的函式庫與設定檔 [5]
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
            # 功能 1: 連結到 GitHub 模板 [User Requirement 1]
            tags$div(
                style = "margin-bottom: 20px;",
                tags$a(
                    href = "https://github.com/your-repo/template.xlsx", # 請替換成實際連結
                    class = "btn btn-info",
                    target = "_blank",
                    "Download Data Template (Excel)"
                )
            ),

            # 功能 2: 上傳 Excel 檔案 [User Requirement 2]
            fileInput("file_input", "Upload Completed Data (Excel)",
                accept = c(".xlsx"),
                placeholder = "Select the data file"
            ),
            helpText("Ensure the data is in a sheet named 'data'."),
            hr(),
            h4("Parameters Settings"),

            # 功能 3: 使用者輸入變數與說明文字 [User Requirement 3]
            textInput("analyte_name", "Analyte Name", value = "Glucose"),
            helpText("The name of the substance being analyzed (e.g., Glucose, Cholesterol)."),
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
            # 功能 4, 5, 6: 結果分頁 [User Requirement 4, 5, 6]
            tabsetPanel(
                id = "main_tabs",

                # 4.1 原始資料表
                tabPanel(
                    "Raw Data List",
                    uiOutput("raw_ft_ui")
                ),

                # 4.2 原始資料圖
                tabPanel(
                    "Raw Data Plot",
                    plotOutput("raw_plot_output", height = "600px")
                ),

                # 4.3 Regression 結果表
                tabPanel(
                    "Regression Analysis",
                    uiOutput("regression_ft_ui")
                ),

                # 4.4 Drift vs Day 1 表
                tabPanel(
                    "Drift Analysis (vs Day 1)",
                    uiOutput("drift_z0_ft_ui")
                ),

                # 4.5 Drift vs Intercept 表
                tabPanel(
                    "Drift Analysis (vs Intercept)",
                    uiOutput("drift_int_ft_ui")
                ),

                # 5. Reference List
                tabPanel(
                    "References (Packages)",
                    verbatimTextOutput("package_refs")
                )
            )
        )
    )
)

# 3. Server 定義
server <- function(input, output, session) {
    # Reactive block: 執行主要分析運算
    # 邏輯主要改寫自 stability.R 的 Stage 1 到 Stage 3 [5]
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_input)

        # 讀取參數
        params <- list(
            analyte_name = input$analyte_name,
            unit = input$unit,
            test_name = input$test_name,
            perc_allowable_drift = input$perc_allowable_drift
        )

        file_path <- input$file_input$datapath
        sheet_name <- "data"

        # * Stage 1: Import
        # 資料讀取與前處理邏輯詳見 source id="5"
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
        # 呼叫核心運算函式庫 general_stability.R [4]
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

        # 回傳所有結果的 List
        list(
            stage1 = stage1_import,
            regression = stage3_share_regression,
            drift_z0 = stage3_share_drift_vs_z0,
            drift_int = stage3_share_drift_vs_intercept
        )
    })

    # --- Outputs Rendering ---

    # 4.1 stage1_import$raw_ft[1][[1]] (原始資料表)
    output$raw_ft_ui <- renderUI({
        res <- analysis_results()
        # 取得 list 中第一個 flextable [5]
        ft <- res$stage1$raw_ft[[1]]
        htmltools_value(ft)
    })

    # 4.2 stage1_import$raw_plot[1][[1]] (原始資料圖)
    output$raw_plot_output <- renderPlot({
        res <- analysis_results()
        # 取得 list 中第一個 plot [4], [5]
        plot_obj <- res$stage1$raw_plot[[1]]
        print(plot_obj)
    })

    # 4.3 stage3_share_regression (regression結果表)
    output$regression_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$regression)
    })

    # 4.4 stage3_share_drift_vs_z0
    output$drift_z0_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$drift_z0)
    })

    # 4.5 stage3_share_drift_vs_intercept
    output$drift_int_ft_ui <- renderUI({
        res <- analysis_results()
        htmltools_value(res$drift_int)
    })

    # 5. Reference List
    # 自動列出 session 中載入的套件引用資訊
    output$package_refs <- renderText({
        # 使用 lib_load_package.R 中定義的 pkg_lst 或是當前載入的套件
        pkg_list <- c(
            "shiny", "readxl", "dplyr", "flextable", "ggplot2",
            "officer", "broom", "purrr", "tidyr"
        )

        refs <- sapply(pkg_list, function(p) {
            if (requireNamespace(p, quietly = TRUE)) {
                cit <- citation(p)
                paste0("Package: ", p, "\n", format(cit, style = "text"), "\n\n")
            } else {
                ""
            }
        })
        paste(refs, collapse = "")
    })
}

# 啟動 Shiny App
shinyApp(ui = ui, server = server)
