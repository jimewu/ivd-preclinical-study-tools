source("conf_toolkit/lib_load_package.R") # [4]
library(shiny)
library(readxl)
library(ggplot2)
library(flextable)
library(dplyr)
library(tidyr)
library(tibble)

# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R") # [5]
source("conf_toolkit/lib_format_flextable.R") # [3]
source("general_method_comparison.R") # [1]

# 檢查並安裝分析所需套件 (邏輯來自 [2])
if (!require("EnvStats")) install.packages("EnvStats")
if (!require("mcr")) install.packages("mcr")

# --- 2. 使用者定義常數 ---
GITHUB_FILE_LINK <- "https://github.com/Start-S/Method_Comparison_Shiny_App/raw/main/method_comparison_sample_data.xlsx"

# 分頁標題設定
TAB_1_RAW_TABLE <- "Raw Data Table"
TAB_2_RAW_PLOT <- "Raw Data Scatter Plot"
TAB_3_DIFF_PLOT <- "Difference Plot"
TAB_4_PERC_DIFF_PLOT <- "% Difference Plot"
TAB_5_ROSNER <- "Rosner's Test (QC)"
TAB_6_COMPARE_FIT <- "Regression Comparison Plot"
TAB_7_BEST_REG_PLOT <- "Best Regression Plot"
TAB_8_COEF_TABLE <- "Regression Coefficients"
TAB_9_BIAS_TABLE <- "Bias at Critical Conc."
TAB_10_REF <- "References"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Method Comparison Analysis App"),
    sidebarLayout(
        sidebarPanel(
            # 1. 連結到 GitHub 模板
            tags$a(
                href = GITHUB_FILE_LINK, target = "_blank",
                class = "btn btn-info",
                "Download Template from GitHub"
            ),
            br(), br(),

            # 2. 檔案上傳
            fileInput("file_upload", "Upload Excel File (Sheet: data)",
                accept = c(".xlsx")
            ),
            hr(),
            h4("Analysis Parameters"),

            # 3. 參數設定
            textInput("input_analyte", "Analyte Name", value = "Glucose"),
            helpText("分析項目的名稱 (例如: Glucose, Cholesterol)。"),
            textInput("input_unit", "Unit", value = "mg/dL"),
            helpText("測量單位。"),
            textInput("input_loq", "LoQ (Limit of Quantitation)", value = "1.5"),
            helpText("定量極限值，低於此數值的結果將被標記。"),
            textInput("input_ref_name", "Reference Method Name", value = "Reference Method"),
            helpText("對照組 (X軸) 的方法名稱。"),
            textInput("input_test_name", "Test Method Name", value = "Subject Device"),
            helpText("實驗組 (Y軸) 的方法名稱。"),
            numericInput("input_err_ratio", "Error Ratio", value = 1, min = 0, step = 0.1),
            helpText("兩方法間的誤差比率 (Error Ratio)。"),
            numericInput("input_alpha", "Alpha (Confidence Level)", value = 0.05, min = 0.01, max = 0.2, step = 0.01),
            helpText("顯著水準 (例如 0.05 代表 95% 信賴區間)。"),
            numericInput("input_conc_crit", "Critical Concentration", value = 50),
            helpText("用於計算 Bias 的關鍵濃度點。"),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                # 4.1 原始資料表
                tabPanel(
                    TAB_1_RAW_TABLE, br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"), br(), br(),
                    uiOutput("out_raw_ft")
                ),
                # 4.2 二維散佈圖
                tabPanel(
                    TAB_2_RAW_PLOT, br(),
                    downloadButton("dl_raw_plot", "Download Plot (.png)"), br(), br(),
                    plotOutput("out_raw_plot", height = "600px")
                ),
                # 4.3 Difference Plot
                tabPanel(
                    TAB_3_DIFF_PLOT, br(),
                    downloadButton("dl_diff_plot", "Download Plot (.png)"), br(), br(),
                    plotOutput("out_diff_plot", height = "600px")
                ),
                # 4.4 % Difference Plot
                tabPanel(
                    TAB_4_PERC_DIFF_PLOT, br(),
                    downloadButton("dl_perc_diff_plot", "Download Plot (.png)"), br(), br(),
                    plotOutput("out_perc_diff_plot", height = "600px")
                ),
                # 4.5 Rosner Test
                tabPanel(
                    TAB_5_ROSNER, br(),
                    downloadButton("dl_rosner_ft", "Download Table (.docx)"), br(), br(),
                    uiOutput("out_rosner_ft")
                ),
                # 4.6 Compare Fit Plot
                tabPanel(
                    TAB_6_COMPARE_FIT, br(),
                    downloadButton("dl_compare_fit", "Download Plot (.png)"), br(), br(),
                    plotOutput("out_compare_fit", height = "600px")
                ),
                # 4.7 Best Regression Plot
                tabPanel(
                    TAB_7_BEST_REG_PLOT, br(),
                    downloadButton("dl_best_reg", "Download Plot (.png)"), br(), br(),
                    plotOutput("out_best_reg", height = "600px")
                ),
                # 4.8 Coefficients Table
                tabPanel(
                    TAB_8_COEF_TABLE, br(),
                    downloadButton("dl_coef_ft", "Download Table (.docx)"), br(), br(),
                    uiOutput("out_coef_ft")
                ),
                # 4.9 Bias Table
                tabPanel(
                    TAB_9_BIAS_TABLE, br(),
                    downloadButton("dl_bias_ft", "Download Table (.docx)"), br(), br(),
                    uiOutput("out_bias_ft")
                ),
                # 4.10 Reference Tab (Citation)
                tabPanel(
                    TAB_10_REF, br(),
                    downloadButton("dl_ref_ft", "Download Table (.docx)"), br(), br(),
                    uiOutput("out_ref_ft")
                )
            )
        )
    )
)

# --- 4. Server 邏輯 ---
server <- function(input, output, session) {
    # 定義 Analysis 核心邏輯
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        # 讀取使用者參數
        params <- list(
            analyte_name = input$input_analyte,
            unit = input$input_unit,
            loq = input$input_loq,
            ref_name = input$input_ref_name,
            test_name = input$input_test_name,
            err_ratio = input$input_err_ratio,
            alpha = input$input_alpha,
            conc_crit = input$input_conc_crit
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        # * Stage 1: Import & Basic Plots
        # 參考 [2], 調用 [1]
        stage1_import <- read_excel(
            path = file_path,
            sheet = sheet_name,
            col_names = TRUE
        ) %>%
            mutate(across(everything(), as.numeric)) %>%
            tidyr::nest(raw = everything()) %>%
            raw2sum(loq = params$loq) %>%
            raw2ft(
                loq = params$loq,
                ref_name = params$ref_name,
                test_name = params$test_name,
                unit = params$unit
            ) %>%
            raw2plot(
                ref_name = params$ref_name,
                test_name = params$test_name,
                unit = params$unit
            ) %>%
            raw2_difference_plot(
                ref_name = params$ref_name,
                unit = params$unit
            ) %>%
            raw2perc_difference_plot(
                ref_name = params$ref_name
            )

        # * Stage 2: QC (Rosner Test)
        # 參考 [2], 調用 [1]
        stage2_qc <- stage1_import %>%
            select(-raw_ft, -raw_plot) %>%
            raw2rosner_test_ft(alpha = params$alpha)

        # * Stage 3: Analyze (Regression)
        # 參考 [2]
        stage3_analyze <- stage1_import %>%
            select(raw) %>%
            mutate(
                regression_method_full = "",
                regression_method = "",
                ci_method = ""
            ) %>%
            tidyr::nest(
                regression_parm = c("regression_method_full", "regression_method", "ci_method")
            ) %>%
            mutate(
                regression_parm = purrr::map(
                    regression_parm,
                    function(x) {
                        tibble(
                            regression_method_full = c(
                                "Ordinary Linear Regression", "Deming Regression",
                                "Weighted Ordinary Linear Regression", "Weighted Deming Regression",
                                "Passing-Bablok Regression"
                            ),
                            regression_method = c("LinReg", "Deming", "WLinReg", "WDeming", "PaBa"),
                            ci_method = c("bootstrap", "analytical", "bootstrap", "bootstrap", "bootstrap")
                        )
                    }
                )
            ) %>%
            tidyr::unnest(regression_parm) %>%
            tidyr::nest(regression_parm = c("regression_method", "ci_method")) %>%
            raw2mcreg(
                err_ratio = params$err_ratio,
                alpha = params$alpha,
                ref_name = params$ref_name,
                test_name = params$test_name
            ) %>%
            mutate(
                mcreg_coef = purrr::map(
                    mcreg,
                    function(x) {
                        mcr::getCoefficients(x) %>%
                            as.data.frame() %>%
                            select(-SE) %>%
                            cbind(item = rownames(.), .) %>%
                            return()
                    }
                )
            ) %>%
            mcreg_coef2ft(ncol_extra = 0) %>%
            mutate(
                if_viable = purrr::map_lgl(
                    mcreg_coef,
                    function(x) {
                        if_intercept <- all(x["Intercept", "LCI"] < 0, x["Intercept", "UCI"] > 0)
                        if_slope <- all(x["Slope", "LCI"] < 1, x["Slope", "UCI"] > 1)
                        return(all(if_intercept, if_slope))
                    }
                )
            ) %>%
            mutate(
                ci_area = purrr::map_dbl(
                    mcreg_coef,
                    function(x) {
                        len_intercept <- x["Intercept", "UCI"] - x["Intercept", "LCI"]
                        len_slope <- x["Slope", "UCI"] - x["Slope", "LCI"]
                        return(len_intercept * len_slope)
                    }
                )
            ) %>%
            arrange(desc(if_viable), ci_area) %>%
            cbind(., if_best = c(TRUE, rep(FALSE, 4))) %>%
            mcreg2bias(conc_crit = params$conc_crit) %>%
            mutate(
                bias_ft = purrr::map(
                    bias,
                    function(bias) {
                        if (is.data.frame(bias)) {
                            bias %>%
                                mutate_if(is.numeric, function(x) round(x, 3)) %>%
                                flextable() %>%
                                set_header_labels("Prop.bias(%)" = "Proportional Bias (%)", LCI = "95% CI Lwr.", UCI = "95% CI Upr.") %>%
                                align(align = "center", part = "all") %>%
                                return()
                        } else {
                            return(NA)
                        }
                    }
                )
            )

        # * Stage 3C: Aggregate Regression Coefficients
        stage3C_reg_ft <- stage3_analyze %>%
            select(regression_method_full, mcreg_coef) %>%
            tidyr::unnest(mcreg_coef) %>%
            tidyr::nest(mcreg_coef = colnames(.)) %>%
            mcreg_coef2ft(ncol_extra = 1)

        # * Stage: Reference Table Generator
        # 列出此 App 使用的關鍵套件
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "mcr", "EnvStats")

        ref_df <- tibble(
            Package = pkg_list,
            Citation = purrr::map_chr(pkg_list, function(pkg) {
                cit <- citation(pkg)
                # 簡單抓取第一筆引用格式化為字串
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

        # 回傳所有結果 List
        list(
            raw_ft = stage1_import$raw_ft[[1]],
            raw_plot = stage1_import$raw_plot[[1]],
            diff_plot = stage1_import$raw_difference_plot[[1]],
            perc_diff_plot = stage1_import$raw_perc_difference_plot[[1]],
            rosner_ft = stage2_qc$rosner_test_ft[[1]],
            mcreg_objects = stage3_analyze$mcreg,
            best_reg_object = stage3_analyze$mcreg[[1]],
            coef_ft = stage3C_reg_ft$mcreg_coef_ft[[1]],
            bias_ft = stage3_analyze$bias_ft[[1]],
            ref_ft = ref_ft, # 新增 Reference table
            params = params
        )
    })

    # --- Output Renders & Download Handlers ---

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

    # 2. Raw Plot
    output$out_raw_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })
    output$dl_raw_plot <- downloadHandler(
        filename = function() {
            paste0("raw_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 8, height = 6)
        }
    )

    # 3. Difference Plot
    output$out_diff_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$diff_plot
    })
    output$dl_diff_plot <- downloadHandler(
        filename = function() {
            paste0("diff_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$diff_plot, width = 8, height = 6)
        }
    )

    # 4. % Difference Plot
    output$out_perc_diff_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$perc_diff_plot
    })
    output$dl_perc_diff_plot <- downloadHandler(
        filename = function() {
            paste0("perc_diff_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$perc_diff_plot, width = 8, height = 6)
        }
    )

    # 5. Rosner Test Table
    output$out_rosner_ft <- renderUI({
        req(analysis_results())
        analysis_results()$rosner_ft %>% flextable::htmltools_value()
    })
    output$dl_rosner_ft <- downloadHandler(
        filename = function() {
            paste0("rosner_qc_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$rosner_ft, path = file)
        }
    )

    # 6. Regression Comparison Plot (Base R Plot)
    output$out_compare_fit <- renderPlot({
        req(analysis_results())
        objs <- analysis_results()$mcreg_objects
        mcr::compareFit(objs[[1]], objs[[2]], objs[[3]], objs[[4]], objs[[5]])
    })
    output$dl_compare_fit <- downloadHandler(
        filename = function() {
            paste0("compare_fit_", Sys.Date(), ".png")
        },
        content = function(file) {
            png(file, width = 2400, height = 1800, res = 300)
            objs <- analysis_results()$mcreg_objects
            mcr::compareFit(objs[[1]], objs[[2]], objs[[3]], objs[[4]], objs[[5]])
            dev.off()
        }
    )

    # 7. Best Regression Plot (Base R Plot)
    output$out_best_reg <- renderPlot({
        req(analysis_results())
        p_args <- analysis_results()$params
        mcr::MCResult.plot(
            analysis_results()$best_reg_object,
            add.legend = FALSE, x.lab = p_args$ref_name, y.lab = p_args$test_name
        )
    })
    output$dl_best_reg <- downloadHandler(
        filename = function() {
            paste0("best_reg_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            p_args <- analysis_results()$params
            png(file, width = 2400, height = 1800, res = 300)
            mcr::MCResult.plot(
                analysis_results()$best_reg_object,
                add.legend = FALSE, x.lab = p_args$ref_name, y.lab = p_args$test_name
            )
            dev.off()
        }
    )

    # 8. Coefficients Table
    output$out_coef_ft <- renderUI({
        req(analysis_results())
        analysis_results()$coef_ft %>% flextable::htmltools_value()
    })
    output$dl_coef_ft <- downloadHandler(
        filename = function() {
            paste0("coef_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$coef_ft, path = file)
        }
    )

    # 9. Bias Table
    output$out_bias_ft <- renderUI({
        req(analysis_results())
        analysis_results()$bias_ft %>% flextable::htmltools_value()
    })
    output$dl_bias_ft <- downloadHandler(
        filename = function() {
            paste0("bias_table_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$bias_ft, path = file)
        }
    )

    # 10. References Table
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
