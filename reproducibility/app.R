source("conf_toolkit/lib_load_package.R")

pkg_lst <- c(
    "shiny"
)

pacman::p_load(char = pkg_lst)


# --- 1. 初始化設定與 Helper 載入 ---
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")
source("general_precision.R") # 確保使用新版 Library

# --- 2. 使用者定義 - 常數 ---
TAB_TITLE_RAW_TABLE <- "Raw Data Table"
TAB_TITLE_PLOT <- "Raw Data Plot"
TAB_TITLE_VCA_TABLE <- "ANOVA Result Table"
TAB_TITLE_REF <- "References" # 新增 Reference 標題
GITHUB_FILE_LINK <- "https://github.com/jimewu/ivd-preclinical-study-tools/releases/download/v1.0/template_reproducibility.xlsx"

# --- 3. UI 介面 ---
ui <- fluidPage(
    titlePanel("Reproducibility Analysis App"),
    sidebarLayout(
        sidebarPanel(
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
            helpText("Required columns: 'conc', 'day', 'replicate', 'y' and ONE condition column (e.g., 'site', 'lot')."),
            hr(),
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    TAB_TITLE_RAW_TABLE,
                    br(),
                    downloadButton("dl_raw_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_raw_ft")
                ),
                tabPanel(
                    TAB_TITLE_PLOT,
                    br(),
                    downloadButton("dl_plot", "Download Plot (.png)"),
                    br(), br(),
                    plotOutput("out_plot", height = "600px")
                ),
                tabPanel(
                    TAB_TITLE_VCA_TABLE,
                    br(),
                    downloadButton("dl_vca_ft", "Download Table (.docx)"),
                    br(), br(),
                    uiOutput("out_vca_ft")
                ),
                # 新增 Reference Tab [2]
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
    analysis_results <- eventReactive(input$run_analysis, {
        req(input$file_upload)

        params <- list(
            analyte_name = input$input_analyte,
            unit = input$input_unit
        )

        file_path <- input$file_upload$datapath
        sheet_name <- "data"

        if (!require("VCA")) install.packages("VCA")

        # --- Stage 0: 讀取並自動偵測欄位 ---
        raw_df_initial <- read_excel(path = file_path, sheet = sheet_name)

        # 1. 檢查並排除標準欄位
        cols_found <- colnames(raw_df_initial)
        cols_lower <- tolower(cols_found)
        std_cols <- c("conc", "day", "replicate", "y")

        custom_col_idx <- which(!cols_lower %in% std_cols)

        if (length(custom_col_idx) == 0) {
            stop("Column Error: Could not find a condition column (like site, lot). Found only standard columns.")
        }

        # 2. 取得實際的 Group 變數名稱
        condition_var <- cols_found[custom_col_idx[1]]

        # --- Stage 1: Import & Format ---
        stage1_import <- raw_df_initial %>%
            transmute(
                sample = factor(conc),
                day = factor(day),
                replicate = factor(replicate),
                y = as.numeric(y),
                # 動態處理 condition,確保它是 factor
                !!sym(condition_var) := factor(!!sym(condition_var))
            ) %>%
            tidyr::nest(raw = everything()) %>%
            # 使用我們新修正的 helper (使用 values=list 方法,更穩定)
            mutate_raw2ft(dev_unit = params$unit, group_var = condition_var) %>%
            mutate_raw2plot(
                analyte_name = params$analyte_name,
                dev_unit = params$unit,
                group_var = condition_var
            )

        # --- Stage 2: VCA Analysis ---
        # 公式: y ~ Condition / day / replicate
        vca_formula_str <- paste("y ~", condition_var, "/ day / replicate")
        vca_formula <- as.formula(vca_formula_str)

        stage2_analyze <- stage1_import %>%
            select(raw) %>%
            tidyr::unnest(raw) %>%
            tidyr::nest(raw = colnames(.)[colnames(.) != "sample"]) %>%
            mutate(
                VCA = purrr::map(
                    raw,
                    function(sub_data) {
                        # 使用 try 避免單一 sample 計算失敗導致整個 app 崩潰
                        tryCatch(
                            {
                                VCA::anovaVCA(
                                    form = vca_formula,
                                    Data = as.data.frame(sub_data)
                                )
                            },
                            error = function(e) {
                                warning(paste("VCA Error:", e$message))
                                return(NULL)
                            }
                        )
                    }
                )
            ) %>%
            # 移除計算失敗的項目
            filter(!sapply(VCA, is.null)) %>%
            mutate_VCA2df()

        # --- Stage 3: Share Result ---
        stage3_share <- stage2_analyze %>%
            select(sample, VCA_df) %>%
            tidyr::unnest(VCA_df) %>%
            tidyr::nest(VCA_df = colnames(.)) %>%
            mutate_VCA_df2ft()

        # --- Stage 4: Reference Table Generator ---
        # 參考 app2.R 的邏輯加入套件列表與引用生成 [2]
        pkg_list <- c("base", "shiny", "readxl", "ggplot2", "flextable", "dplyr", "VCA")

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

        list(
            raw_ft = stage1_import$raw_ft[[1]],
            raw_plot = stage1_import$raw_plot[[1]],
            vca_ft = stage3_share$VCA_ft[[1]],
            ref_ft = ref_ft # 回傳 reference table
        )
    })

    # Output Render
    output$out_raw_ft <- renderUI({
        req(analysis_results())
        analysis_results()$raw_ft %>% flextable::htmltools_value()
    })

    output$dl_raw_ft <- downloadHandler(
        filename = function() {
            paste0("repro_raw_data_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$raw_ft, path = file)
        }
    )

    output$out_plot <- renderPlot({
        req(analysis_results())
        analysis_results()$raw_plot
    })

    output$dl_plot <- downloadHandler(
        filename = function() {
            paste0("repro_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            ggsave(file, plot = analysis_results()$raw_plot, width = 10, height = 8, dpi = 300)
        }
    )

    output$out_vca_ft <- renderUI({
        req(analysis_results())
        analysis_results()$vca_ft %>% flextable::htmltools_value()
    })

    output$dl_vca_ft <- downloadHandler(
        filename = function() {
            paste0("repro_vca_result_", Sys.Date(), ".docx")
        },
        content = function(file) {
            save_as_docx(analysis_results()$vca_ft, path = file)
        }
    )

    # 新增 References 的 Output Render 與 Download Handler [2]
    output$out_ref_ft <- renderUI({
        req(analysis_results())
        analysis_results()$ref_ft %>% flextable::htmltools_value()
    })

    output$dl_ref_ft <- downloadHandler(
        filename = function() {
            paste0("repro_references_", Sys.Date(), ".docx")
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
