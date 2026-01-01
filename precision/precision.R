source("conf_toolkit/lib_load_package.R")
source("conf_toolkit/lib_officeverse.R")
source("conf_toolkit/lib_format_flextable.R")

source("conf_toolkit/lib_general_precision.R")

# 定義參數 (Metadata) - 這裡模擬未來的 Shiny Input
params <- list(
    analyte_name = "Glucose",
    unit = "mg/dL"
)

# 設定檔案路徑
file_path <- "conf_data/precision_sample_data.xlsx" # 請修改為您的檔案名稱
sheet_name <- "data" # 請修改為您的分頁名稱

# 分析所需套件:如果沒安裝就安裝
if (!require("VCA")) install.packages("VCA")

stage1_import <- read_excel(
    path = file_path,
    sheet = sheet_name
) %>%
    # 格式整理：將 conc 對應為原本邏輯中的 sample，並設定因子
    transmute(
        sample = factor(conc), # 將 conc 欄位轉為 sample 欄位
        day = factor(day),
        run = factor(run),
        replicate = factor(replicate),
        y = as.numeric(y)
    ) %>%
    # 將所有資料 nest 進 'raw' 欄位，以符合原本程式碼的結構
    tidyr::nest(
        raw = everything()
    ) %>%
    # 呼叫您原本的自定義函式
    mutate_raw2ft(dev_unit = params$unit) %>%
    # 繪圖邏輯 (保留您原始程式碼的設定) [1]
    mutate_raw2plot(
        analyte_name = params$analyte_name,
        dev_unit = params$unit
    )

# * Stage 2: VCA Analysis
stage2_analyze <- stage1_import %>%
    select(raw) %>%
    tidyr::unnest(raw) %>%
    tidyr::nest(
        raw = colnames(.)[colnames(.) != "sample"]
    ) %>%
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

# * Stage 3: Share Result
stage3_share <- stage2_analyze %>%
    select(
        sample,
        VCA_df
    ) %>%
    tidyr::unnest(VCA_df) %>%
    tidyr::nest(
        VCA_df = colnames(.)
    ) %>%
    mutate_VCA_df2ft()
