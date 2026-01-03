#* Flextable
# 依照可視寬度等比例換算欄位寬度:新版(2022-04-15)
colratio <- function(x,
                     ratio,
                     width = 7) {
    result <-
        ratio * width / sum(ratio)

    flextable::width(
        x = x,
        width = result
    )
}

# 依照可視寬度等比例換算欄位寬度:舊版
width_ratio <- function(x,
                        width_col_ratio,
                        width_visible = 7) {
    result <-
        width_col_ratio * width_visible / sum(width_col_ratio)

    flextable::width(
        x = x,
        width = result
    )
}

theme_ms <- function(x) {
    x <-
        # 以theme_box為基礎
        flextable::theme_box(x) %>%
        # header背景顏色
        flextable::bg(
            part = "header",
            bg = "#4F81BD"
        ) %>%
        # header外圍框線顏色&寬度
        flextable::border_outer(
            part = "header",
            border = fp_border(
                color = "#4F81BD",
                width = 1
            )
        ) %>%
        # header內側橫向框線顏色&寬度
        flextable::border_inner(
            part = "header",
            border = fp_border(
                color = "white",
                width = 1
            )
        ) %>%
        # body框線顏色&寬度
        flextable::border(
            part = "body",
            border = fp_border(
                color = "#4F81BD",
                width = 1
            )
        ) %>%
        # header文字顏色
        flextable::color(
            part = "header",
            color = "white"
        )

    return(x)
}

theme_cer <- function(x) {
    x <-
        # 以theme_box為基礎
        flextable::theme_box(x) %>%
        # header背景顏色
        flextable::bg(
            part = "header",
            bg = "#BDC0BA"
        ) %>%
        # header外圍框線顏色&寬度
        flextable::border_outer(
            part = "header",
            border = fp_border(
                color = "#000000",
                width = 1
            )
        ) %>%
        # header內側橫向框線顏色&寬度
        flextable::border_inner(
            part = "header",
            border = fp_border(
                color = "#000000",
                width = 1
            )
        ) %>%
        # body框線顏色&寬度
        flextable::border(
            part = "body",
            border = fp_border(
                color = "#000000",
                width = 1
            )
        ) %>%
        # header文字顏色
        flextable::color(
            part = "header",
            color = "#000000"
        )

    return(x)
}

# 多數情況
flextable::set_flextable_defaults(
    font.family = "Times New Roman",
    eastasia.family = "DFKai-SB",
    theme_fun = "theme_ms"
)

# # for CER
# flextable::set_flextable_defaults(
#   font.family = "Cambria",
#   eastasia.family = "DFKai-SB",
#   theme_fun = "theme_cer"
# )


#* Officer
# 指定一個會加入到tab這個seq_id的run動作
runs_tab <- officer::run_autonum(
    seq_id = "tab",
    pre_label = "",
    post_label = ""
)

# 指定一個會加入到fig這個seq_id的run動作
runs_fig <- officer::run_autonum(
    seq_id = "fig",
    pre_label = "",
    post_label = ""
)

# 指定一個會加入到ref這個seq_id的run動作
runs_ref <- officer::run_autonum(
    seq_id = "ref",
    pre_label = "",
    post_label = ""
)

# 定義常用文字格式
fp_red <- officer::fp_text_lite(color = "red")
fp_blue <- officer::fp_text_lite(color = "#005CAF")
fp_bold <- officer::fp_text_lite(bold = TRUE)
fp_underline <- officer::fp_text_lite(underlined = TRUE)
fp_high <- officer::fp_text_lite(shading.color = "yellow")