# 1. 強制設定鏡像站為 Global CDN (避免跳出詢問視窗)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 2. 自動檢查並安裝 pacman 套件
if (!require("pacman")) install.packages("pacman")

# 3. 載入所需套件 (pacman 會自動判斷安裝)

#! 這裡列出的是通用需要的package
pkg_lst <- c(
    "knitr",
    "conflicted",
    "readODS",
    "readxl",
    "dplyr",
    "flextable",
    "officedown",
    "officer",
    "docxtractr",
    "ggplot2",
    "tidyr",
    "tibble",
    "purrr"
)

pacman::p_load(char = pkg_lst)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("chisq.test", "stats")
conflicted::conflict_prefer("fisher.test", "stats")
conflicted::conflict_prefer("wday", "lubridate")
conflicted::conflict_prefer("hour", "lubridate")
conflicted::conflict_prefer("read_docx", "docxtractr")
