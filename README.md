# ivd-preclinical-study-tools
This repo provides tools for IVD preclinical studies.

## Disclaimer
This application is provided for educational and research purposes only. It is intended to assist in statistical analysis based on CLSI guidelines (e.g., EP06) but has not been validated as a medical device or IVD software.

Users are solely responsible for verifying the accuracy of the results and ensuring compliance with their local regulatory requirements. The author assumes no liability for errors or omissions in the analysis or for any decisions made based on the output of this tool. Use valid CLSI official documents for final interpretation.

## 免責聲明
本應用程式僅供教育訓練與學術研究使用。本工具依照 CLSI 指引提供統計輔助，但未經醫療器材或體外診斷軟體之認證。

使用者有責任自行驗證結果的準確性，並確保符合當地法規要求。作者對於分析結果的誤差或基於本工具所做的任何決策不承擔任何法律責任。最終判讀請務必以 CLSI 官方標準文件為準。

## 使用準備

1. 請先前往[這裡](https://cloud.r-project.org/)下載並安裝R。
2. 如果要用rstudio desktop請前往[這裡](https://posit.co/download/rstudio-desktop/)下載並安裝。
3. 請先執行"install_deps.R"或者windows下請執行"install_deps_windows.R"，以安裝所需套件。也可以不安裝，執行時會自動安裝但會花時間。
  - 執行方法:
    - 如果是mac osx或是linux建議用`Rscript install_deps.R`來從終端執行。
    - 如果是windows (其他系統也可以)，則請用rstudio desktop，開啟"install_deps_windows.R"，然後全選並選擇"Run"，就可以執行。
4. 執行完成後，會出現"安裝程序完成。"或是"所有指定套件皆已安裝，無需執行任何動作。"

## 使用個別Shiny APP

### 以Rstudio Desktop使用
1. 開啟要執行的項目的app.R檔案
2. 點選選單中的"Session" -> "Set Working Directory" -> "To Source File Location"
3. 全選所有code，然後按下"Run APP"按鈕，就會出現App

### 以Rscript使用

1. 目錄切換到項目的app.R檔案所在目錄
2. `Rscript app.R`
