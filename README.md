# 台灣各縣市勞動市場與社經條件分析

分析台灣 22 縣市 2019–2024 年的勞動市場結構與薪資脆弱度，涵蓋探索性分析、降維、群聚、迴歸與分類模型。

---

## 資料來源

| 資料 | 來源 | 年份 |
|------|------|------|
| 月薪中位數（本國籍全時受僱員工） | 主計總處《工業及服務業受僱員工全年總薪資統計》表7 | 2019–2024 |
| 老化指數、人口密度、勞動力參與率、失業率、就業結構、教育程度 | 主計總處《縣市重要統計指標查詢系統》 | 2019–2024 |

> 金門縣、連江縣無薪資資料，相關分析中該兩縣將被排除。

---

## 專案結構

```
project/
├── data/
│   ├── raw/
│   │   └── taiwan_panel_2019_2024.csv   ← 放這裡
│   └── processed/                        ← 由腳本自動產生
├── output/
│   ├── figures/                          ← 圖表輸出
│   ├── model_comparison.csv
│   └── cv_summary.csv
├── 01_data_collection.R
├── 02_data_cleaning.R
├── 03_eda.R
├── 04_pca_cluster.R
├── 05_regression_models.R
├── 06_classification.R
├── 07_bootstrap_cv.R
└── README.md
```

---

## 變數說明

| 變數 | 說明 | 單位 |
|------|------|------|
| `wage` | 月薪中位數（年薪 ÷ 12） | 萬元 |
| `unemp_rate` | 失業率 | % |
| `lfpr` | 勞動力參與率 | % |
| `college_pct` | 就業者大專以上比例 | % |
| `aging_idx` | 老化指數（65歲以上 / 15歲以下 × 100） | — |
| `industry_pct` | 就業者工業結構比例（含製造、營造、水電等） | % |
| `density` | 人口密度 | 人/km² |
| `log_density` | 人口密度（對數） | — |
| `region` | 地區（北部／中部／南部／東部／離島） | — |

---

## 執行方式

依序執行以下腳本：

```r
source("01_data_collection.R")   # 讀入資料、補地區欄、換算月薪
source("02_data_cleaning.R")     # 建衍生變數、標準化、輸出截面資料
source("03_eda.R")               # 描述統計、趨勢圖、相關矩陣
source("04_pca_cluster.R")       # PCA + K-means + 階層式群聚
source("05_regression_models.R") # OLS / Ridge / Lasso / EN / PCR / PLS / RF / GBM / SVR
source("06_classification.R")    # Logistic / LDA / QDA / KNN
source("07_bootstrap_cv.R")      # K-fold CV / LOO-CV / Bootstrap
```

---

## 套件需求

```r
install.packages(c(
  "tidyverse",
  "glmnet",
  "pls",
  "randomForest",
  "gbm",
  "e1071",
  "MASS",
  "class"
))
```

---

## 分析架構

1. **EDA**：各縣市薪資排名、地區趨勢、變數相關矩陣
2. **PCA**：2024 年截面降維，檢視縣市分布結構
3. **群聚分析**：K-means（k=4）+ 階層式群聚（Ward's method）
4. **迴歸**：以 `log(wage)` 為目標，比較 9 種模型的 RMSE 與 R²
5. **分類**：以「高薪／低薪」為標籤，比較 4 種模型的準確率
6. **重抽樣**：5-fold / 10-fold / LOO-CV + Bootstrap（B=1000）估計係數信賴區間
