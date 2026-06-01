# ============================================================
# 01_data_collection.R
# 台灣各縣市勞動市場與社經條件資料建構
# 資料來源：主計總處、內政部（2019-2024）
# ============================================================

library(tidyverse)
select <- dplyr::select

# ── 讀入整合後的面板資料 ──────────────────────────────────
df_raw <- read.csv("data/raw/taiwan_panel_2019_2024.csv",
                   encoding = "UTF-8", stringsAsFactors = FALSE)

# ── 補入地區分類 ──────────────────────────────────────────
region_map <- tibble(
  county = c(
    "臺北市", "新北市", "桃園市", "基隆市", "新竹市", "新竹縣","宜蘭縣",
    "臺中市", "苗栗縣", "彰化縣", "南投縣","雲林縣",
    "臺南市", "高雄市", "嘉義縣", "嘉義市", "屏東縣",
    "花蓮縣", "臺東縣",
    "澎湖縣", "金門縣", "連江縣"
  ),
  region = c(
    rep("北部", 7),
    rep("中部", 5),
    rep("南部", 5),
    rep("東部", 2),
    rep("離島", 3)
  )
)

# ── 換算月薪（萬元/年 ÷ 12）──────────────────────────────
panel_data <- df_raw %>%
  left_join(region_map, by = "county") %>%
  mutate(wage = wage_median_10k / 12) %>%   # 月薪中位數（萬元）
  select(county, region, year, wage,
         unemp_rate, lfpr, college_pct,
         aging_idx, industry_pct, density, log_density) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# ── 輸出 ─────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(panel_data, "data/processed/taiwan_labor_panel.csv")

cat("資料建構完成\n")
cat("觀測數：", nrow(panel_data), "\n")
cat("縣市數：", n_distinct(panel_data$county), "\n")
cat("年份：", min(panel_data$year), "–", max(panel_data$year), "\n")
cat("欄位：", paste(names(panel_data), collapse = ", "), "\n")
