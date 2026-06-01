# ============================================================
# 02_data_cleaning.R
# 資料清理、標準化、建衍生變數
# ============================================================

library(dplyr)
select <- dplyr::select

Sys.setlocale("LC_ALL", "C.UTF-8")

df <- read.csv("data/processed/taiwan_labor_panel.csv",
               encoding = "UTF-8", stringsAsFactors = FALSE)

# ── 1. 建衍生變數 ─────────────────────────────────────────
df <- df %>%
  mutate(
    log_wage   = log(wage),
    wage_level = ifelse(wage >= median(wage, na.rm = TRUE), "高薪", "低薪"),
    high_wage  = as.integer(wage >= median(wage, na.rm = TRUE)),
    region_f   = factor(region, levels = c("北部","中部","南部","東部","離島")),
    year_f     = factor(year)
  )

# ── 2. 標準化數值欄位（用於 PCA / 群聚 / SVR）───────────────
num_vars <- c("wage","unemp_rate","lfpr","college_pct",
              "aging_idx","log_density","industry_pct")

df_scaled <- df %>%
  mutate(across(all_of(num_vars), scale, .names = "{.col}_z"))

# ── 3. 截面資料（2024年，用於群聚分析）────────────────────
df_2024 <- df %>% filter(year == 2024)

# ── 4. 輸出 ──────────────────────────────────────────────
write.csv(df,        "data/processed/taiwan_clean.csv",   row.names = FALSE)
write.csv(df_scaled, "data/processed/taiwan_scaled.csv",  row.names = FALSE)
write.csv(df_2024,   "data/processed/taiwan_2024.csv",    row.names = FALSE)

cat("清理完成\n")
cat("欄位：", paste(names(df), collapse = ", "), "\n")
cat("高薪縣市比例：", mean(df$high_wage, na.rm = TRUE), "\n")
