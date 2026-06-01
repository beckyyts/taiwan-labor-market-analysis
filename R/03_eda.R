# ============================================================
# 03_eda.R
# 探索性資料分析與視覺化
# ============================================================

library(dplyr)
library(ggplot2)

Sys.setlocale("LC_ALL", "C.UTF-8")

df <- read.csv("data/processed/taiwan_clean.csv",
               encoding = "UTF-8", stringsAsFactors = FALSE)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

theme_tw <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ── 1. 各縣市月薪中位數（2024）────────────────────────────
df_2024 <- df %>% filter(year == 2024) %>% arrange(desc(wage))

p1 <- ggplot(df_2024, aes(x = reorder(county, wage), y = wage, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "各縣市月薪中位數（2024年）",
       x = NULL, y = "月薪中位數（萬元）", fill = "地區") +
  theme_tw
ggsave("output/figures/01_wage_by_county.png", p1, width = 8, height = 6, dpi = 150)

# ── 2. 薪資趨勢（2019–2024，依地區）──────────────────────
p2 <- df %>%
  group_by(year, region) %>%
  summarise(mean_wage = mean(wage, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_wage, color = region, group = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "各地區月薪中位數趨勢（2019–2024）",
       x = "年份", y = "月薪中位數（萬元）", color = "地區") +
  theme_minimal(base_size = 12)
ggsave("output/figures/02_wage_trend.png", p2, width = 8, height = 5, dpi = 150)

# ── 3. 相關矩陣 ────────────────────────────────────────────
num_df <- df %>%
  select(wage, unemp_rate, lfpr, college_pct, aging_idx, log_density, industry_pct) %>%
  na.omit()

cor_mat  <- round(cor(num_df), 2)
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(Var1 = Var1, Var2 = Var2, corr = Freq)

p3 <- ggplot(cor_long, aes(Var1, Var2, fill = corr)) +
  geom_tile(color = "white") +
  geom_text(aes(label = corr), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d73027",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "變數相關矩陣", x = NULL, y = NULL, fill = "相關係數") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/03_correlation_matrix.png", p3, width = 7, height = 6, dpi = 150)

# ── 4. 薪資 vs 大學比例 ────────────────────────────────────
p4 <- ggplot(df, aes(x = college_pct, y = wage, color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "大學比例 vs 月薪中位數",
       x = "大專以上就業者比例（%）", y = "月薪中位數（萬元）", color = "地區") +
  theme_minimal(base_size = 12)
ggsave("output/figures/04_college_vs_wage.png", p4, width = 7, height = 5, dpi = 150)

# ── 5. 失業率分布（boxplot，依地區）───────────────────────
p5 <- ggplot(df, aes(x = region, y = unemp_rate, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "各地區失業率分布", x = "地區", y = "失業率（%）") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggsave("output/figures/05_unemp_boxplot.png", p5, width = 6, height = 5, dpi = 150)

# ── 6. 薪資 QQ 圖 ─────────────────────────────────────────
png("output/figures/06_wage_qqplot.png", width = 600, height = 500, res = 150)
qqnorm(df$wage, main = "月薪 QQ 圖", pch = 16, col = "steelblue")
qqline(df$wage, col = "red", lwd = 2)
dev.off()

# ── 7. 描述統計輸出 ────────────────────────────────────────
desc_stats <- df %>%
  select(wage, unemp_rate, lfpr, college_pct, aging_idx, industry_pct) %>%
  summarise(across(everything(), list(
    mean = ~round(mean(., na.rm = TRUE), 2),
    sd   = ~round(sd(., na.rm = TRUE), 2),
    min  = ~round(min(., na.rm = TRUE), 2),
    max  = ~round(max(., na.rm = TRUE), 2)
  )))

cat("=== 描述統計 ===\n")
print(t(desc_stats))
cat("\nEDA 圖表已存至 output/figures/\n")
