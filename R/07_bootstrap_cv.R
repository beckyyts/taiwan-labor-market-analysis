# ============================================================
# 07_bootstrap_cv.R
# Bootstrap 信賴區間 + K-fold / LOO 交叉驗證
# ============================================================

library(dplyr)
library(ggplot2)

Sys.setlocale("LC_ALL", "C.UTF-8")

df <- read.csv("data/processed/taiwan_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

predictors <- c("unemp_rate","lfpr","college_pct","aging_idx","log_density","industry_pct")
df_model   <- df[, c("log_wage", predictors)] %>% na.omit()

rmse_fn <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

# ════════════════════════════════════════════════════════════
# 一、K-fold 交叉驗證（K = 5, 10）
# ════════════════════════════════════════════════════════════
kfold_cv <- function(data, k) {
  n      <- nrow(data)
  folds  <- sample(rep(1:k, length.out = n))
  rmse_v <- numeric(k)
  for (i in 1:k) {
    train <- data[folds != i, ]
    val   <- data[folds == i, ]
    mod   <- lm(log_wage ~ ., data = train)
    rmse_v[i] <- rmse_fn(val$log_wage, predict(mod, val))
  }
  mean(rmse_v)
}

set.seed(42)
cv5  <- kfold_cv(df_model, k = 5)
cv10 <- kfold_cv(df_model, k = 10)

cat("=== K-fold 交叉驗證 ===\n")
cat("5-fold CV RMSE :", round(cv5,  4), "\n")
cat("10-fold CV RMSE:", round(cv10, 4), "\n\n")

# ════════════════════════════════════════════════════════════
# 二、Leave-One-Out 交叉驗證
# ════════════════════════════════════════════════════════════
n     <- nrow(df_model)
loo_e <- numeric(n)
for (i in 1:n) {
  mod      <- lm(log_wage ~ ., data = df_model[-i, ])
  loo_e[i] <- df_model$log_wage[i] - predict(mod, df_model[i, ])
}
loocv_rmse <- sqrt(mean(loo_e^2))
cat("LOO-CV RMSE    :", round(loocv_rmse, 4), "\n\n")

# ════════════════════════════════════════════════════════════
# 三、Bootstrap
# ════════════════════════════════════════════════════════════
B <- 1000

set.seed(42)
boot_coef <- matrix(NA, nrow = B, ncol = length(predictors) + 1)
colnames(boot_coef) <- c("Intercept", predictors)

for (b in 1:B) {
  idx           <- sample(n, n, replace = TRUE)
  mod           <- lm(log_wage ~ ., data = df_model[idx, ])
  boot_coef[b,] <- coef(mod)
}

boot_ci <- apply(boot_coef, 2, quantile, probs = c(0.025, 0.975))
cat("=== Bootstrap 95% CI（OLS 係數）===\n")
print(round(boot_ci, 4))

boot_df <- data.frame(college_pct = boot_coef[, "college_pct"])

p_boot <- ggplot(boot_df, aes(x = college_pct)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = boot_ci[, "college_pct"], color = "red", linetype = "dashed") +
  labs(title = "Bootstrap：college_pct 係數分布（B=1000）",
       subtitle = paste0("95% CI: [", round(boot_ci[1,"college_pct"],4),
                         ", ", round(boot_ci[2,"college_pct"],4), "]"),
       x = "係數估計", y = "次數") +
  theme_minimal(base_size = 12)
ggsave("output/figures/19_bootstrap_coef.png", p_boot, width = 7, height = 4, dpi = 150)

set.seed(42)
boot_rmse <- numeric(B)
for (b in 1:B) {
  idx       <- sample(n, n, replace = TRUE)
  test_b    <- df_model[-unique(idx), ]
  if (nrow(test_b) == 0) next
  mod       <- lm(log_wage ~ ., data = df_model[idx, ])
  boot_rmse[b] <- rmse_fn(test_b$log_wage, predict(mod, test_b))
}

boot_rmse <- boot_rmse[boot_rmse > 0]
rmse_ci   <- quantile(boot_rmse, c(0.025, 0.975))

cat("\n=== Bootstrap RMSE 分布 ===\n")
cat("平均 RMSE   :", round(mean(boot_rmse), 4), "\n")
cat("95% CI      :", round(rmse_ci[1], 4), "–", round(rmse_ci[2], 4), "\n")

p_rmse <- ggplot(data.frame(rmse = boot_rmse), aes(x = rmse)) +
  geom_histogram(bins = 40, fill = "#2ca25f", color = "white", alpha = 0.8) +
  geom_vline(xintercept = rmse_ci, color = "red", linetype = "dashed") +
  labs(title = "Bootstrap RMSE 分布（B=1000）",
       subtitle = paste0("95% CI: [", round(rmse_ci[1],4), ", ", round(rmse_ci[2],4), "]"),
       x = "RMSE", y = "次數") +
  theme_minimal(base_size = 12)
ggsave("output/figures/20_bootstrap_rmse.png", p_rmse, width = 7, height = 4, dpi = 150)

# ════════════════════════════════════════════════════════════
# 四、CV 方法比較
# ════════════════════════════════════════════════════════════
cv_summary <- data.frame(
  Method = c("5-fold CV", "10-fold CV", "LOO-CV", "Bootstrap"),
  RMSE   = round(c(cv5, cv10, loocv_rmse, mean(boot_rmse)), 4)
)
cat("\n=== 重抽樣方法 RMSE 比較 ===\n")
print(cv_summary)

p_cv <- ggplot(cv_summary, aes(x = Method, y = RMSE, fill = Method)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = RMSE), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "重抽樣方法 RMSE 比較", x = NULL, y = "RMSE") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggsave("output/figures/21_cv_comparison.png", p_cv, width = 6, height = 4, dpi = 150)

write.csv(cv_summary, "output/cv_summary.csv", row.names = FALSE)
cat("\nBootstrap + CV 分析完成\n")
