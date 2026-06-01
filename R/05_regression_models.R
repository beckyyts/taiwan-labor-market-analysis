# ============================================================
# 05_regression_models.R
# 迴歸模型比較：OLS / Ridge / Lasso / Elastic Net / PCR / PLS / RF / GBM / SVR
# ============================================================

library(dplyr)
library(ggplot2)
library(glmnet)
library(pls)
library(randomForest)
library(gbm)
library(e1071)

Sys.setlocale("LC_ALL", "C.UTF-8")

df <- read.csv("data/processed/taiwan_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ── 資料準備 ──────────────────────────────────────────────
predictors <- c("unemp_rate","lfpr","college_pct","aging_idx","log_density","industry_pct")
target     <- "log_wage"

df_model <- df %>%
  select(all_of(c(target, predictors))) %>%
  na.omit()

set.seed(42)
train_idx  <- sample(nrow(df_model), 0.8 * nrow(df_model))
train_data <- df_model[train_idx, ]
test_data  <- df_model[-train_idx, ]

X_train <- as.matrix(train_data[, predictors])
y_train <- train_data[[target]]
X_test  <- as.matrix(test_data[, predictors])
y_test  <- test_data[[target]]

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
r2   <- function(actual, predicted) 1 - sum((actual-predicted)^2)/sum((actual-mean(actual))^2)

results <- list()

# ════════════════════════════════════════════════════════════
# 1. OLS
# ════════════════════════════════════════════════════════════
ols      <- lm(log_wage ~ ., data = train_data)
pred_ols <- predict(ols, test_data)
results[["OLS"]] <- c(RMSE = rmse(y_test, pred_ols), R2 = r2(y_test, pred_ols))

cat("=== OLS ===\n")
print(summary(ols)$coefficients)

# ════════════════════════════════════════════════════════════
# 2. Ridge
# ════════════════════════════════════════════════════════════
cv_ridge   <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 5)
ridge_mod  <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
pred_ridge <- predict(ridge_mod, X_test)[,1]
results[["Ridge"]] <- c(RMSE = rmse(y_test, pred_ridge), R2 = r2(y_test, pred_ridge))

# ════════════════════════════════════════════════════════════
# 3. Lasso
# ════════════════════════════════════════════════════════════
cv_lasso   <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
lasso_mod  <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
pred_lasso <- predict(lasso_mod, X_test)[,1]
results[["Lasso"]] <- c(RMSE = rmse(y_test, pred_lasso), R2 = r2(y_test, pred_lasso))

cat("\n=== Lasso 保留變數 ===\n")
print(coef(lasso_mod))

# ════════════════════════════════════════════════════════════
# 4. Elastic Net
# ════════════════════════════════════════════════════════════
cv_enet   <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 5)
enet_mod  <- glmnet(X_train, y_train, alpha = 0.5, lambda = cv_enet$lambda.min)
pred_enet <- predict(enet_mod, X_test)[,1]
results[["ElasticNet"]] <- c(RMSE = rmse(y_test, pred_enet), R2 = r2(y_test, pred_enet))

png("output/figures/13_lasso_cv.png", width = 600, height = 450, res = 150)
plot(cv_lasso, main = "Lasso 交叉驗證：最佳 Lambda 選擇")
dev.off()

# ════════════════════════════════════════════════════════════
# 5. PCR
# ════════════════════════════════════════════════════════════
pcr_mod   <- pcr(log_wage ~ ., data = train_data, scale = TRUE, validation = "CV")
ncomp_pcr <- which.min(RMSEP(pcr_mod)$val[1,,]) - 1
pred_pcr  <- predict(pcr_mod, test_data, ncomp = ncomp_pcr)[,,1]
results[["PCR"]] <- c(RMSE = rmse(y_test, pred_pcr), R2 = r2(y_test, pred_pcr))
cat("\n=== PCR 最佳主成分數：", ncomp_pcr, "===\n")

# ════════════════════════════════════════════════════════════
# 6. PLS
# ════════════════════════════════════════════════════════════
pls_mod   <- plsr(log_wage ~ ., data = train_data, scale = TRUE, validation = "CV")
ncomp_pls <- which.min(RMSEP(pls_mod)$val[1,,]) - 1
pred_pls  <- predict(pls_mod, test_data, ncomp = ncomp_pls)[,,1]
results[["PLS"]] <- c(RMSE = rmse(y_test, pred_pls), R2 = r2(y_test, pred_pls))

# ════════════════════════════════════════════════════════════
# 7. Random Forest
# ════════════════════════════════════════════════════════════
set.seed(42)
rf_mod  <- randomForest(log_wage ~ ., data = train_data, ntree = 500, importance = TRUE)
pred_rf <- predict(rf_mod, test_data)
results[["RandomForest"]] <- c(RMSE = rmse(y_test, pred_rf), R2 = r2(y_test, pred_rf))

png("output/figures/14_rf_importance.png", width = 600, height = 450, res = 150)
varImpPlot(rf_mod, main = "Random Forest：變數重要性")
dev.off()

# ════════════════════════════════════════════════════════════
# 8. GBM
# ════════════════════════════════════════════════════════════
set.seed(42)
gbm_mod   <- gbm(log_wage ~ ., data = train_data,
                 distribution = "gaussian",
                 n.trees = 500, interaction.depth = 3,
                 shrinkage = 0.01, cv.folds = 5, verbose = FALSE)
best_iter <- gbm.perf(gbm_mod, method = "cv", plot.it = FALSE)
pred_gbm  <- predict(gbm_mod, test_data, n.trees = best_iter)
results[["GBM"]] <- c(RMSE = rmse(y_test, pred_gbm), R2 = r2(y_test, pred_gbm))

# ════════════════════════════════════════════════════════════
# 9. SVR
# ════════════════════════════════════════════════════════════
train_scaled <- as.data.frame(scale(train_data))
test_scaled  <- as.data.frame(scale(test_data,
                                    center = colMeans(train_data),
                                    scale  = apply(train_data, 2, sd)))
svr_mod   <- svm(log_wage ~ ., data = train_scaled, kernel = "radial", cost = 10, epsilon = 0.1)
pred_svr_s <- predict(svr_mod, test_scaled)
pred_svr   <- pred_svr_s * sd(train_data$log_wage) + mean(train_data$log_wage)
results[["SVR"]] <- c(RMSE = rmse(y_test, pred_svr), R2 = r2(y_test, pred_svr))

# ════════════════════════════════════════════════════════════
# 模型比較
# ════════════════════════════════════════════════════════════
comp_df <- as.data.frame(do.call(rbind, results))
comp_df$Model <- rownames(comp_df)
comp_df <- comp_df %>% arrange(RMSE)

cat("\n=== 模型比較 ===\n")
print(comp_df)

p_comp <- ggplot(comp_df, aes(x = reorder(Model, -R2), y = R2, fill = R2)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = round(R2, 3)), vjust = -0.4, size = 3) +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  labs(title = "各模型預測表現比較（Test R²）", x = "模型", y = "Test R²") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("output/figures/15_model_comparison.png", p_comp, width = 9, height = 5, dpi = 150)

write.csv(comp_df, "output/model_comparison.csv", row.names = FALSE)
cat("\n模型比較完成，結果存至 output/model_comparison.csv\n")
