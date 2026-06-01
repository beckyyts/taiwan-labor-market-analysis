# ============================================================
# 06_classification.R
# 分類模型：Logistic / LDA / QDA / KNN
# ============================================================

library(dplyr)
library(ggplot2)
library(MASS)
library(class)
select <- dplyr::select

Sys.setlocale("LC_ALL", "C.UTF-8")

df <- read.csv("data/processed/taiwan_clean.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ── 資料準備 ──────────────────────────────────────────────
predictors <- c("unemp_rate","lfpr","college_pct","aging_idx","log_density","industry_pct")

df_cls <- df %>%
  select(all_of(c("high_wage", predictors))) %>%
  na.omit() %>%
  mutate(high_wage = factor(high_wage, labels = c("低薪","高薪")))

set.seed(42)
train_idx  <- sample(nrow(df_cls), 0.8 * nrow(df_cls))
train_data <- df_cls[train_idx, ]
test_data  <- df_cls[-train_idx, ]

accuracy    <- function(pred, actual) mean(pred == actual)
conf_pretty <- function(pred, actual, model_name) {
  ct <- table(Predicted = pred, Actual = actual)
  cat("\n===", model_name, "混淆矩陣 ===\n")
  print(ct)
  cat("Accuracy:", round(accuracy(pred, actual), 3), "\n")
}

results_cls <- list()

# ════════════════════════════════════════════════════════════
# 1. Logistic Regression
# ════════════════════════════════════════════════════════════
logit_mod  <- glm(high_wage ~ ., data = train_data, family = binomial)
prob_logit <- predict(logit_mod, test_data, type = "response")
pred_logit <- factor(ifelse(prob_logit > 0.5, "高薪", "低薪"), levels = c("低薪","高薪"))
conf_pretty(pred_logit, test_data$high_wage, "Logistic Regression")
results_cls[["Logistic"]] <- accuracy(pred_logit, test_data$high_wage)

cat("\n=== Logistic 係數 ===\n")
print(summary(logit_mod)$coefficients)

# ════════════════════════════════════════════════════════════
# 2. LDA
# ════════════════════════════════════════════════════════════
lda_mod  <- lda(high_wage ~ ., data = train_data)
pred_lda <- predict(lda_mod, test_data)$class
conf_pretty(pred_lda, test_data$high_wage, "LDA")
results_cls[["LDA"]] <- accuracy(pred_lda, test_data$high_wage)

lda_scores <- predict(lda_mod, df_cls)
df_lda <- data.frame(LD1 = lda_scores$x[,1], high_wage = df_cls$high_wage)

p_lda <- ggplot(df_lda, aes(x = LD1, fill = high_wage)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("低薪" = "#e41a1c", "高薪" = "#377eb8")) +
  labs(title = "LDA 判別分數分布", x = "LD1", fill = "薪資水準") +
  theme_minimal(base_size = 12)
ggsave("output/figures/16_lda_scores.png", p_lda, width = 6, height = 4, dpi = 150)

# ════════════════════════════════════════════════════════════
# 3. QDA
# ════════════════════════════════════════════════════════════
qda_mod  <- qda(high_wage ~ ., data = train_data)
pred_qda <- predict(qda_mod, test_data)$class
conf_pretty(pred_qda, test_data$high_wage, "QDA")
results_cls[["QDA"]] <- accuracy(pred_qda, test_data$high_wage)

# ════════════════════════════════════════════════════════════
# 4. KNN
# ════════════════════════════════════════════════════════════
X_train_knn <- scale(train_data[, predictors])
X_test_knn  <- scale(test_data[, predictors],
                     center = attr(X_train_knn, "scaled:center"),
                     scale  = attr(X_train_knn, "scaled:scale"))

set.seed(42)
k_acc  <- sapply(1:15, function(k) {
  pred <- knn(X_train_knn, X_test_knn, train_data$high_wage, k = k)
  accuracy(pred, test_data$high_wage)
})
best_k <- which.max(k_acc)
cat("\n=== KNN 最佳 k:", best_k, "(Accuracy:", round(k_acc[best_k], 3), ") ===\n")

pred_knn <- knn(X_train_knn, X_test_knn, train_data$high_wage, k = best_k)
conf_pretty(pred_knn, test_data$high_wage, paste0("KNN (k=", best_k, ")"))
results_cls[["KNN"]] <- accuracy(pred_knn, test_data$high_wage)

png("output/figures/17_knn_k_selection.png", width = 600, height = 400, res = 150)
plot(1:15, k_acc, type = "b", pch = 16, col = "steelblue",
     xlab = "k", ylab = "Accuracy", main = "KNN：k 選擇")
abline(v = best_k, lty = 2, col = "red")
dev.off()

# ════════════════════════════════════════════════════════════
# 分類模型比較
# ════════════════════════════════════════════════════════════
cls_comp <- data.frame(
  Model    = names(results_cls),
  Accuracy = unlist(results_cls)
) %>% arrange(desc(Accuracy))

cat("\n=== 分類模型比較 ===\n")
print(cls_comp)

p_cls <- ggplot(cls_comp, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Accuracy)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = round(Accuracy, 3)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  scale_y_continuous(limits = c(0, 1.1)) +
  labs(title = "分類模型準確率比較", x = "模型", y = "Accuracy") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggsave("output/figures/18_classification_comparison.png", p_cls, width = 7, height = 4, dpi = 150)

cat("\n分類分析完成\n")
