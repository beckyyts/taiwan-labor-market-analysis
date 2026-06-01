# ============================================================
# 04_pca_cluster.R
# 主成分分析（PCA）+ 群聚分析（K-means / 階層式）
# ============================================================

library(dplyr)
library(ggplot2)

Sys.setlocale("LC_ALL", "C.UTF-8")

df      <- read.csv("data/processed/taiwan_clean.csv",  encoding = "UTF-8", stringsAsFactors = FALSE)
df_2024 <- read.csv("data/processed/taiwan_2024.csv",   encoding = "UTF-8", stringsAsFactors = FALSE)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

num_vars <- c("wage","unemp_rate","lfpr","college_pct","aging_idx","log_density","industry_pct")

# ════════════════════════════════════════════════════════════
# 一、主成分分析（PCA）
# ════════════════════════════════════════════════════════════
X_2024 <- df_2024[, num_vars]
pca    <- prcomp(X_2024, center = TRUE, scale. = TRUE)

var_exp <- summary(pca)$importance[2, ]
cum_var <- summary(pca)$importance[3, ]

cat("=== PCA 解釋變異量 ===\n")
print(round(var_exp * 100, 1))
cat("前兩個主成分累積解釋：", round(cum_var[2] * 100, 1), "%\n\n")

# Scree plot
png("output/figures/07_pca_scree.png", width = 600, height = 450, res = 150)
plot(var_exp * 100, type = "b", pch = 16, col = "steelblue",
     xlab = "主成分", ylab = "解釋變異量（%）", main = "PCA Scree Plot")
abline(h = 10, lty = 2, col = "red")
dev.off()

# PC1 vs PC2 散佈圖
scores         <- as.data.frame(pca$x[, 1:2])
scores$county  <- df_2024$county
scores$region  <- df_2024$region

p_pca <- ggplot(scores, aes(x = PC1, y = PC2, label = county, color = region)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.15, size = 2.5, check_overlap = TRUE) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "PCA：各縣市在前兩主成分上的分布（2024）",
       x = paste0("PC1（", round(var_exp[1]*100,1), "%）"),
       y = paste0("PC2（", round(var_exp[2]*100,1), "%）"),
       color = "地區") +
  theme_minimal(base_size = 11)
ggsave("output/figures/08_pca_biplot.png", p_pca, width = 8, height = 6, dpi = 150)

# Loading 圖
loadings          <- as.data.frame(pca$rotation[, 1:2])
loadings$variable <- rownames(loadings)

p_loading <- ggplot(loadings, aes(x = PC1, y = PC2, label = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
  geom_text(nudge_x = 0.03, size = 3) +
  labs(title = "PCA Loading 圖", x = "PC1", y = "PC2") +
  theme_minimal(base_size = 11)
ggsave("output/figures/09_pca_loading.png", p_loading, width = 6, height = 5, dpi = 150)

cat("PC1 最高負荷變數：\n")
print(sort(abs(pca$rotation[,1]), decreasing = TRUE))

# ════════════════════════════════════════════════════════════
# 二、K-means 群聚分析
# ════════════════════════════════════════════════════════════
X_scaled <- scale(X_2024)

set.seed(42)
wss <- sapply(1:8, function(k) kmeans(X_scaled, k, nstart = 25)$tot.withinss)

png("output/figures/10_kmeans_elbow.png", width = 600, height = 450, res = 150)
plot(1:8, wss, type = "b", pch = 16, col = "steelblue",
     xlab = "群數 k", ylab = "Within-cluster SS", main = "K-means Elbow Method")
dev.off()

set.seed(42)
km <- kmeans(X_scaled, centers = 4, nstart = 25)
df_2024$cluster_km <- factor(km$cluster)

cat("\n=== K-means 分群結果 ===\n")
print(table(df_2024$cluster_km))
cat("\n各群月薪中位數平均（萬元）：\n")
print(tapply(df_2024$wage, df_2024$cluster_km, mean))

scores$cluster_km <- df_2024$cluster_km

p_km <- ggplot(scores, aes(x = PC1, y = PC2, color = cluster_km, label = county)) +
  geom_point(size = 3.5) +
  geom_text(nudge_y = 0.15, size = 2.5, check_overlap = TRUE) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "K-means 群聚結果（PCA空間，k=4）", color = "群別") +
  theme_minimal(base_size = 11)
ggsave("output/figures/11_kmeans_pca.png", p_km, width = 8, height = 6, dpi = 150)

# ════════════════════════════════════════════════════════════
# 三、階層式群聚分析
# ════════════════════════════════════════════════════════════
dist_mat <- dist(X_scaled, method = "euclidean")
hc       <- hclust(dist_mat, method = "ward.D2")

png("output/figures/12_hclust_dendrogram.png", width = 900, height = 500, res = 150)
plot(hc, labels = df_2024$county,
     main = "階層式群聚樹狀圖（Ward's Method）",
     xlab = "", ylab = "距離", cex = 0.7)
rect.hclust(hc, k = 4, border = c("red","blue","green","orange"))
dev.off()

df_2024$cluster_hc <- factor(cutree(hc, k = 4))

cat("\n=== 階層式群聚結果（k=4）===\n")
print(table(df_2024$cluster_hc))

cat("\nPCA + 群聚分析完成，圖表已存至 output/figures/\n")
