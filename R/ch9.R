##### 第9章_主成分分析 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
x1 <- c(86,71,42,62,96,39,50,78,51,89)
x2 <- c(79,75,43,58,97,33,53,66,44,92)
x3 <- c(67,78,39,98,61,45,64,52,76,93)
x4 <- c(68,84,44,95,63,50,72,47,72,91)
# DataFrame化
data <- data.frame(x1 = x1,
                   x2 = x2,
                   x3 = x3,
                   x4 = x4)
data # 表9.1 試験の成績のデータ

scatter_plot1 <- data %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x1とx2)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot2 <- data %>% 
  ggplot(aes(x = x1, y = x3)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x1とx3)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot3 <- data %>% 
  ggplot(aes(x = x1, y = x4)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x1とx4)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot4 <- data %>% 
  ggplot(aes(x = x2, y = x3)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x2とx3)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot5 <- data %>% 
  ggplot(aes(x = x2, y = x4)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x2とx4)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot6 <- data %>% 
  ggplot(aes(x = x3, y = x4)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図9.1 散布図(x3とx4)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot1 # 図9.1左上
scatter_plot2 # 図9.1右上
scatter_plot3 # 図9.1左中
scatter_plot4 # 図9.1右中
scatter_plot5 # 図9.1左下
scatter_plot6 # 図9.1左下


# 主成分分析
pca <- princomp(data, cor = T)
# cor = T の時、出発行列が相関係数行列

cor(data) # 相関係数行列
pca_sdev <- pca$sdev
pca_sdev**2 # 固有値
pca_loadings <- pca$loadings[,1:4]
pca_loadings # 固有ベクトル
summary(pca) # 寄与率や累積寄与率
screeplot(pca) # 寄与率可視化

factor_loadings <- t(sapply(1:NCOL(data),function(i) pca_sdev[i] * pca_loadings[,i]))
factor_loadings # 表9.2 因子負荷量

pca_scores <- pca$scores # 主成分得点
data_std <- scale(data)[,1:4] # 標準化
cbind(data_std, pca_scores[,1:2]) # 表9.3 標準化した値と主成分得点

row.names(factor_loadings) <- paste('z', c(1:4), sep = '')
factor_loadings <- t(factor_loadings)
factor_loadings <- data.frame(factor_loadings)
scatter_factor_loadings <- factor_loadings %>% 
  ggplot(aes(x = z1, y = z2)) +
  geom_point(size = 2, color = 'blue') +
  geom_text(aes(label = row.names(factor_loadings)),
            hjust = 0, vjust = 1, color = 'blue') +
  ggtitle('図9.2 因子負荷量と散布図') +
  xlim(-1, 1) +
  ylim(-1, 1) +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_factor_loadings # 図9.2 因子負荷量の散布図

pca_scores <- data.frame(pca_scores)
row.names(pca_scores) <- paste('No.', c(1:10), sep = '')
colnames(pca_scores) <- paste('z', c(1:4), sep = '')
scatter_pca_scores <- pca_scores %>% 
  ggplot(aes(x = z1, y = z2)) +
  geom_point(size = 2, color = 'blue') +
  geom_text(aes(label = row.names(pca_scores)),
            hjust = 0, vjust = 1, color = 'blue') +
  ggtitle('図9.3 主成分得点の散布図') +
  xlim(-3, 3) +
  ylim(-3, 3) +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_pca_scores # 図9.3 主成分得点の散布図

