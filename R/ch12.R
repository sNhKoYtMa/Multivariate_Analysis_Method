##### 第12章_クラスタ―分析 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')
library('ggdendro')

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
data # 表12.1 試験の成績のデータ(表9.1の再掲)

distdata <- dist(data)^2 / 2
hc <- hclust(distdata, 'ward.D') # ウォード法
dhc <- as.dendrogram(hc)
ddata <- dendro_data(dhc, type = 'rectangle')
dendrogram_plot <- ggplot(segment(ddata)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2, 0))
dendrogram_plot 

# ggdendro使わないオーソドックスなパターン
k <- 3 #クラスター数
cutdata <- cutree(hc, k = k)
hcc <- length(hc$height)
cutline <- (hc$height[hcc - (k - 2)] + hc$height[hcc - (k - 1)]) / 2
plot(hc, hang = -1, xlab = '', ylab = 'dist')
abline(h = cutline, lty = 4) # 図12.7 表12.1のデータのデンドログラム


