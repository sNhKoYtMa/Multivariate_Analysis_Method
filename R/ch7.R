##### 第7章_判別分析 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
y <- c('H','H','H','H','H','P','P','P','P','P')
# 健常者をH(Healthy)、患者をP(Patient)
x1 <- c(50,69,93,76,88,43,56,38,21,25)
x2 <- c(15.5,18.4,26.4,22.9,18.6,16.9,21.6,12.2,16.0,10.5)
# DataFrame化
data <- data.frame(y = y,
                   x1 = x1,
                   x2 = x2)
data # 表7.1 健常者・患者の検査値のデータ

Z <- lda(data = data, y ~ x1 + x2)
Z
Z$means
Z$scaling
-apply(Z$means%*%Z$scaling,2,mean)

summary(Z)
predict(Z)$class # 予測クラス
predict(Z)$posterior # クラス毎の事後確率
predict(Z)$x # 判別得点

result_table <- table(data[,1], predict(Z)$class)
result_table # 表7.5 判別表
(result_table[1,1] + result_table[2,2]) / sum(result_table) # accuracy
result_table[1,2] / sum(result_table[1,])
result_table[2,1] / sum(result_table[2,])


