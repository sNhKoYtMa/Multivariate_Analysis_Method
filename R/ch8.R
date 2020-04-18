##### 第8章_数量化2類 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
y <- c('H','H','H','H','H','P','P','P','P','P')
# 健常者をH(Healthy)、患者をP(Patient)
x1 <- c('N','L','N','N','N','L','M','L','L','M')
x2 <- c('L','N','N','N','N','M','N','L','M','L')
# 無をN(None)、少をL(Little)、多をM(Much)
# DataFrame化
data <- data.frame(y = y,
                   x1 = x1,
                   x2 = x2)
data # 表8.1 健常者・患者の症状のデータ

# ダミー変数化
# 説明変数が1つの場合(p.120-125)は割愛
library('makedummies')
data_dummied <- makedummies(data, basal_level = F)
df_dummied <- data.frame(data_dummied)

Z <- lda(data = df_dummied, y ~ .)
Z
Z$means
Z$scaling
-apply(Z$means%*%Z$scaling,2,mean)

summary(Z)
predict(Z)$class # 予測クラス
predict(Z)$posterior # クラス毎の事後確率
predict(Z)$x # 判別得点

result_table <- table(data[,1], predict(Z)$class)
result_table # 表8.5 判別表
(result_table[1,1] + result_table[2,2]) / sum(result_table) # accuracy
result_table[1,2] / sum(result_table[1,])
result_table[2,1] / sum(result_table[2,])


# 変数に量的変数と質的変数が混在する場合
# データ作成
x3 <- c(50,69,93,76,88,43,56,38,21,25)
x4 <- c(15.5,18.4,26.4,22.9,18.6,16.9,21.6,12.2,16.0,10.5)
data_new <- cbind(data, x3, x4)
data_new # 表8.6 健常者・患者のデータ

# ダミー変数化
# 説明変数が1つの場合(p.120-125)は割愛
library('makedummies')
data_dummied_new <- makedummies(data_new, basal_level = F)
df_dummied_new <- data.frame(data_dummied_new)

Z <- lda(data = df_dummied_new, y ~ .)
Z
Z$means
Z$scaling
-apply(Z$means%*%Z$scaling,2,mean)

summary(Z)
predict(Z)$class # 予測クラス
predict(Z)$posterior # クラス毎の事後確率
predict(Z)$x # 判別得点

result_table <- table(data[,1], predict(Z)$class)
result_table # 表8.5 判別表
(result_table[1,1] + result_table[2,2]) / sum(result_table) # accuracy
result_table[1,2] / sum(result_table[1,])
result_table[2,1] / sum(result_table[2,])

