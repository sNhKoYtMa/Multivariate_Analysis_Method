##### 第6章_数量化1類 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
x1 <- c('A','A','A','A','B','B','B','C','C','C')
# 本コード内では優良可をABCで示すこととする
x2 <- c('Y','Y','N','N','Y','N','N','Y','Y','N')
# 本コード内では所属無所属をYNで示すこととする
y <- c(96,88,77,89,80,71,77,78,70,62)
# DataFrame化
data <- data.frame(x1 = x1,
                   x2 = x2,
                   y = y)
data # 表6.1 成績のデータ

# ダミー変数化
# 説明変数が1つの場合(p.88-93)は割愛
library('makedummies')
data_dummied <- makedummies(data, basal_level = F)

# 数量化1類
multiple_reg <- lm(data = data_dummied, formula = y ~ x1 + x2)
summary(multiple_reg)
anova(multiple_reg) # 分散分析(表6.4)
multiple_reg_coef <- tidy(multiple_reg)
multiple_reg_coef # 偏回帰係数
# ダミー変数化後は重回帰分析と同様に計算できる

# 予測結果
y_hat <- c(predict(multiple_reg))
data65 <- cbind(data_dummied, y_hat)
data65 # 表6.5 表6.1のデータの書き換えおよび予測値


# 説明変数に量的変数と質的変数が混在する場合
# データ作成
x1 <- c('A','A','A','A','B','B','B','C','C','C')
# 本コード内では優良可をABCで示すこととする
x2 <- c('Y','Y','N','N','Y','N','N','Y','Y','N')
# 本コード内では所属無所属をYNで示すこととする
x3 <- c(15,85,78,15,57,29,64,22,57,50)
y <- c(96,88,77,89,80,71,77,78,70,62)
# DataFrame化
data_new <- data.frame(x1 = x1,
                       x2 = x2,
                       x3 = x3,
                       y = y)
data_new # 表6.6 量的変数と質的変数が混在した場合のデータの形式

# ダミー変数化
data_new_dummied <- makedummies(data_new, basal_level = F)

# 数量化1類
multiple_reg_new <- lm(data = data_new_dummied, formula = y ~ x1 + x2 + x3)
summary(multiple_reg_new)
anova(multiple_reg_new) # 分散分析
multiple_reg_coef_new <- tidy(multiple_reg_new)
multiple_reg_coef_new # 偏回帰係数
# ダミー変数化後は量的変数が混じっていても重回帰分析と同様に計算できる

# 予測結果
y_hat_new <- c(predict(multiple_reg_new))
result <- cbind(data_new, y_hat_new)
result

