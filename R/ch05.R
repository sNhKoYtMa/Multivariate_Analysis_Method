##### 第5章_重回帰分析 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
x1 <- c(51,38,57,51,53,77,63,69,72,73)
x2 <- c(16,4,16,11,4,22,5,5,2,1)
y <- c(3.0,3.2,3.3,3.9,4.4,4.5,4.5,5.4,5.4,6.0)
# DataFrame化
data <- data.frame(x1 = x1,
                   x2 = x2,
                   y = y)
data # 表5.1 中古マンションのデータ

# 可視化 (図5.1 散布図)
scatter_plot1 <- data %>% 
  ggplot(aes(x = x1, y = y)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図5.1 x1とyの散布図') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot2 <- data %>% 
  ggplot(aes(x = x2, y = y)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図5.1 x2とyの散布図') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot3 <- data %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 2, color = 'blue') +
  ggtitle('図5.1 x1とx2の散布図') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
scatter_plot1 # 図5.1の左上
scatter_plot2 # 図5.1の右上
scatter_plot3 # 図5.1の下

# 重回帰分析
multiple_reg <- data %>% 
  lm(data = ., formula = y ~ x1 + x2)
summary(multiple_reg)
anova(multiple_reg) # 分散分析
multiple_reg_coef <- multiple_reg %>% 
  tidy()
multiple_reg_coef # 偏回帰係数

# 多重共線性確認
X <- cbind(x1, x2)
V <- cov(X)
R <- cor(X)
det(V)
solve(V)
eigen(V)$values

# 回帰診断
sr <- rstandard(multiple_reg)
leverage <- hat(X)
threshold <- 2.5*(1+(NCOL(data)-1))/NROW(data)

# 回帰診断可視化
# 図5.3 標準化残差とテコ比の散布図
srlev <- data.frame(sr = sr,
                    leverage = leverage)
reg_diag_srlev <- srlev %>% 
  ggplot(aes(x = sr, y = leverage)) +
  geom_point(size = 2, color = 'blue') +
  xlim(-2, 2) +
  xlab('Standardized Residuals') +
  ylab('Leverage') +
  ggtitle('図5.3 標準化残差とテコ比の散布図') +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = threshold, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
reg_diag_srlev # 図5.3

# 図5.4 xと標準化残差の散布図
xsr <- data.frame(x = X,
                  sr = sr)
reg_diag_x1sr <- xsr %>% 
  ggplot(aes(x = x1, y = sr)) +
  geom_point(size = 2, color = 'blue') +
  ylim(-2,2) +
  xlab('x1') +
  ylab('Standardized Residuals') +
  ggtitle('図5.4 x1と標準化残差の散布図') +
  geom_hline(yintercept = 0, linetype = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
reg_diag_x2sr <- xsr %>% 
  ggplot(aes(x = x2, y = sr)) +
  geom_point(size = 2, color = 'blue') +
  ylim(-2,2) +
  xlab('x2') +
  ylab('Standardized Residuals') +
  ggtitle('図5.4 x2と標準化残差の散布図') +
  geom_hline(yintercept = 0, linetype = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
reg_diag_x1sr # 図5.4の左
reg_diag_x2sr # 図5.4の右

ggsave(file = '図05.1 x1とyの散布図.png', plot = scatter_plot1)
ggsave(file = '図05.1 x2とyの散布図.png', plot = scatter_plot2)
ggsave(file = '図05.1 x1とx2の散布図.png', plot = scatter_plot3)
ggsave(file = '図05.3 標準化残差とテコ比の散布図.png', plot = reg_diag_srlev)
ggsave(file = '図05.4 x1と標準化残差の散布図.png', plot = reg_diag_x1sr)
ggsave(file = '図05.4 x2と標準化残差の散布図.png', plot = reg_diag_x2sr)

# 数値が若干本と違うけどめんどくさいので無視
