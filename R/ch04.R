##### 第4章_単回帰分析 #####
library('stats')
library('MASS')
library('broom')
library('tidyverse')

# データ作成
x <- c(2.2,4.1,5.5,1.9,3.4,2.6,4.2,3.7,4.9,3.2)
y <- c(71,81,86,72,77,73,80,81,85,74)
# DataFrame化
data <- data.frame(x = x,
                   y = y)
data # 表4.1 成分Aの含有量xと収率yのデータ

# 単回帰
simple_reg <- data %>% 
  lm(data = ., formula = y ~ x)
summary(simple_reg)
anova(simple_reg)
simple_reg_coef <- simple_reg %>% 
  tidy()
simple_reg_coef # 偏回帰係数

# 単回帰可視化 (図4.1 散布図)
simple_reg_plot <- data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = 'blue') +
  stat_smooth(method = 'lm', se = F, colour = 'blue', size = 1) +
  # se = Tにすれば信頼区間表示
  xlim(0.5, 6) +
  xlab('成分Aの含有量') +
  ylab('収率') +
  ggtitle('図4.1 散布図') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
simple_reg_plot # 図4.1

# 回帰診断
sr <- rstandard(simple_reg) # 標準化残差
leverage <- hat(x) # テコ比
threshold <- 2.5*(1+(NCOL(data)-1))/NROW(data) # 閾値

# 回帰診断可視化
# 図4.4 標準化残差とテコ比の散布図
srlev <- data.frame(sr = sr,
                    leverage = leverage)
reg_diag_srlev <- srlev %>% 
  ggplot(aes(x = sr, y = leverage)) +
  geom_point(size = 2, color = 'blue') +
  xlim(-2, 2) +
  xlab('Standardized Residuals') +
  ylab('Leverage') +
  ggtitle('図4.4 標準化残差とテコ比の散布図') +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_hline(yintercept = threshold, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
reg_diag_srlev # 図4.4

# 図4.5 xと標準化残差の散布図
xsr <- data.frame(x = x,
                  sr = sr)
reg_diag_xsr <- xsr %>% 
  ggplot(aes(x = x, y = sr)) +
  geom_point(size = 2, color = 'blue') +
  xlim(0,NA) +
  ylim(-2,2) +
  xlab('x') +
  ylab('Standardized Residuals') +
  ggtitle('図4.5 xと標準化残差の散布図') +
  geom_hline(yintercept = 0, linetype = 1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1,'cm'))
reg_diag_xsr # 図4.5
