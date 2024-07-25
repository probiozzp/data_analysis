
# Load R packages
library(lme4)
library(emmeans)
library(car)

# read data
data <- read.csv("/Users/zhongzhaopeng/CV0.5-GF0.5.csv")

# Define a generalized linear mixed model
model <- glmer(response ~ treat + trails + (1 | body), data = data, family = binomial)

# Display the results
summary(model)

# Perform significance tests
anova_results <- Anova(model, type = "III")
print(anova_results)




#####如果采用多重比较则使用下面的脚本

# 检查trails的唯一值
unique(data$trails)

# 将trails转换为因子变量
data$trails <- factor(data$trails)

# 拟合GLMM模型
model <- glmer(response ~ treat * trails + (1|body), data = data, family = binomial)

# 显著性检验
summary(model)

# 估计边际均值，只有在treat中有很多不同处理的时候需要用到emmeans，如果只有两种的话可以直接查看模型中的treat的固定效应估计值来实现
emmeans_results <- emmeans(model, ~ treat * trails)

# 进行Tukey HSD事后检验
pairwise_comparisons <- pairs(emmeans_results, adjust = "tukey")

# 显示比较结果
print(pairwise_comparisons)



