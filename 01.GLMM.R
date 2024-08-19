
# Load R packages
library(lme4)
library(emmeans)
library(car)

# read data
data <- read.csv("/Users/zhongzhaopeng/CV0.5-GF0.5.csv")

# Define a generalized linear mixed model
model <- glm(response ~ treat + trails + (1 | body), data = data, family = binomial)

# Display the results
summary(model)

# Perform significance tests
anova_results <- Anova(model, type = "III")
print(anova_results)
