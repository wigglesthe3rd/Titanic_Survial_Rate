library(dplyr)
library(caret)

train <- read.csv("train.csv")

## EDA
head(train)

library(ggplot2)

# check distribution of target variable
ggplot(data = train, aes(train$Age)) + 
  geom_histogram(aes( y = ..density..), fill = "blue") +
  geom_density()

# analyze summary statistics
library(psych)

psych::describe(train)

# check outliers
library(reshape2)
melted_train <- melt(train)
p <- ggplot(melted_train, aes(factor(variable),value))
p + geom_boxplot() + 
  facet_wrap(~variable, scale = "free")

# data already split into train & test
# build model using lm() function
lm_model <- lm(Age ~ . , data = train)
print(lm_model)

lm(formula = Age ~ ., data = train)

summary(lm_model)

