library(ggplot2) 
library(tidyverse)
library(class)

library(leaps)
library(glmnet)

library(ISLR)
attach(Credit)

################################################################################

data("Credit")
summary(Credit)


table(Credit$Student)["Yes"]
table(Credit$Student)["No"]

table(Credit$Married)["Yes"]
table(Credit$Married)["No"]

table(Credit$Gender)[" Male"]
table(Credit$Gender)["Female"]

table(Credit$Ethnicity)["Asian"]
table(Credit$Ethnicity)["African American"]
table(Credit$Ethnicity)["Caucasian"]


################################################################################


bestsub  <- regsubsets(Balance ~ ID + Income + Limit + Rating + Cards + Age + Education, data = Credit, nvmax = 11)
forward  <- regsubsets(Balance ~ ID + Income + Limit + Rating + Cards + Age + Education, data = Credit, method = "forward", nvmax = 11)
backward <- regsubsets(Balance ~ ID + Income + Limit + Rating + Cards + Age + Education, data = Credit, method = "backward", nvmax = 11)

bestsub.sum <- summary(bestsub)
bestsub.sum$adjr2
bestsub.sum$cp
bestsub.sum$bic
bestsub.sum

forward.sum <- summary(forward)
forward.sum$adjr2
forward.sum$cp
forward.sum$bic
forward.sum

backward.sum <- summary(backward)
backward.sum$adjr2
backward.sum$cp
backward.sum$bic
backward.sum

c(which.max(bestsub.sum$adjr2),
  which.max(forward.sum$adjr2),
  which.max(backward.sum$adjr2))

c(which.min(bestsub.sum$cp),
  which.min(forward.sum$cp),
  which.min(backward.sum$cp))

c(which.min(bestsub.sum$bic),
  which.min(forward.sum$bic),
  which.min(backward.sum$bic))


################################################################################


lm.fit <- lm(Balance ~ Income + Rating)

lm.fit

confint(lm.fit)

summary(lm.fit)

plot_1 <- ggplot( data = lm.fit, mapping = aes( x = Income + Rating, y = Balance ) ) +
  labs( x = "Income + Credit Rating", y = "Average Credit Card Balance In Dollarrs", title = "The Effect Of Income & Credit Rating On Account Balance" ) +
  geom_jitter( aes ( x = Income + Rating, y = Balance ), color = "Darkred", width = 0.5, height = 0.6, alpha = 0.5 ) +
  geom_smooth( mapping = aes ( x = Income + Rating, y = Balance ), method = lm, level = 0.95, color = "black" )

plot_1 +                                                   
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


################################################################################


lm.fit <- lm(Balance ~ Income + Limit + Rating + Cards + Age)

lm.fit

confint(lm.fit)

summary(lm.fit)

plot_1 <- ggplot( data = lm.fit, mapping = aes( x = Income + Limit + Rating + Cards + Age, y = Balance ) ) +
  labs( x = "Income + Limit + Rating + Cards + Age", y = "Average Credit Card Balance In Dollarrs", title = "The Effect Of Income, Credit Limit, Credit Rating, Number Of Cards & Age On Account Balance" ) +
  geom_jitter( aes ( x = Income + Limit + Rating + Cards + Age, y = Balance ), color = "blue", width = 0.5, height = 0.6, alpha = 0.5 ) +
  geom_smooth( mapping = aes ( x = Income + Limit + Rating + Cards + Age, y = Balance ), method = lm, level = 0.95, color = "black" )

plot_1 +                                                   
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


################################################################################


set.seed(1)

Credit[2:7]

train <- seq(1,250)
train.X <- cbind(Credit[4:6])[train, ]

test <- seq(251,400)
test.X <- cbind(Credit[4:6])[test, ]

train.Y <- Married[train]

knn.pred <- knn(train.X, test.X, train.Y, k = 10)

summary(knn.pred)

test.Y <- Married[test]

table(knn.pred, test.Y)

Accuracy <- mean(knn.pred == test.Y)
Accuracy

Error <- 1 - Accuracy
Error


