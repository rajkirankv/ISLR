rm(list = ls(all=TRUE))
library(MASS)
library(ISLR)
# fix(Boston)
Boston = Boston
names(Boston)

#SLR

lm_Boston = lm(formula = medv ~ lstat, data = Boston)
confint(lm_Boston)
lstat_data <- data.frame(lstat = c(5, 10, 15))
predict(lm_Boston, newdata = lstat_data, interval = "confidence")
predict(lm_Boston, newdata = lstat_data, interval = "prediction")
plot(lstat, medv)
abline(lm_Boston, col = 'red')
par(mfrow=c(2, 2))
plot(lm_Boston)
par(mfrow=c(1, 1))
plot(predict(lm_Boston), residuals(lm_Boston))
plot(predict(lm_Boston), rstudent(lm_Boston))
plot(hatvalues(lm_Boston))
which.max(hatvalues(lm_Boston))

#MLR

lm_Boston_2 = lm(formula = medv ~ lstat + age, data = Boston)
summary(lm_Boston_2)
lm_Boston_3 = lm(formula = medv ~ ., data = Boston)
summary(lm_Boston_3)
library(car)
vif(lm_Boston_3)
lm_Boston_4 <- lm(formula = medv ~ . -tax - rad, data = Boston)
summary(lm_Boston_4)

#MLR non linear

lm_Boston_5 <- lm(formula = medv ~ lstat + I(lstat^2), data = Boston)
summary(lm_Boston_5)

lm_Boston_4 <- lm(formula = medv ~ lstat, data = Boston)
summary(lm_Boston_4)

anova(lm_Boston_4, lm_Boston_5)
par(mfrow = c(2, 2))
plot(lm_Boston_5)

lm_Boston_6 <- lm(formula = medv ~ poly(lstat, 5), data = Boston)
summary(lm_Boston_6)

#----Carseats------
rm(list = ls(all=TRUE))
Carseats = Carseats

names(Carseats)
lm_carseats <- lm(data = Carseats, formula = Sales ~ . + Income:Advertising + Price:Age)
summary(lm_carseats)
contrasts(Carseats$ShelveLoc)