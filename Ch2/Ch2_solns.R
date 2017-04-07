#Q8
rm(list = ls(all=TRUE))
Auto <- Auto
names(Auto)
lm_auto <- lm(data = Auto, formula = mpg ~ horsepower)
summary(lm_auto)
attach(Auto)
par(mfrow=c(1, 1))
plot(horsepower, mpg)
detach(Auto)
abline(lm_auto, col='red')
horsepower_data = data.frame(horsepower = c(98))
predict(lm_auto, newdata = horsepower_data, interval = 'confidence')
predict(lm_auto, newdata = horsepower_data, interval = 'prediction')
par(mfrow=c(2, 2))
plot(lm_auto)
par(mfrow=c(1, 1))

#Q9
pairs(Auto)
cor_auto <- cor(Auto[, -which(names(Auto) == 'name')])
lm_auto_mult <- lm(data = Auto, formula = mpg ~ . - name)
summary(lm_auto_mult)

par(mfrow=c(2,2))
plot(lm_auto_mult)
par(mfrow=c(1,1))

lm_auto_mult_int <- lm(data = Auto, formula = mpg ~ . - name + displacement:horsepower)