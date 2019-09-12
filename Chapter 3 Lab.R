library (MASS) 
library (ISLR)
fix(Boston)
names(Boston)

attach(Boston)
lm.fit <- lm(medv ~ lstat)
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
?(coef)
confint(lm.fit)

predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")

plot(lstat ,medv)
abline(lm.fit)

abline(lm.fit, lwd = 3, col = "red", pch = 20)

par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit = lm(medv~lstat + age , data = Boston); summary(lm.fit)

lm.fit <- lm(medv ~. data = Boston)
summary(lm.fit)

?summary.lm

summary(lm.fit)$r.sq
summary(lm.fit)$sigma
library(car)
vif(lm.fit)

lm.fit_2 <- update(lm.fit, ~.-age)

lm(medv~lstat * age, data = Boston)

lm.fit_3 <- lm(formula = medv ~ lstat + I(lstat^2))

anova(lm.fit_3)
?anova

lm.fit_4 <- lm(medv ~ poly (lstat, 5))
summary(lm.fit_4)

fix(Carseats)
names(Carseats)

lm.fit_5 <- lm(Sales ~. Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit_5)

lm.fit_6 <- lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)

contrasts(Carseats$ShelveLoc)
