library(MASS)
library(ISLR)

names(Boston)
plot(medv~lstat,Boston)
fit1 = lm(medv~lstat,data = Boston)
fit1

summary(fit1)

abline(fit1, col = "red")
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat = c(5,10,15))), interval = "confidence "

fit2 = lm(medv~lstat + age, ata = Boston)
fit2

summary(fit2)

fit3 = lm(medv ~., Boston)
fit3

par(mfrow = c(2,2))
plot(fit3)

fit4 = update(fit3, ~ -age - indus)

fit5 = lm(medv~lstat*age, Boston)
summary(fit5)

fit6 <- lm(medv~lstat + I(lstat^2),Boston)
summary(fit6)
attach(Boston)
par(mfrow  = c(1,1))

plot(medv~lstat)
points(lstat,fitted(fit6),col = "red", pch = 20)
plot(1:20, 1:20, pch = 1:20, cex = 2)



fix(Carseats)
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~.+Income: Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)
regplot <- function(x,y,...){
     fit = lm(y~x)
     plot(x,y)
     abline(fit, col = "red")
  
}

regplot(Price,Sales)
