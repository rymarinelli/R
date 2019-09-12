library(ISLR)

#Question 8
attach(Auto)

lm.fit.1 <- lm(mpg~horsepower)
summary(lm.fit.1)

# Part i 
# There is a signficant relationship. It is slightly
# negative. 

# Part ii
summary(lm.fit.1)$r.sq
# It is statistically significant. The R-squared 
# is .6. This seems rather high. 

# Part iii
# It is negative relationship. 

# Part iv
predict(lm.fit.1, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit.1, data.frame(horsepower=c(98)), interval="prediction")

# The CI is {23.97308,24.96108}
# The PI is {14.8094,34.12476}


#Part v
plot(horsepower,mpg)
abline(lm.fit.1)
# The resiudals appear to take on a quadratic shape.
# Perhaps, linear regression is not the correct
# specfication to use in this instance. 

# Question 9
par(mfrow = c(2,2))
pairs(Auto)

cor(subset(Auto, select = -name))
# The variables seems to be somewhat correlated.
library(car)

lm.fit.2 <- lm(mpg~.-name, data = Auto)
summary(lm.fit.2)

vif(lm.fit.2)
# There is a significant amount of correlation
# Cylcinders,displacement,horsepower, and weight
# are a bit too coorelated. Multicolinearity
# is problematic. 

# Question 9 Part c 
#i + ii 

# Displacement, weight, year, and origin are signficant. 
# Displacement has a slight positive relationship.
# Weight has a slight negative relationship.
# Year has a positive relationship. It is a bit more
# pronounced than the coefficents of the other estimates. 
# Origin has a postive relationship. This makes
# sense different countries have different standards
# Year is also positive.

#  Question 9 Part C iii
# The signficance of year suggests that newer cars
# are more fuel efficent. This may come from newer
# cars leveraging more fuel efficent technologies.


# Question 9 Part D 
plot(lm.fit.2)
# The residuals seems to suggest  some curvature
# A linear model might not be best. 
# Also, leverage appears to be a bit of an issue. 
# There could be an outlier as one point seems to 
# increase the leverage rather significantly. 
# There are two points around .8 and .16 respectively. 

# Question 9 Part E 
lm.fit.3 <- lm(mpg ~ cylinders*displacement + displacement*weight, data = Auto)
summary(lm.fit.3)

# The interaction term of displacement and weight is signficant. 
#Weight its self is negatively related with mpg. However, it appears that the larger cars 
#may also have greater displacement which is also related to mpg. 

lm.fit.4 <- lm(mpg ~ year*weight + year + weight + displacement, data = Auto)
summary(lm.fit.4)
# The interaction with year and weight is significant. Separately, year and weight are both positively assiociated 
#  with mpg. However, their interaction term is negative. It could be that newer cars that are heavier are less fuel efficent.
# It could be that there are more trucks that are less fuel efficent. 


#Question 10
attach(Carseats)
# Part A
lm.fit.5 <- lm(Sales ~ Price + Urban + US)
summary(lm.fit.5)
contrasts(Urban)
contrasts(US)

#Part B
# For price, there is a negative relationship. For every 500 they lower, they will sell an additional 1000 cars. 
# Urban is not statistically signifcant. 
# If a car is made in the US, it will sell an additional 1000 units. 

#Question 10 C
# Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes

#Question 10 D
# You may reject the null for Price and for the binary variable US. US

#Question 10 E
lm.fit.6 <- lm(Sales ~ Price + US)

#Question 10 F
summary(lm.fit.5)$fstatistic[1]
summary(lm.fit.6)$fstatistic[1]       

summary(lm.fit.5)
summary(lm.fit.6)

# Given the F-stat, the second strutural equation seems to be better fit. 
# The RSE from the second regression is also lesser as well. 
# This demonstrates that is likely a mild improvement through removing
# non-significant parameters from the model. 

# Question 10 G 
# Finds Confidence Intervals.
confint(lm.fit.5)
confint(lm.fit.6)

#Question 10 H
plot(predict(lm.fit.6), rstudent(lm.fit.6))

# When considering the distribution of studentizes residuals, there appears to be issues.
# Some of the studentized residuals, which are residuals divided by their SD are 3 SD away from the mean of
# residuals. 

plot(lm.fit.6)


#Question 11 
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# Question 11 Part A
lm.fit.7 <- lm(y???x+0)
summary(lm.fit.7)
# X is positively related to y. It is a significant relationship. 

#Question 10 Part B
lm.fit.8 <- lm(x~y + 0)
summary(lm.fit.8)
 
#Question Part C
# Without an intercept, the same line may be drawn using the points. 


#Question 11 Part E
(sqrt(length(x)-1 )* sum(x*y)) / sqrt(sum(x*x) * sum(y*y) - (sum(x*y)^2))

# Question 11 Part F 
lm.fit.9 = lm(y~x)
lm.fit.10= lm(x~y)



# Question 11 Part F
# The t-value is the same for both 
coefficients(summary(lm.fit.9))[1,3] == coefficients(summary(lm.fit.10))[1,3]

#Question 12 Part A
# The regressions are the same when the sum of squares of the the predictors is equal to the sum of the squares for the X. 

#Question 12 Part B
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit.11 = lm(y~x+0)
lm.fit.12 = lm(x~y+0)
coef(summary(lm.fit.11))
coef(summary(lm.fit.12))

#Question 12 Part C 
x <- seq(1:100)
y <- seq(1:100)

lm.fit.13 <- lm(x~y +0)
coef(lm.fit.13)
lm.fit.14 <- lm(y~x + 0)
summary(lm.fit.14)

# Since the sum of the squares are the same for x and y, the values are the same. Although, they are pulled from the same sequence
# of numbers. 

#Question 13 Part A
x <- rnorm(100)

# Question 13 Part B 
eps <- rnorm(100,0, sqrt(.25))

#Question 13 Part C
y = -1 + 0.5*x + eps
length(y)
# The intercept is -1 and beta-one is .5. 

#Question 13 Part D 
plot(x,y)
#There is a positive relationship with x and y values. 

#Question 13 Part E
lm.fit.15 <- lm(y~x)
summary(lm.fit.15)
# It does a decent job at predicting the the model. The intercept is nearly -1 and the beta-one is nearly .5

#Question 13 Part F
plot(x,y)
abline(lm.fit.15, lwd = 3, col = 2)
abline(-1,.5, lwd = 3, col = 3)
legend(-1, legend = c("model fit", "population regression"), col = 2:3, lwd = 3)

#Question 13 Part G
lm.fit.16 <- lm(y~ x + I(x^2))
summary(lm.fit.16)$r.sq
summary(lm.fit.15)$r.sq
# The p-value for the squared term is not statistically significant. Also, the f-statistic for the structural model
# is higher than the larger model. This suggests that the model without including the x^2 is better


#Question 13 Part H
eps <- rnorm(100, mean =0, sd = 0.10)
y = -1+0.5*x+eps
lm.fit.16 = lm(y~x)
summary(lm.fit.16)


plot(y~x)
abline(lm.fit.16, col = "red")
legend("bottomright", "Regression Line", lwd = 1, col = "red", bty = "n")

lm.fit.17 <- lm(y~x + I(x^2))
summary(lm.fit.17)

# The decrease in variance allows for the model to tighten up its confidence intervals and enables the model
# to have a lesser MSE and SE for the parameters. This is scene in the distribution of points.

#Question 13 Part I
eps <- rnorm(100, mean =0, sd = 2)

y = -1+0.5*x+eps
lm.fit.18 = lm(y~x)
summary(lm.fit.18)


plot(y~x)
abline(lm.fit.17, col = "red")
legend("bottomright", "Regression Line", lwd = 1, col = "red", bty = "n")

lm.fit.19 <- lm(y~x + I(x^2))
summary(lm.fit.19)

# The opposing effect can be seen with a high varience. The R-sqared is lower for both models.
#The squared term is also not signficant


# Question 13 Part J
eps <- rnorm(100, mean =0, sd = 1)
y = -1+0.5*x+eps
lm.fit.20 <- lm(y~x)

eps <- rnorm(100, mean =0, sd = .5)
y = -1+0.5*x+eps
lm.fit.21 <- lm(y~x)

eps <- rnorm(100, mean =0, sd = 2)
y = -1+0.5*x+eps
lm.fit.22 <- lm(y~x)

confint(lm.fit.20)
confint(lm.fit.21)
confint(lm.fit.22)

# The CI seem to hover around the same point,.5, as there is higher varience the more skewed distributions are
# and the wider the CI have to be to encapsulate the mean.

#Question 14
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
# The intercept is 2
# beta one is 2
# beta two is .3 

#Question 14 Part B
cor(x1,x2)
plot(x1,x2)
# The variables are positively correlated. The correlation coefficent is .83

lm.fit.21 <- lm(y ~ x1+x2)
lm.fit.22 <- lm(y ~ x1 + x2 + x1*x2)
summary(lm.fit.21)
summary(lm.fit.22)$fstatistic

# Question 14 Part C

# X1 is signficant to the model. The null that there is no relationship may be rejected. 
# X2 is not signficant. It has a p-value that is too high. Also the F-statistic may be rejected
# so at least one of the predictors is signficant to the model. 
# The R-squared is fine as well. 
# Interacting with X2 makes the model weaker None of the values are signficant, and the F-statistic suggests that none of the 
# of the predictors are signficant to the model. 

# Question 14 Part D
lm.fit.23 <- lm(y ~ x1)
summary(lm.fit.23)
# X1 is signficant in this model

# Question 14 Part E
lm.fit.24 <- lm(y ~ x2)
summary(lm.fit.24)
#X2 is signficant in this model

#Question 14 Part F
library(car)
vif(lm.fit.22)
# Multicolinearity is a source of the issue. Because the predictors are highly coorlated, it is difficult to oberserve their 
# indivudal effects. This bias is amplified when looking at interaction effects as well. 

#Question 14 Part G
 x1=c(x1, 0.1) 
 x2=c(x2, 0.8) 
 y=c(y,6)

lm.fit.25 <- lm(y~x1 + x2)
vif(lm.fit.25)
vif(lm.fit.21) #Model without the extra observation 
# The extra observation reduces the the colinearity.

summary(lm.fit.25)
# x2 is now signficiant, while x1 is not 
 
par(mfrow=c(2,2))
plot(lm.fit.21)
plot(lm.fit.25)

# The points are high leverage points 

plot(predict(lm.fit.21), rstudent(lm.fit.22))
plot(predict(lm.fit.25), rstudent(lm.fit.25))

# None of the points are 3 standard deviations away. This suggests that there are not any clear outliers. 


#Question 15
library(MASS)
summary(Boston)
attach(Boston)

names(Boston)

y <- crim

lm.fit.26 <- lm(y~zn)
summary(lm.fit.26)

lm.fit.27 <- lm(y~indus)
summary(lm.fit.27)

lm.fit.28 <- lm(y~chas)
summary(lm.fit.28)

lm.fit.29 <- lm(y ~ nox)
summary(lm.fit.29)

lm.fit.30 <- lm(y ~ rm)
summary(lm.fit.30)

lm.fit.31 <- lm(y ~ age)
summary(lm.fit.31)

lm.fit.32 <- lm(y ~ dis)
summary(lm.fit.32)

lm.fit.33 <- lm(y ~ rad)
summary(lm.fit.33)

lm.fit.34 <- lm(y ~ tax)
summary(lm.fit.34)

lm.fit.35 <- lm(y ~ ptratio)
summary(lm.fit.35)

lm.fit.36 <- lm(y ~ black)
summary(lm.fit.36)

lm.fit.37 <- lm(y ~ lstat)
summary(lm.fit.37)

# all the simple linear regressions expect chas are signficant
 
lm.fit.38 <- lm(y~., data = Boston)
summary(lm.fit.38)
# only crime and rm are signficant in this model. All the others are not signficant and the null cannot be rejected

x <- c (coefficients(lm.fit.26)[2], coefficients(lm.fit.27)[2], coefficients(lm.fit.28)[2], coefficients(lm.fit.29)[2],
        coefficients(lm.fit.30)[2], coefficients(lm.fit.31)[2], coefficients(lm.fit.32)[2], coefficients(lm.fit.33)[2],
        coefficients(lm.fit.34)[2], coefficients(lm.fit.35)[2], coefficients(lm.fit.36)[2], coefficients(lm.fit.37)[2])

y <- coefficients(lm.fit.38[2:14])

plot(x,y)
# There is a signficant different for nox between the univarate and multivarate casecase.names
names(Boston)
# Question 15 Part D
lm.fit.39 <- lm(crim ~ poly(zn,3) )
summary(lm.fit.39) 
# Some evidence of higher order relationships 

lm.fit.40 <- lm(crim ~ poly(indus,3))
summary(lm.fit.40)
# Some evidence of higher order relationships 

lm.fit.41 <- lm(crim ~ poly(chas, 3))
summary(lm.fit.41)

lm.fit.42 <- lm(crim ~ poly(nox,3))
summary(lm.fit.42)
# Some evidence of higher order relationships 

lm.fit.43 <- lm(crim ~ poly(rm,3))
summary(lm.fit.43)
# Some evidence of higher order relationships for the squared term

lm.fit.44 <- lm(crim ~ poly(age,3))
summary(lm.fit.44)
# Some evidence of higher order relationships 

lm.fit.45 <- lm(crim ~ poly(dis,3))
summary(lm.fit.45)
# Some evidence of higher order relationships 

lm.fit.46 <- lm(crim ~ poly(rad, 3))
summary(lm.fit.46)
# Some evidence of higher order relationships for the squared term

lm.fit.47 <- lm(crim ~ poly(rad,3))
summary(lm.fit.47)
# Some evidence of higher order relationships up until the squared term

lm.fit.48 <- lm(crim ~ poly(tax,3))
summary(lm.fit.48)
# Some evidence of higher order relationships up until the squared term

lm.fit.49 <- lm(crim ~ poly(ptration, 3))
summary(lm.fit.49)

lm.fit.50 <- lm(crim ~ poly(black,3))
summary(lm.fit.50)

lm.fit.51 <- lm(crim ~ poly(lstat, 3))
summary(lm.fit.51)

lm.fit.52 <- lm(crim ~ poly(lmedv, 3))
summary(lm.fit.52)

# Most of the terms have signficant terms when squared or cubed. 
# Expection is black and chas. 
