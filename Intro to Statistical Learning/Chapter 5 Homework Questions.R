# Chapter 5 Question 2 Part G
x <- 1 : 10000
plot(x, 1 - (1 - 1/x)^x)


#Chapter 5 Question 2 Part H
store <- rep(NA,10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep = TRUE) == 4) > 0
}

mean(store)

#chapter 5 Question 5 Part A 
library(ISLR)
attach(Default)

set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = binomial)
summary(fit.glm)

#Part B 
train <- sample(dim(Default)[1], dim(Default[1])/2)

fit.glm.2 <- glm(default ~ income + balance, data = Default, 
              family = "binomial", subset = train)

summary(fit.glm)

#Part B iii
probs <- predict(fit.glm, newData = Default[-train,], type = "response")
pred.glm <- rep("No", length (probs))
pred.glm[probs > .5] <- "Yes"

#Part B iv 
mean(pred.glm != Default[-train,]$default)

# Part C 
train <- sample(dim(Default[1]), dim(Default)[1]/2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict (fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No", length(probs))     
pred.glm[probs > .5] <- " Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# There is varation in the results depending on which observations are placed in the validation and trainning set


#D
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)


#Question 6
set.seed(1)
attach(Default)

fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#Part B
boot.fn <- function(data, index){
  fit <- glm(default ~ income + balance, data = data, family = "binomial",
             subset = index)
  return(coef(fit))
  
  
}

# Part C
library(boot)

boot(Default,boot.fn, 1000) 

# Part D

# The difference in results appears to be non-significant. 


# Question 7
set.seed(1)
attach(Weekly)  
fit.glm <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit.glm)            

fit.glm.3 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = "binomial")
summary(fit.glm.3)
predict.glm(fit.glm.3, Weekly[1,], type = "response") > .5


 # The first observation is not correctly classified 
error <- rep(0, dim(Weekly)[1])

error <- rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  fit.glm <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
  pred.up <- predict.glm(fit.glm, Weekly[i, ], type = "response") > 0.5
  true.up <- Weekly[i, ]$Direction == "Up"
  if (pred.up != true.up)
    error[i] <- 1
}


mean(error)


#Question 8 
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# Question 8 Part B 
plot(x,y)

# Question 8 Part C
library(boot)
set.seed(1)
Data <- data.frame(x,y)
fit.glm.1 <- glm(y~x)
cv.glm(Data, fit.glm.1)$delta[1]

# Question 8 Part C ii 
library(boot)
set.seed(1)
Data <- data.frame(x, y)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]


# Question 8 Part iii
fit.glm.3 <- glm(y ~ poly(x,3))
cv.glm(Data, fit.glm.3)$delta[1]

# Question 8 Part C iv
fit.glm.4 <- glm(y~ poly(x,4))
cv.glm(Data,fit.glm.4)$delta[1]

set.seed(3)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

# The quadradic model is the best fit. Considering the model was generated with a quadratic
# this is not suprising. 

# Question 9 

# Question 9 Part A
library(MASS)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

# Question 9 Part B
se.hat <- sd(medv)/ sqrt(length(medv))
se.hat  

# Question 9 Part C 
library(boot)
set.seed(1)
boot.fn <- function(data,index){
 mu <- mean(data[index])
 return(mu)
}

boot(medv, boot.fn, 1000)

# Question 9 Part D
t.test(medv)


CI.mu.hat <- c(22.53 - 2 * .4119, 22.53 + 2 * .4119)
CI.mu.hat

#Question 9 Part E 
med.hat <- median(medv)
med.hat

#Question 9 Part F
boot.fn <- function(data, index){
  mu <- median(data[index])
  return(mu)
}

boot(medv, boot.fn, 1000)

# Question 9 Part G
percent.hat <- quantile(medv, c(0.1))
percent.hat

boot.fn <- function(data, index){
  mu <- quantile(data[index], c(0.1))
  return(mu)
  
}  

# Question 9 Part H 
boot(medv, boot.fn, 1000)

