
#4.6.1 Logistical Regression Lab 
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9])

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family = binomial)

coef(glm.fits)

summary(glm.fits)$coef

summary(glm.fits)$coef[,4]

glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]

contrasts(Smarket$Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

attach(Smarket)
table(glm.pred,Direction)

mean(glm.pred == Direction)


train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]

glm.fits=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
             data=Smarket ,family=binomial ,subset=train)

glm.probs = predict(glm.fits, Smarket.2005, type = "response")

glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred ,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!= Direction.2005)

glm.fits = glm(Direction ~ Lag1 + Lag2,
               data = Smarket, family = binomial,
               subset = train)

glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)

returns <- predict(glm.fits, newdata = data.frame(Lag1 = c(1.2,1.5),
 Lag2 = c(1.1,-.8)), type = "response")

# Linear Discriminant Lab
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >= .5)
sum(sum.pred$posterior[,1] < .5)


#Quadratic Discriminant Analysis
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
              subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class,Direction.2005)

# K- Nearest Neighbors 
library(class)
train.X <- cbind(Lag1,Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
test.X <- cbind(Lag1,Lag2)[!train,]
train.Direction = Direction [train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
mean(knn.pred == Direction.2005)


# An Application to Caravan Insurence Data 
dim(Caravan)
attach(Caravan)
summary(Purchase)

#Normalize so SD = 1 
standarize.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standarize.X[,1])
var(standarize.X[,2])

test <- 1:1000
train.X <- standarize.X[-test,]
test.X <- standarize.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)

knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/26

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15

glm.fits = glm(Purchase ~., data = Caravan, family = binomial,
            subset = -test)
glm.probs <- predict(glm.fits, Caravan[test,], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs >.5] = "Yes"
table(glm.pred, test.Y)

glm.pred= rep("No",1000)
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)

