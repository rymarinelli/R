library(rgdal)
library(classInt)
library(spdep)
library(RColorBrewer)
library(car)
library(spatialreg)
library(e1071) 

setwd("C:/Users/Ryan/Desktop/Fall 2019/Spaital Statistics/PuertoRico_data")
pr<-readOGR("PuertoRico_SPCS.shp")
pr.nb <- read.gal("PuertoRico.gal")
plot(pr)
summary(pr)


points <- coordinates(pr)
plot(pr, lwd = .5)
plot(pr.nb, points, add=T, col="red")

farm.den07 <- pr$nofarms_07/pr$area
farm.den02 <- pr$nofarms_02/pr$area
plot(farm.den02)
mean(farm.den02)
mean(farm.den07)


#Ploting density of agriculture for farms in 2007
pal.gray <- gray.colors(4)
pal.green <- brewer.pal(4,"Greens")
q5.den <- classIntervals(farm.den07,5,style="quantile") 
cols.den <- findColours(q5.den, pal.green)
plot(pr, col=cols.den)
brks.den <- round(q5.den$brks,3)
leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
legend("topright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
plot(pr.nb, points, add=T, col="red")

#Plotting density of agriculture for farms in 2002
pal.gray <- gray.colors(4)
pal.green <- brewer.pal(4,"Greens")
q5.den <- classIntervals(farm.den02,5,style="quantile") 
cols.den <- findColours(q5.den, pal.green)
plot(pr, col=cols.den)
brks.den <- round(q5.den$brks,3)
leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
legend("topright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
plot(pr.nb, points, add=T, col="red")

#Testing for autocorrelation 
pr.listw <- nb2listw(pr.nb, style="B")

# P-value less than .05, reject null
moran.test(farm.den07, pr.listw)

#P-value less than .05, reject null
geary.test(farm.den07, pr.listw)

# P-value less than .05, reject null
moran.test(farm.den02,pr.listw)

# P-value less than .05, reject null
geary.test(farm.den02,pr.listw)

# It appears that there is positive autocorrelation
# This suggests 
moran.plot(farm.den07, pr.listw, pch= 15)
moran.plot(farm.den02, pr.listw, pch= 15)

# The varirogram in 2002 is a lot less steep 
pr.v <- variogram(farm.den07 ~ 1,  pr)
pr.v.fit <- fit.variogram(pr.v, vgm(11,"Sph", "30000", 1))
plot(pr.v, pr.v.fit)

pr.v.2 <- variogram(farm.den02 ~ 1,  pr)
pr.v.fit.2 <- fit.variogram(pr.v.2, vgm(11,"Sph", "30000", 1))
plot(pr.v.2, pr.v.fit.2)

# High R-Squared
#.6 beta-coef is significant
# Res as expected. They have a lot more leverage since, they are closer. 
# Pretty interesting plot 
lm.1 <- lm(farm.den07 ~ farm.den02)
summary(lm.1)
lm.morantest(lm.1,pr.listw)
plot(lm.1)

# Graph of residuals 
res <- resid(lm.1)
q5.res <- classIntervals(res,5,style="quantile")
cols.res <- findColours(q5.res, pal.red)
plot(pr, col=cols.res)
brks.res <- round(q5.res$brks,3)
leg.txt  <- paste(brks.res[-6], brks.res[-1], sep=" - ")
legend("bottomright", fill=attr(cols.res,"palette"), legend=leg.txt ,bty="n")

pr.f <- read.csv(file="PR-farm-data.csv")
y <- log(farm.den02 + .04)
#y <- log(farm.den07 + .04)
r <- pr.f$rain_mean

#Rain is statistically significant
#Normality assumptions fail
# OLS is not BLUE
# Biased Estimates 
lm.2 <- lm(y ~ r)
summary(lm.2)
shapiro.test(resid(lm.2))

#Run SAR model
# Slightly better normality test
# likely still biased but less so
pr.listw <- nb2listw(pr.nb, style="W")
pr.listb <- nb2listw(pr.nb, style="B")
sar <- errorsarlm(y ~ r, listw = pr.listw)
summary(sar)
if.res <- residuals(sar)
shapiro.test(if.res)

# Is an even distribution of larger points 
# San Juan is east and capital
# Archibo is northwest
# Mayagues is southwest
# Ponce is south center 
# Caguas is south east
adm <- factor(pr.f$ADM, levels=1:5, labels= c("San Juan", "Arecibo", "Mayaguez", "Ponce", "Caguas"))


# Running a complete SAR Model
sar.chi<-lagsarlm(pr.f$irr_farms_07~ pr.f$rain_mean + pr.f$elev_mean + adm, data = pr.f, pr.listw)
summary(sar.chi)
sar.chi.2 <- lagsarlm(pr.f$irr_farms_02~ pr.f$rain_mean + pr.f$elev_mean + adm, data = pr.f, pr.listw)
summary(sar.chi.2)

#Basic Info _farms_02)
summary(pr.f$rain_mean)
summary(pr.f$elev_mean)

par(mfrow=c(2,2))
hist(pr.f$irr_farms_07, main = "Number of farms in 2007", xlab = "farms")
hist(pr.f$irr_farms_02, main = "Number of farms in 2002", xlab = "farms")
hist(pr.f$rain_mean, main = "Distribution of Rain ", xlab = "mean of rain")
hist(pr.f$elev_mean, main = "Distribution of Elevation", xlab = "mean of elevation")

test <- log(pr.f$irr_farms_07)
kurtosis(pr.f$irr_farms_02)
kurtosis(pr.f$irr_farms_07)
t.test(pr.f$irr_farms_02, pr.f$irr_farms_07)
summary(pr.f$irr_farms_07)
summary(pr.f$irr)
        
mean(farm.den02)       
mean(farm.den07)
t.test(farm.den02,farm.den07)
