library(rgdal)
library(sp)
library(spdep)

# Data pulled from the local government of New Orleans
#https://portal-nolagis.opendata.arcgis.com/datasets/total-of-registered-voters
# There was an election this past saturday, so the data is of interest. 

#Question 1
data <-  readOGR("C:/Users/Ryan/Desktop/Fall 2019/Spaital Statistics/Total_#_of_Registered_Voters.shp")
summary(data)
proj4string(data) <-CRS("+init=epsg:27700")
csr <-slot(data,"proj4string")

voting_data<-as(data,"data.frame")
class(voting_data)
head(voting_data)

plot(voting_data)
plot(data)


# Question 2
# This plot is of the total amount of registered voters per parish. 
# Parish is important to note since, it based on older French systems.
# It is a rather unique feature of geography in New Orleans.
choropleth <- spplot(data,"Registered")



data_nb<-poly2nb(data)
matrix<-coordinates(data)
plot (data_nb, matrix)

delauney_Scot <- tri2nb(matrix)
class(delauney_Scot)
summary(delauney_Scot)


Gabriel_Scot<-graph2nb(gabrielneigh(matrix))
relative_neigh_Scot<-graph2nb(relativeneigh(matrix))
k3neigh_Scot<-knn2nb(knearneigh(matrix, k=3))
plot.nb(delauney_Scot,matrix)
plot(data,add=TRUE)
plot.nb(Gabriel_Scot,matrix)

plot(data,add=TRUE)
plot.nb(relative_neigh_Scot,matrix)
plot(data,add=TRUE)
plot(k3neigh_Scot,matrix)
plot(data,add=TRUE)

contig_listw <- nb2listw(data_nb, style="B", zero.policy=TRUE)
summary(contig_listw,zero.policy=TRUE)


# Question 3 Part A
#Calculating the weights 
delaun_listw <- nb2listw(delauney_Scot, style="B")
nearneigh_listw <-nb2listw(k3neigh_Scot, style="B")
Gabriel_listw <- nb2listw(Gabriel_Scot, style="B", zero.policy=TRUE)
rel_neigh_listw <-nb2listw(relative_neigh_Scot, style="B", zero.policy=TRUE)

#Looking at Spatial Lags 
lag <- nblag(data_nb, maxlag = 9)


#Looking at closeness with rook criteria
rook <- cell2nb(7,7, type = "rook",torus = T)

#Looking at closeness with queen criteria
queen <- cell2nb(7,7,type = "queen", torus = T)

#Had difficultity using the suggested functions with dataset
# By taking the correlation of the  product of the transpose of the SAR matrix it is possible to 
# to get a consistent estimator for expected autocorrelation 
# See Haining 1990 for details of method  
# 

data_w <- nb2listw(data_nb)
n <- length(data_nb)
uncorr_x <- rnorm(n)

cor(uncorr_x,lag(data_w,uncorr_x))

rho <- .5
Irw <- (diag(n) - rho)* listw2mat(data_w)
SARcov <- solve(t(Irw %*% Irw))
SARcovL <- svd(SARcov + t(SARcov)/2)
uncorr_x <- as.matrix(uncorr_x) 
SARcovL <- as.matrix(SARcovL)

#Question 3 B
# The p-value is greater than .05, thus you fail to reject the null
# The distribution of feature values could be the result of randomness

# While this is difficult to accept in a qualitaitve sense, it could be that since 
# people are displaced so often because of flooding, it makes the process more random. 
# It could make it so that there is a less significant relationship between voting populations
# and geography. But, this is only a suggestion. 
test.1 <- moran.test(uncorr_x,listw = delaun_listw)

#Question 4
# The plot seems to reinforce the moran test. There are no clear clusters appearing in the data. 
# It appears that the values are slightly more negative though than positive. 
test.plot <- moran.plot(uncorr_x,listw = delaun_listw)

meanVal <- mean(uncorr_x)
meanVal

sd<-sd(uncorr_x)

zScore <- (meanVal/sd)



