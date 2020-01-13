#Question 3
library(maptools)
library(gstat)

interpolate<-function(z,datatable,grid,exp){
  idw_map<- idw(z~1, datatable, newdata=grid, idp=exp)
  im <- as.image.SpatialGridDataFrame(idw_map) # this is incredibly useful !!!!!!
  image(idw_map, "var1.pred", col=terrain.colors(20))
  contour(idw_map, "var1.pred", add=TRUE)
  plot(datatable, add=TRUE, pch="+")
}

data<-read.table("Fall 2019/Spaital Statistics/topo_data.txt", header = T)
head(data)

coordinates(data)<-c(1,2)

class(data)

data_grid<-spsample(data,"regular", n=3720)

class(data_grid)

gridded(data_grid)<-TRUE
class(data_grid) 

z <- data$Z
exp <- 2
map.2<- interpolate(z,data,data_grid,exp)


exp<-1 
map.1<- interpolate(z,data,data_grid,exp)

exp<-3 
map.3 <- interpolate(z,data,data_grid,exp)



#Question 4

library(maptools)
library(gstat)

interpolate<-function(z,datatable,grid,exp){
  idw_map<- idw(z~1, datatable, newdata=grid, idp=exp)
  im <- as.image.SpatialGridDataFrame(idw_map) # this is incredibly useful !!!!!!
  image(idw_map, "var1.pred", col=terrain.colors(20))
  contour(idw_map, "var1.pred", add=TRUE)
  plot(datatable, add=TRUE, pch="+")
}

data<-read.table("Fall 2019/Spaital Statistics/soils_data_2012.txt", header = T)
newData <- data<-read.table("Fall 2019/Spaital Statistics/soils_data_2012.txt", header = T)

head(data)

coordinates(data)<-c(1,2)

class(data)

data_grid<-spsample(data,"regular", n=3720)

class(data_grid)

gridded(data_grid)<-TRUE
class(data_grid) 

z <- data$CA
exp <- 2
map.2<- interpolate(z,data,data_grid,exp)


exp<-1 
map.1<- interpolate(z,data,data_grid,exp)

exp<-3 
map.3 <- interpolate(z,data,data_grid,exp)

data_1<-krige(z~1,data,data_grid,degree=1)
im <- as.image.SpatialGridDataFrame(data_1)
image(data_1,"var1.pred",col=terrain.colors(5))
contour(data_1,"var1.pred", add=TRUE)
first <-  plot(data,add=TRUE,pch="+")

data_2 <-krige(z~1,data, data_grid, degree=2)
im <- as.image.SpatialGridDataFrame(data_2)
image(data_2,"var1.pred",col=terrain.colors(5))
contour(data_2,"var1.pred", add=TRUE)
second <- plot(data_2,add=TRUE,pch="+") 

X <- newData$X
Y <- newData$Y
Z <- newData$CA

lm.1 <-lm(Z~X+Y)
summary(lm.1)

lm.2 <- lm(Z ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
summary(lm.2)

lm.3 <- glm(Z ~ Z+ Y, family="poisson", newData)
summary(lm.3)

# Question 5
library(maptools)
library(gstat)

interpolate<-function(z,datatable,grid,exp){
  idw_map<- idw(z~1, datatable, newdata=grid, idp=exp)
  im <- as.image.SpatialGridDataFrame(idw_map) # this is incredibly useful !!!!!!
  image(idw_map, "var1.pred", col=terrain.colors(20))
  contour(idw_map, "var1.pred", add=TRUE)
  plot(datatable, add=TRUE, pch="+")
}

data<-read.table("Fall 2019/Spaital Statistics/soils_data_2012.txt", header = T)
coordinates(data)<-c(1,2)

Z = data$CA

data_vg <- variogram(Z~1, data= data, cloud=TRUE)
plot(data_vg)

dataSV_edit <- plot(variogram(Z~1,data, cloud=TRUE), digitize=TRUE)
plot(dataSV_edit,data)

data_vg <- variogram(Z~1, data)
plot(data_vg,plot.number=TRUE)

vgm() 
show.vgms() 
vgm(1,"Sph", range=390)


model<-vgm("Gau")
data_vfit<-fit.variogram(data_vg, model)
data_vfit
plot(data_vg, model=data_vfit, plot.numbers=TRUE,pch="+") 

data_grid<- spsample(data,type="regular",n=3720)
gridded(data_grid)<-TRUE
fullgrid(data_grid)<-TRUE #topo_grid is spatial grid object
data_map<- krige ( Z~1, data,data_grid,data_vfit)

im<- as.image.SpatialGridDataFrame(data_map)
image(im,"var1.pred",col=terrain.colors(20))
contour(data_map,"var1.pred",add=TRUE)
plot(data,add=TRUE,pch="+",cex=1.0)

summary(data_map)

