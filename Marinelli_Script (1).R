#Ryan Marinelli
#Homework 2
#11-11 - 2017

#Reviewed spatstat.org for assistance for this assignment

library(spatstat)
data("nztrees")
xlim <- c(0.0, 1.0)
ylim<- c(0.0, 1.0)
attach(nztrees) # necessary!
nztrees_ppp <- ppp(nztrees$x, nztrees$y, xlim, ylim) # convert to a ppp object
summary(nztrees_ppp )

dmap <- density(nztrees, sigma=0.5)
range(dmap$v)
class(dmap)

#Plots points over graph
plot(dmap)
contour(dmap)



nn_dens<-nndensity(nztrees, k=20)
plot(nn_dens)




nn_dens<-nndensity(nztrees, k=20)
plot(nn_dens)
```

#Finds ideal sigma
sigma <-bw.diggle(nztrees)
d_kde <- density(nztrees, sigma)
plot(d_kde)
plot(nztrees, pch="+",add=TRUE)



library(spatstat)
plot(japanesepines)
sects <- quadratcount(japanesepines, nx = 3  )
sects
plot(sects, main = "")

#Gives some information on the distribution
plot(japanesepines, add = TRUE, pch = 16, cex = 0.5)


chisq.test<- quadrat.test(sects)
chisq.test
print(chisq.test)


#Provides More Information on first map
plot(chisq.test, main = "", cex = 1.5, col = "red")
plot(japanesepines, add = TRUE, cex = 0.5, pch = 16)


#Density Map
pineDensity <- density(japanesepines, sigma = 0.1)
plot(pineDensity, main = "")

#Adding events to Density Map
plot(pineDensity, main = "", ribbon = FALSE)
plot(japanesepines, add = TRUE, cols = "white", cex = 0.5, pch = 16)



# 3D Density Map 
persp(pineDensity, theta=70, phi=25, shade=0.4)
persp(pineDensity, theta=70, phi=25, shade=0.4, main = "")


# The first graph that lumps the Japanese pines into a 3 by 3 gives some useful information.
# It starts out by helping us to understand the data through some basic visual inspect. For instance, 
# there is clustering in the upper-middle quadrants and the left-middle quadrant. This provides some higher 
# level data. 
# 
# The second plot takes things a bit further. It is essentially starting to intergrate statistical testing 
# into the mapping. The test shown here is for over representation or under representation of classes in a
# particular sector. The results of the testing is rather conclusive, that there is no over or unders representation. 
# This does vary a bit between quadrants, it appears the lower-middle quadrant is almost experiencing some statistically 
# significant results. This appears to be the case for the lower-right as well. 
# 
# The third density plot is also a bit useful. There is higher density in the upper-middle and middle sections.
# It is essentially providing a continous represetnation of the data as to the more discrete regions. When considering the 
# fourth graph, it does not provide signficantly more information. It  mostly provides secondary reinforcement of the data
# 
# The fifth graph is rather useful. It provides a 3-D representation of the graph. This information 
# would be useful to understand the mathematics more. For instance, there is a clear peak, and the slope becomes
# signficantly more pronouced along with the smoothness of the curves. When considering the data in this lens, 
# this graphic provides the most information. 
