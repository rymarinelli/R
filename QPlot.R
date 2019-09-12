#install.packages("ggplot2")
library(ggplot2)
?qplot

stats <- read.csv(file.choose())

qplot(data = stats, x = Internet.users)
qplot(data = stats, x = Income.Group, y = filter, size = I(3), color = I("blue"))

qplot(data = stats, x = Income.Group, y = Birth.rate, geom = "boxplot")
  
#Visualizing 
qplot(data = stats, x=  Internet.users, y = Birth.rate,
      color = I("red"), size = I(4))

qplot(data = stats, x=  Internet.users, y = Birth.rate,
      color = Income.Group, size = I(4))

#Creating Data Frame

mydf <- data.frame (Country = Countries_2012_Dataset, Code = Codes_2012_Dataset, Region = Codes_2012_Dataset)

head(mydf)

colnames(mydf) <- c("County", "Code","Region")

head(mydf)
tail(mydf)
summary(mydf)


#Merging Data Frames
head(stats)
head(mydf)

merged <- merge(stats,mydf, by.x = "Country.Code", by.y = "Code")
merged$Country <- NULL
str(merged)
merged
head(merged)
Region <- merged$Region

qplot(data = merged, x = Internet.users, y = Birth.rate)
qplot( data = merged, x = Internet.users, y = Birth.rate,
      size= I(2), shape  = I(19), alpha = I(0.6), colour = I("Red")
