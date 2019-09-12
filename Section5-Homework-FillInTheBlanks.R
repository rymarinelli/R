#Set the Working Directory
getwd()
setwd("~/Learning R")
getwd()

#Import the csv dataset
data <- P2_Section5_Homework_Data


#Explore the data
data
header(data)      #check top 6 rows
tail(data, n=7) #check bottom 7 rows
str(data)      #check the structure of the data frame
summary(data)      #check the summary of the data

#Did you pick up that there is more than one year in the data?
#From the challenge we know that there are two: 1960 and 2013

#Filter the dataframes
data1960 <- data[data$Year == 1960,]
data2013 <- data[data$Year == 2013,]

#Check row counts
nrow(data1960) #187 rows
ncol(data2013) #187 rows. Equal split.

#Create the additional dataframes
add1960 <- data.frame(Code = Country_Code, Life.Exp = Life_Expectancy_At_Birth_1960)
add2013 <- data.frame(Code = Country_Code, Life.Exp = Life_Expectancy_At_Birth_2013)

#Check summaries
summary(add1960)
summary(add2013)


#Merge the pairs of dataframes  
merged1960 <- merge(data1960, add1960, by.x = "Country Code", by.y ="Code")
merged2013 <- merge(data2013, add2013, by.x = "Country Code", by.y ="Code")

#Check the new structures
str(merged1960)
str(merged2013)

#We can see an obsolete column in each of the merged dataframes
#Column "Year" is no longer required. Let's remove it
merged1960$Year <- NULL
merged2013$Year <- NULL

#Check structures again
(merged1960)
(merged2013)

#Visualization time
library(ggplot2)

str(merged1960)
Fertility  <- merged1960$`Fertility Rate`

#Visualize the 1960 dataset
qplot(data = merged1960, x = Rate ,  xlab = "Fertility Rate", y=Life.Exp,
      color = Region,                               #colour
      size=I(5), 
      alpha=I(0.6),                               #transparency
      main ="Life Expectancy vs Fertility (1960)"
      #title
   )

rate2013 <- merged2013$`Fertility Rate`
#Visualize the 2013 dataset
qplot(data = merged1960, x = rate2013, xlab = "Fertility Rate", y=Life.Exp,
      color = Region,                               #colour
      size=I(5), 
      alpha=I(0.6),                               #transparency
      main ="Life Expectancy vs Fertility (2013)" #title
)





