#8A
college <- read.csv("Learning R/College.csv")

#8B
fix(college)


rownames (college ) <- college [,1] 
fix(college)

college <- college [,-1]
fix(college)

#8C i
summary(college)

#8C ii 
pairs(college[,1:10])

#8C iii
plot(college$Private, college$Outstate)


#8C4
Elite <- rep("No",nrow(college ))

# Filters Yes into the logical vector
Elite[college$Top10perc >50]=" Yes"

# Creates factor variables  
Elite=as.factor(Elite)

# Creates a data frame with college and the elite vector
college = data.frame(college, Elite)

#Summarizes the total 
summary(college$Elite)

#Plots the relationship between elite students and students that are out of state 
plot(college$Elite,college$Outstate)

structure(college)

#8C5
hist(college$Top10perc)

#Breaks the graphing area into multiple sections
par(mfrow=c(2,2)) 

#Graphs the Distribution of Apps
hist(college$Apps)

#Graphs the distribution of alumini that participate
hist(college$perc.alumni, col=2)

#Graphs the student falculty ratio 
hist(college$S.F.Ratio, col=3, breaks=11)

#Graphs Expenture 
hist(college$Expend, breaks=100)

par(mfrow=c(1,1))
scatter.smooth(college$Outstate, college$Grad.Rate)

#_____________________________________________________________________________________________________________
auto <- read.csv("Learning R/Auto.csv", header = TRUE, na.strings = "?")

#9A
auto <- na.omit(auto)
dim(auto)
summary(auto)

str(auto)

#9B
sapply(auto[,1:7], range)
sapply(auto[,1:7], sd)
sapply(auto[, 1:7], sd)

#9D
#Remove 10th percentile and 85th, redo SD and sd 

newAuto = auto[-(10:85),]
dim(newAuto) == dim(auto) - c(76,0)
newAuto[9,] == auto[9,]
newAuto[10,] == auto[86,]

sapply(newAuto[,1:7], range)
sapply(newAuto[,1:7], sd)
sapply(newAuto[, 1:7], sd)

pairs(auto)
plot(auto$mpg, auto$weight)
# Heavier weight correlates with lower mpg.
plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.
plot(auto$mpg, auto$year)

# 10
library(MASS)

?Boston
nrow(Boston)
ncol(Boston)
str(Boston)
head(Boston)

colnames(Boston) <- c("Crime","Zoning","Industry","CharlesRiver","Nitrogen","Rooms",
                      "Age","Distance","Highways","Tax","Teacher","Black","Lower","Median") 
#10B = 10C
taxPlot <- ggplot(data = Boston, aes(x = Tax, y = Crime, color = Lower))
  # Areas with lower taxation, have lower crime rates. This might because these areas have the lowest
  # concenrations of lower income people 

#10D
distanceplot <- ggplot( data = Boston, aes(x = Tax, y = Distance, color = Teacher))                       
distanceplot + geom_point()  

  # The darker blues are where the teacher-student ratio is best. The closer students are to 
  # the city, the better the ration. The further away, the worst
  
#10E
Charles <- Boston[Boston$CharlesRiver == 1,]      

attach(Boston)

#10F
median(Teacher)

#10G
#Which suburb has the lowest median value of owner occupied homes
# What are the values of this suburb
#How does this compare to the predictors overall? 
?Boston
min(Median)
Boston[399,]
summary(Boston)
#This area seems to have more industry. While there are fewer teachers per student, there may be more 
# job opprountities to offset the issues. 

Seveth <- Boston[Rooms > 7,]
Eighth <- Boston[Rooms > 8,]

summary(Seveth)
summary(Eighth)
summary(Boston)
# There appears to be more crime in the areas that have dwellings with more room. 
#There also appears to be less industry as well. The taxes seem to be lower though