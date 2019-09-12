library(readr)
 stats <- P2_Demographic_Data <- read_csv("Learning R/P2-Demographic-Data.csv")
View(P2_Demographic_Data)
rm(P2_Demographic_Data)
getwd()

nrow(stats)
ncol(stats)
head(stats, n = 20)
tail(stat)
str(stats)
summary(stats)

# Using the dollar sign 
stats$`Internet users`[2]

stats[1:10,]
stats[c(4,100),]
is.data.frame(stats[,1])

stats$MyCalc <- 1:5
stats$MyCalc <- NULL

#Filtering Data
filter <- stats$Internet.users < 2
stats[filter,]
stats

stats[stats$Birth.rate > & stats&Internet.user 40,]
stats [stats$Income.Group == "High income",]
stats
levels(statsIncome.Group)

stats[stats$Country.Name == "Malta", ]
