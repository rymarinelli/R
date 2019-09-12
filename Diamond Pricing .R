mydata <- read.csv(file.choose())

library(ggplot)

ggplot(data = mydata[mydata$carat< 2.5,], aes(x = carat, y = price , colour = clarity )) +
   geom_point(alpha = .1) +
   geom_smooth()
