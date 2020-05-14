library(tidyverse)
library(ggplot2)
library(fmsb)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
df <- food_consumption

df <- df %>% group_by(food_category) %>% summarise("Total Carbon Footprint" = sum(co2_emmission))


min <- df$`Total Carbon Footprint` %>% min()
max <- df$`Total Carbon Footprint` %>% max()
min <- rep(min, times = nrow(df))
max <- rep(max,times = nrow(df))

data <- rbind(max,min,df$`Total Carbon Footprint`)
rownames(data) <- NULL
names <- df$food_category 
names[4] = "Lamb"
names[5] = "Dairy"
names[11] = "Wheat"
names[6] = "Nuts"
names

colnames(data) <- names
data <- data %>% data.frame()

#test <- radarchart(data, pfcol = 11, cglty = 7)

colors_border= rgb(0,.5,0)
colors_in = rgb(0,.7,0, .4)

test <- radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd= , plty=1,
            #custom the grid
            cglcol="black", cglty= 1, axislabcol="white", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8, title = "Carbon Intensive Consumption", centerzero = F)

par(bg = 200)
