library(tidyverse)
library(factoextra)
library(patchwork)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
df <- coffee_ratings %>% data_frame()
df %>% colnames()
df <- df %>% select(total_cup_points, country_of_origin)

df_1 <- df %>% group_by(country_of_origin)
df_2 <- mutate(df_1, average_coffee_points = mean(total_cup_points))
df_3 <-  df_2 %>%  distinct(average_coffee_points, .keep_all = TRUE)
df_3$total_cup_points <- NULL 
df <- df_3
df <- df %>% na.omit() %>% data.frame()
df$average_coffee_points %>% round() 
df$average_coffee_points <- df$average_coffee_points %>% round()

k2 <- kmeans(df$average_coffee_points, centers = 3, nstart = 25)

df <- cbind(df,k2$cluster)
colnames(df)[3] <- "cluster"  

df <- df %>% data.frame()

df_1 <- filter(df, cluster == "1")
df_2 <- filter(df, cluster == "2")
df_3 <- filter(df, cluster == "3")

a <- ggplot(df_1, aes(x = df_1$country_of_origin, y = df_1$average_coffee_points ))
a <- a + geom_point(color = "#DEB887")+ theme(axis.text.x=element_text(angle=90 ,margin = margin(.5, unit = "cm"),vjust =1))
a <- a + xlab("Countries With Worst Coffee") + ylab("Coffee Quality Score")
a <- a + ylim(75, 90)


b <- ggplot(df_2, aes(x = df_2$country_of_origin, y = df_2$average_coffee_points, ))
b <- b + geom_point(color = "#CD853F") + theme(axis.text.x=element_text(angle=90 ,margin = margin(.5, unit = "cm"),vjust =1))
b <- b + xlab("Countries with Moderate Coffee") + ylab("")
b <- b + ylim(75, 90)

c <- ggplot(df_3,aes(x = df_3$country_of_origin, y = df_3$average_coffee_points))
c <- c + geom_point(color = "#8B4513") + theme(axis.text.x=element_text(angle=90 ,margin = margin(.5, unit = "cm"),vjust =1))
c <- c + xlab("Countries With Best Coffee") + ylab("")
c <- c + ylim(75, 90)

e <- (a | b | c) + plot_annotation(title = "Comparison of Coffee Quality")
e




