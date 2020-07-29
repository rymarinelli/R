library(tidyverse)
library(factoextra)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')
df <- df %>% na.omit()
df <- df %>% mutate(gender = case_when(sex == "male" ~ "0", T ~ "1"))
df$island %>% unique()

df <- df %>% mutate(location = case_when(
                                    island == "Torgersen" ~ "0",
                                    island == "Biscoe" ~ "1",
                                    T ~ "2"))


names <- df$species

df$species <- NULL
df$island <- NULL
df$sex <- NULL
df$year <- NULL

df[,1:4] %>% scale()
df <- df %>% as.matrix()
df[,1:6] %>% as.integer()
rownames(df) <- names

df <- data.frame(df)
df$bill_length_mm <- df$bill_length_mm %>% as.numeric()
df$bill_depth_mm <- df$bill_depth_mm  %>% as.numeric()
df$body_mass_g  <- df$body_mass_g  %>% as.numeric()
df$flipper_length_mm <- df$flipper_length_mm %>% as.numeric()
df$gender <- df$gender  %>% as.integer()
df$location <- df$location %>% as.integer()

k3 <- kmeans(df, centers = 3, nstart = 25)
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("Likeness of Penguins")

p2 + scale_fill_discrete(name = "Species Designation", labels = c("Adelie","Gentoo","Chinstrap"))
