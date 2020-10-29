library(tcR)
library(tidyverse)

movies_metadata <- read_csv("C:/Users/ryanm/Desktop/Movie Data/movies_metadata.csv")
df <- movies_metadata %>% select("id","overview")
rm(movies_metadata)

genre_id <- read_csv('https://raw.githubusercontent.com/rymarinelli/R/master/Data%20Mining/MergedData.csv')
genre_id <- genre_id %>% select("Genre",'id')

df <- dplyr::inner_join(df,genre_id,"id")
rm(movies_metadata)
rm(genre_id)

df
