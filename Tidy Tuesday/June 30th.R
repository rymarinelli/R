library(magrittr)
library(dplyr)

comic_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/comic_bechdel.csv')
df <- comic_bechdel
df_1 <- df %>% group_by(series) %>% count(,pass_bechdel) %>% data_frame()

pass <- dplyr::filter(df_1, pass_bechdel == "yes") %>% data_frame()
fail <- dplyr::filter(df_1, pass_bechdel == "no") %>% data_frame()

df_2 <- full_join(pass,fail, by = "series") %>% data_frame()
df_3 <- df_2 %>% mutate( pass_rate = n.x/(n.x+n.y))

library(ggplot2)


a <- ggplot(df_3, aes(x = n.x, y = n.y )) +
geom_point(aes(color = series, size = pass_rate), alpha = 0.5) 

b <-  a + ggtitle("Pass Percentage By Series") + xlab("Number of Series that Passed Bechdel") +
ylab("Number of Series that Passed Bechdel") 

ggsave("Bechdel.pdf")
