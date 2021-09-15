library(magrittr)
library(dplyr)
library(lubridate)
library(tidyverse)

### I want to make a Shiny plot that will show the stands per year per driver

driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')


df <- left_join(drivers,driver_standings, on = "driverId")

df$dob <- df$dob %>% ymd()

df <- df %>% mutate(Year = df$dob %>% year())

df <- df %>% group_by(surname,Year) %>% summarise(Driver_Points = sum(points))


driver_name <- df %>% mutate(zero_points = case_when(Driver_Points == 0 ~ T, TRUE ~ FALSE)) %>% subset(zero_points == F) %>% na.omit() %>% select(surname)

df <- left_join(driver_name,df, on = "surname")

df <- df %>% group_by(surname,Year) %>% summarise(Yearly_Total = sum(Driver_Points)) %>% arrange(Year)
