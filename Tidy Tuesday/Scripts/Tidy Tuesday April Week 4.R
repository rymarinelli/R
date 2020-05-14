library(readr)
library(tidyverse)
library(lubridate)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

cpi$year_month <- cpi$year_month %>% ymd()
grosses$week_ending <- grosses$week_ending %>% ymd()

cpi_months <- cpi$year_month %>% year()
cpi_year <- cpi$year_month %>%  month()
cpi_key <- paste(cpi_months,cpi_year, sep = "/")



gross_year <- grosses$week_ending %>% year() 
gross_month <- grosses$week_ending %>% month()

gross_key <- paste(gross_year, gross_month, sep = "/")
cpi <- cbind(cpi_key,cpi) %>% as.data.frame()
grosses <- cbind(grosses,gross_key) %>% data.frame()

colnames(grosses) <- c("week_ending", "week_number", "weekly_gross_overall", "show",
                       "theatre","weekly_gross","potential_gross", "avg_ticket_price",    
                       "top_ticket_price", "seats_sold", "seats_in_theatre", "pct_capacity",        
                        "performances", "previews","key")

colnames(cpi) <- c("key", "year_month", "cpi")


 df <- full_join(cpi,grosses, by = "key") %>% as.data.frame()
 df <- df %>% select(year_month,cpi,avg_ticket_price)
 df <- df %>% na.omit()
 
 final_cpi <- cpi$cpi[length(cpi$cpi)]   
 df <- df %>% mutate(real_average_price = (avg_ticket_price)*((final_cpi/cpi)))
 df$year_month <- df$year_month %>% ymd()
 years <- df$year_month %>% year()
 months <- df$year_month %>% month()
 df <- cbind(years,months, df) 
 df %>% View()
 # a <- ggplot(data=df, aes(x= years, y= real_average_price, group= years)) +
 #   geom_line(color="red")+
 #   geom_point()
 
   #ggplot(df,aes(x= real_average_price, y= years, group= years)) +
   #geom_line()+ geom_point()
 
 
 mymonths <- c("Jan","Feb","Mar",
               "Apr","May","Jun",
               "Jul","Aug","Sep",
               "Oct","Nov","Dec")
 
 df$months <- mymonths[ df$month ]
 df$months <- df$months %>% as.factor()
 
 
library(rayshader)
library(png)
 
a <-  ggplot(df,aes(x = years, y = avg_ticket_price)) +
      geom_line(color = "purple") + ggtitle("Average Ticket Price of Broadway Shows") +
      xlab("Year") + ylab("Real Average Ticket Price \n (2020 Dollars)")

# plot_gg(a, width=3.5, height_aes = df$real_average_price, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
#         zoom = 0.60, phi = 30, theta = 45)

  plot_gg(a, width=3.5, height_aes = df$real_average_price, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
         zoom = 0.60, phi = 35, theta = 45)

  render_snapshot(clear = TRUE)
  
