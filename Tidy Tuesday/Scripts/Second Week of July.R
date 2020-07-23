library(tidyverse)

df <- readr:: read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')



ACT <- df %>% group_by(df$ACT) %>%  count(,outcome, outcome) %>% data.frame()
NSW <- df %>% group_by(df$NSW) %>%  count(,outcome, outcome) %>% data.frame()
NT <- df %>% group_by(df$NT) %>%  count(,outcome, outcome) %>% data.frame()
QLD <- df %>%group_by(df$QLD) %>% count(,outcome, outcome) %>% data.frame()
SA <- df %>% group_by(df$SA) %>% count(,outcome, outcome) %>% data.frame()
TAS <- df %>% group_by(df$TAS) %>% count(,outcome, outcome) %>% data.frame()
VIC <- df %>% group_by(df$VIC) %>% count(,outcome, outcome) %>% data.frame()
WA <- df %>% group_by(df$WA) %>% count(,outcome, outcome) %>% data.frame()


rate <- function(x)
{  
  home_found <- x %>% filter(x$outcome == "Rehomed") %>% count(,outcome)
  home_found <- home_found[2]
  home_not_found <- x %>% filter(x$outcome != "Rehomed") %>% select(n) %>% sum()
  home_not_found <- home_not_found[1]
  percent <- home_found / (home_found+ home_not_found)
  percent <- percent* 100
  return (percent)
}



ACT_val  <- rate(ACT)
NSW_val <- rate(NSW)
NT_Val <- rate(NT)
QLD_val <- rate(QLD)
SA_Val <- rate(SA)
TAS_Val <- rate(TAS)
VIC_VAL <- rate(VIC)
WA_Val <- rate(WA)


a <- rbind("ACT",ACT_val) 
b <- rbind("NSW",NSW_val) 
c <- rbind("NT", NT_Val)
d <- rbind("QLD",QLD_val)
e <- rbind("SA", SA_Val)
f <- rbind("TAS", TAS_Val)
g <- rbind("VIC", VIC_VAL)
h <- rbind("WA", WA_Val)

df <- cbind(a,b,c,d,e,f,g,h) %>% data.frame()
colnames(df) <- df[1,]
df <- df %>% t()
colnames(df) <- c("States", "Percent_Found")
df <- df %>% data.frame()

head(df)

a <- ggplot(df, aes(x = df$States, y = df$Percent_Found)) + geom_point( aes(color = States))+
  ggtitle("Percentage of Animals Found Homes in Australia") + xlab("States") + ylab("Percent of Animals Found a Home")
a
