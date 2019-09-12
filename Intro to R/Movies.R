movie <- P2_Movie_Ratings

colnames(movie) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
rownames(movie)
head(movie)
tail(movie)
str(movie)
summary(movie)

factor(movie$Year)
movie$Year <- factor(movie$Year)
summary(movie)

library(ggplot2)

P <- ggplot(data = movie, aes(x= CriticRating, y = AudienceRating, 
                         color = Genre, size = BudgetMillions)) +
  
P + geom_line() + geom_point() 


q <- ggplot(data = movie, aes(x= CriticRating, y = AudienceRating, 
                              color = Genre, size = BudgetMillions))

q + geom_point(aes(size = CriticRating))

q + geom_point(aes(colour = BudgetMillions))

q + geom_point(aes(x = BudgetMillions))+
  xlab("Budget in Millions")

q + geom_line(size = 1) + geom_point()

r <- ggplot(data= movie, aes(x = CriticRating, y = AudienceRating))
r+ geom_point()
r + geom_point(color = "DarkGreen")

r + geom_point(aes(size = BudgetMillions))
r + geom_point(size = 1)

s <- ggplot(data = movie, aes(x = BudgetMillions))
s + geom_histogram(binwidth = 10)

s + geom_histogram(binwidth = 10, aes(fill = Genre), color = "Black")

s + geom_density(aes(fill = Genre), position = "stack")

t <- ggplot(data = movie, aes(x = CriticRating))
t + geom_histogram(binwidth = 10,
                   fill = "white", colour = "Blue")

u <- ggplot(data = movie, aes(x = CriticRating, y = AudienceRating, color = Genre)
u + geom_point() + geom_smooth(fill = NA)

u <- ggplot(data = movie, aes(x = Genre, y = AudienceRating, color = Genre))      
u + geom_boxplot()
u + boxplot(size = 1.2)+ geom_point()
u + geom_boxplot(size = 1.2) + geom_jitter()
u + geom_jitter() + geom_boxplot(size = 1.2, alpha = .5)


v <- ggplot(data= movie, aes(x = BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill = Genre), color = "black")+
  
  facet_grid(Genre ~., scale = "free")

w <- ggplot(data = movie, aes (x = CriticRating, y = AudienceRating, color = Genre)+
  w + geom_point(size = 3) +
  facet_grid(Genre ~ Year) +
    geom_smooth()
  
# w + geom_point(size = BudgetMillions)) +
 # geom_smooth()+  
  #facet_grid(Genre ~ Year) 
  
m <- ggplot(data = movie, aes(x = CriticRating, y = AudienceRating,
                              size = BudgetMillions,
                              color = Genre)) 
m + geom_point()
             
m + geom_point() +
  xlim(50,100)
  ylim(50,100)
  
n <- ggplot(data = movie, aes(x = BudgetMillions))
n + geom_histogram(binwidth = 10, aes( fill = Genre), color = "Black")

n + geom_histogram(binwidth = 10, aes(fill = Genre, color = "Black") +
                     ylim(0,50)
                   
n + geom_histogram(binwidth = 10, aes( fill = Genre), color = "Black") +
    coord_cartesian(ylim = c(0,50))

w <- ggplot(data = movie, aes (x = CriticRating, y = AudienceRating, color = Genre)+
              w + geom_point(size = 3) +
              facet_grid(Genre ~ Year) +
              geom_smooth() +
              coord_cartesian(ylim = c(0,100))
            
o <- ggplot(data = movie, aes(x = BudgetMillions))
h <- o + geom_histogram( binwidth = 10, aes( fill = Genre), color = "Black")
h + 
    xlab("Money Axis")+
    ylab("Number of Movies") +
    ggtitle("Movie Budget Distribution")+ 
  
    theme(axis.title.x = element_text(color = "Dark Green", size = 30),
           axis.title.y = element_text(color = "Red", size = 30),
           axis.text.x = element_text(size = 20),
           axis.text.y = element_text(size = 20),
           legend.title = element_text ( size = 30),
           legend.text = element_text(size = 20),
           legend.position = c(1,1),
           legend.justification = c(1,1),
           plot.title = element_text(color = "Dark Blue", size = 40, family = "Courier"))
           
           
          
