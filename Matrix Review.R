# Matrix and Named Vectors

Charlie <- 1:5
Charlie

#give names 
names(Charlie) <- c("a","b","c","d",'e')
Charlie
Charlie["d"]

#Clear names 
names(Charlie) <- NULL
Charlie

temp.vec <- c('a','b','zZ')

rep(temp.vec, each = 3)

Bravo <- matrix(temp.vec, 3,3)
Bravo
colnames(Bravo) <- c("X","Y","Z")
Bravo

rownames(Bravo) <- c("how",'are','you')
Bravo['are','Y'] <- 0
Bravo
