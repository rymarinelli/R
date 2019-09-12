MyFirstVector <- c(3,45,56,732)
MySecondVector<- c(3L,45L,56L,732L)
is.numeric(MyFirstVector)
is.integer(MySecondVector)
is.double(MyFirstVector)

seq(1,15)
1:15

seq(1,15,2)

w <- c("a","b", "c","d","e")

w[c(-1,-5)]

l <- c("A","B","C")
n <- c(1,2,3)

t <- c(l,n)
t

x <- rnorm(5)
x

for( i in x){
  print (i)
}

for (j in 1:5){
  print(x[j])
}

## Comparing vectorized and de-vectorized approaches 
N <- 100000000 
a <- rnorm(N)
b <- rnorm(N)

# vectorized
c <- a * b 
c

#devectorized
d <- rep(NA,N)


for(i in 1:N){
  d[i] <- a[i] * b[i]
  
}
  
d

seq(from = 10, to = 20, length.out = 50)
  
  
  
  
  