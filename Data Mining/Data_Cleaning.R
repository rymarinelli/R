library(tidyverse)
library(stringr)

movies_metadata <- read_csv("C:/Users/rmarinelli4/Downloads/movies_metadata.csv")
meta_data <- movies_metadata %>% select(budget,genres,id,original_title,popularity)
meta_data$genres


stringr::str_extract_all(meta_data$genres, "\\w+(?='\\})")
values <- data.frame()
colnames(values) <- c("Genre","Movie_ID")

i = 0


cleaner <- function()
{
  for(i in 1:length(meta_data$genres))
  {
    tryCatch({
    
      a <- stringr::str_extract_all(meta_data$genres, "\\w+(?='\\})")[i] %>% unlist()
      b <- meta_data$id[i]
      d <- cbind(a,b) %>% data.frame()
      colnames(d) <- NULL
      colnames(d) <- c("Genre","Movie_ID")
      .GlobalEnv$values <- rbind(values,d) 
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }
}

library(snow)
cl <- snow::makeCluster(11)
stopCluster(cl)
