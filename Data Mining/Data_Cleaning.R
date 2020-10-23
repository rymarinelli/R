library(tidyverse)
library(stringr)

movies_metadata <- read_csv("C:/Users/rmarinelli4/Downloads/movies_metadata.csv")
meta_data <- movies_metadata %>% select(budget,genres,id,original_title,popularity)
meta_data$genres


stringr::str_extract_all(meta_data$genres, "\\w+(?='\\})")
values <- data.frame()
colnames(values) <- c("Genre","Movie_ID")

i = 0


for(i in i:length(meta_data$genres))
{
    tryCatch({
      
      a <- stringr::str_extract_all(meta_data$genres, "\\w+(?='\\})")[i] %>% unlist()
      b <- meta_data$id[i]
      d <- cbind(a,b) %>% data.frame()
      colnames(d) <- NULL
      colnames(d) <- c("Genre","Movie_ID")
      .GlobalEnv$values <- rbind(values,d) 
      .GlobalEnv$i = i + 1
      print(length(meta_data$genres) - i)
      flush.console()
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
        print(a)
        print(d)
        })
    
}


values <- values %>% rename(id = Movie_ID)
values$id <- values$id %>% as.factor()
meta_data$id <- meta_data$id %>% as.factor()


df <- inner_join(values,meta_data, by = "id")

write.csv(df,"MergedData.csv")
