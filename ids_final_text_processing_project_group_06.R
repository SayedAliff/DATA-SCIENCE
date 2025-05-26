library(rvest)
library(dplyr)
library(tm)
library(SnowballC)


categories <- c("bangladesh", "international", "sports", "opinion", "business")
base_url = read_html("https://en.prothomalo.com/") 


all_news <- data.frame()


for (cat in categories) {
  cat_news <- data.frame()
  
  for (page_num in 1:100) {
    cat_url <- paste0(base_url, "/", cat, "?page=", page_num)
    
    page <- try(read_html(cat_url), silent = TRUE)
    if (inherits(page, "try-error")) next
    

    links <- page %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      unique()
    

    links <- links[grepl(paste0("/", cat, "/"), links)]
    links <- ifelse(grepl("^http", links), links, paste0(base_url, links))
    
    for (link in links) {
      if (nrow(cat_news) >= 100) break
      try({
        article <- read_html(link)
        
        title <- article %>%
          html_node("h1") %>%
          html_text(trim = TRUE)
        
        description <- article %>%
          html_nodes("p") %>%
          html_text() %>%
          paste(collapse = " ")
        
        date <- article %>%
          html_node("time") %>%
          html_attr("datetime")
        
        news_row <- data.frame(
          title = title,
          description = description,
          date = date,
          category = cat,
          stringsAsFactors = FALSE
        )
        
        cat_news <- bind_rows(cat_news, news_row)
      })
    }
    if (nrow(cat_news) >= 100) break
  }
  all_news <- bind_rows(all_news, cat_news)
}


head(all_news)

write.csv(all_news, "ids_final_project_group_06_news_raw.csv", row.names = FALSE)




corpus <- Corpus(VectorSource(all_news$description)) #Create a corpus from the 'description' column

corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, stemDocument) 


dtm <- DocumentTermMatrix(corpus)


word_freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(word_freq, 20)


all_news$cleaned_description <- sapply(corpus, as.character)


write.csv(all_news, "ids_final_project_group_06_news_clean.csv", row.names = FALSE)

cat("leaned news saved to prothomalo_news_cleaned.csv\n")
