# Scrape metacritic for game reviews
library(tidyverse)
library(rvest)
library(foreach)

# Base URL
url <- "https://www.metacritic.com/browse/games/score/metascore/all?page=%i"

# Max pages
pages <- read_html(sprintf(url, 0)) %>%
  html_nodes(".page_num") %>%
  html_attr("href") %>%
  str_extract(., "[0-9]+") %>%
  as.numeric() %>%
  max(na.rm = TRUE)

# Index stats at 0
pages <- pages - 1

# Crawler function
scrape_critic <- function(url = url, pages = pages, .verbose = TRUE, .errorhandling = "remove") {
  data <- tibble()
  
  foreach(i = 0:pages) %do% {
    
    html <- read_html(sprintf(url, i))
    
    rank <- html %>%
      html_nodes(".title.numbered") %>%
      html_text2() %>%
      as.numeric()
    
    game <- html %>%
      html_nodes(".title") %>%
      html_nodes("h3") %>%
      html_text()
    
    platform <- html %>%
      html_nodes(".clamp-details") %>%
      html_node(".platform") %>%
      html_node(".data") %>%
      html_text2()
    
    released_on <-  html %>%
      html_nodes(".clamp-details") %>%
      html_nodes("span") %>%
      html_text() %>%
      .[!str_detect(., "\n")] %>%
      .[!str_detect(., "Platform:")]
    
    text_summary <- html %>%
      html_nodes(".summary") %>%
      html_text2()
    
    meta_score <- html %>%
      html_nodes(".clamp-metascore") %>%
      html_text2() %>%
      str_extract(., "[0-9]+") %>%
      as.numeric()
    
    user_score <- html %>%
      html_nodes(".clamp-userscore") %>%
      html_text2() %>%
      str_extract(., "[0-9]+") %>%
      as.numeric()
    
    tmp <- tibble(game, platform, released_on, text_summary, meta_score, user_score, rank)
    data <- bind_rows(data, tmp)
  }
  data
}

# Scraped Data
df <- scrape_critic(url = url, pages = pages)

# Write Data
write_rds(df, paste0("./data/MetaScore_", Sys.Date(),".RDS"))










