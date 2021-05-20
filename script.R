library(tidyverse)
library(tidytext)
library(textdata)
library(glue)
library(stringr)
library(SnowballC)
library(udpipe)
library(rvest)
library(igraph)
library(viridis)
library(ggpubr)

## URL to get the articles
url <- "https://www.bis.org/publ/qtrpdf/"

# Theme plots
p <- theme(plot.title = element_text(face = "bold", hjust = 0.5),
           plot.subtitle = element_text(hjust = 0.5),
           legend.title = element_text(size = 8, face = "bold"),
           plot.caption = element_text(face = "italic", 
                                       hjust = 1,
                                       size = 7),
           axis.text.y = element_text(size = 8,
                                      hjust = 1,
                                      vjust = 0,
                                      face = "plain"),  
           axis.title.y = element_text(size = 10,
                                       hjust = .5,
                                       vjust = 0,
                                       face = "plain"),
           axis.text.x = element_text(size = 8,
                                      hjust = 1,
                                      vjust = 0,
                                      face = "plain"),  
           axis.title.x = element_text(size = 10,
                                       hjust = .5,
                                       vjust = 0,
                                       face = "plain"))



# Sentiment Analysis Function
sentimentAnalysis <- function(words_df, lexi){
  
  total_words <- dim(words_df)[1]
  
  words_df <- words_df %>% 
    group_by(word_token) %>% 
    count(word_token) %>% 
    left_join(get_sentiments(lexi),
              by = c("word_token" = "word")) %>%
    plyr::rename(replace = c(sentiment = "sentiment",
                             value = "sentiment"),
                 warn_missing = FALSE)
  
  words_df <- words_df %>% 
    filter(!is.na(sentiment)) %>% 
    distinct(word_token, .keep_all = TRUE) %>% 
    mutate(prop = n*100/total_words, total_words = total_words)
  
  return(words_df)
}

# getting data from the website and transforming it in words 
# including the sentiment analysis given a specific lexicon. For this case
# I selected the lexicon "loughran" which is dedicated for finacial articles.
# However the function works with the other lexicons as well.

getAnalysis <- function(q, lexi){
      
      quarter_list <- list()
      for(j in 1:length(q)){
  
          p_tags <- read_html(paste0(c(url), "r_qt", q[j], "a.htm"),
                              collapse = '') %>%
                    html_nodes('p')
          
          text_list <- html_text(p_tags)
          text_list <- text_list[-c(1:2)]
          text <- tibble(text = paste(text_list, collapse = ""))
          words_df <- unnest_tokens(text, word_token, text, token = "words")
          
          words_df <- sentimentAnalysis(words_df, lexi)  %>% 
                      mutate(period = q[j])
          
          quarter_list[[j]] <- words_df
      
      }
      
      consolidated_data <- bind_rows(quarter_list, .id = "column_labels")
      
      consolidated_data
      
}


# Function for comparative Plot
sentiment_plot <- function(q, lexi){
  
  plot_df <- as.data.frame(getAnalysis(q, lexi))
  
    senti_plot <- ggplot(plot_df, aes(x = reorder(sentiment, n), y = prop)) +
    geom_bar(aes(fill =as.factor(period)),
             stat = "identity",
             position = "dodge") +
    coord_flip() +
    labs(title = paste0("Sentiment Analysis"),
         subtitle = paste0(lexi, " Lexicon"),
         x = "Sentiment",
         y = "% of Total Text",
         fill = "Period (yymm)") +
    scale_fill_viridis(discrete = TRUE,
                       direction = 1, 
                       option = "E") +
    theme_bw() + p
  
    senti_plot
}

# Table with sentiment Analysis

sumTable <- function(q, lexi){

  sum_table <- getAnalysis(q, lexi)
  
  sum_table <- sum_table %>%
                group_by(sentiment, period) %>%
                summarise(n = n()) %>%
                ungroup() %>% 
                pivot_wider(names_from = period,
                            names_prefix = "n_q",
                            values_from = n)

  
  sum_table
}


tidyTable <- getAnalysis(quarters, "loughran") %>% 
                slice(rep(1:n(), n)) %>% 
                dplyr::select(-c(n, column_labels, prop, word_token)) %>% 
                group_by(sentiment, period) %>% 
                summarise(n = n())

# write_csv(tidyTable,'question2_table.csv')

# write_csv(sumTable(quarters, "loughran"),'question2_table.csv')


quarters <- c(1912, 2012) ## the format must be yymm
#structure of the constructed dataset
head(getAnalysis(quarters, "loughran"), 20)

# summary table with frequency of words in each sentiment per period
# The function works for more than on period and for different lexicons.

sumTable(quarters, "loughran")

# plot with comparative analysis between the periods of analysis
sentiment_plot(quarters, "loughran")
# ggsave("question2_plot.png")

