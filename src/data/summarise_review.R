library(tidytext)
library(tidyverse)
library(SnowballC)

#us_restaurant_review <- read.csv("../../data/processed/us_restaurant_reviews_10.csv", row.names = NULL)
us_restaurant_review <- read.csv("../../data/processed/us_restaurant_reviews_50.csv", row.names = NULL)

corpus <- us_restaurant_review %>%
  select(business_id, review_id, text, stars, date, useful) %>%
  separate(date, into = c("year", "month", "day")) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    text = as.character(text),
    business_id = as.character(business_id),
    review_id = as.character(review_id)
  ) 

## clean text

corpus_token_unstem <- corpus %>%
  ungroup() %>%
  unnest_tokens(word, text) 


# get total number of reviews by business id

review_num <- corpus %>%
  group_by(business_id) %>%
  summarise(sumBID = n())

# get average number of words of review by business id

review_word <- corpus_token_unstem %>%
  group_by(business_id, review_id) %>%
  summarise(sumwords = n()) %>%
  group_by(business_id) %>%
  summarise(meannum = mean(sumwords)) %>%
  mutate(meannum = round(meannum, 2))



# get sentiment

corpus_token <- corpus_token_unstem %>%  
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))

bing <- get_sentiments("bing")

review_sentiment <- corpus_token %>%
  group_by(business_id, review_id) %>%
  inner_join(bing) %>%
  count(review_id, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(positive = ifelse(is.na(positive), 0, as.numeric(positive)),
         negative = ifelse(is.na(negative), 0, as.numeric(negative))) %>%
  mutate(sentiment = positive - negative)

review_sentiment2 <- review_sentiment %>%
  group_by(business_id) %>%
  summarise(meansentiment = mean(sentiment)) %>%
  mutate(meansentiment = round(meansentiment, 2))

# combine as data frame
review_summary <- inner_join(review_sentiment2, review_num) 
review_summary <- inner_join(review_summary, review_word) 

save(review_summary, file = "../../data/processed/review_summary.RData")