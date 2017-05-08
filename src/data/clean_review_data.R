###################
# Yelp Project
# Text Cleaning
# 03/04/2016
###################

require(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(Matrix)

library(SnowballC)
library(tm)




## read dataset

dataset <- read.csv("../../data/processed/us_restaurant_reviews_50.csv", row.names = NULL)

#dataset <- read.csv("../../data/processed/us_restaurant_reviews_10.csv", row.names = NULL)
rests <- read.csv("../../data/processed/us_restaurants.csv", row.names = NULL)
rests <- rests[, c("business_id", "city")]

# add city to reviews dataset
dataset <- merge(dataset, rests, by = "business_id")

## For training set, take a sample of 5000 reviews per city so the topic modeling is balanced
# group by city
by_city <- dataset %>%
  group_by(city, stars)

# take a random sample of 5000 reviews per city
set.seed(1234)
rand_rev <- sample_n(by_city, 5000, replace = TRUE)$review_id
rand_rev <- as.data.frame(rand_rev)
colnames(rand_rev) <- "review_id"

# filter training set to only include 5000 reviews per city
dataset_training <- merge(dataset, rand_rev, by = "review_id")

#dataset_training <- dataset %>%
  #filter(review_id %in% rand_rev)

########################################
# NOTE, might also be worth repeating the above but stratifying by rating of review, so that we make sure
# the training corpus has an even number of positive and negative reviews
########################################

## tidy corpus

corpus_training <- dataset_training %>%
  select(business_id, review_id, text, stars, date, useful) %>%
  separate(date, into = c("year", "month", "day")) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    text = as.character(text),
    business_id = as.character(business_id),
    review_id = as.character(review_id)
  ) %>%
  unite(biz_review_id, business_id, review_id, sep = "/")

corpus <- dataset %>%
  select(business_id, review_id, text, stars, date, useful) %>%
  separate(date, into = c("year", "month", "day")) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    text = as.character(text),
    business_id = as.character(business_id),
    review_id = as.character(review_id)
  ) %>%
  unite(biz_review_id, business_id, review_id, sep = "/")


## clean text
data("stop_words")

#corpus_token_unstem <- corpus %>%
# ungroup() %>%
#  unnest_tokens(word, text) %>%   # tokenize, remove punctuation, lowercase
# anti_join(stop_words)           # remove stopwords

corpus_token_unstem_bigrams_train <- corpus_training %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%   # tokenize, remove punctuation, lowercase
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word,
         !next_word %in% stop_words$word,
         str_detect(word, "[a-z]"),
         str_detect(next_word, "[a-z]")
  ) %>%
  unite(bigram, word, next_word, sep = ' ')

corpus_token_unstem_bigrams <- corpus %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%   # tokenize, remove punctuation, lowercase
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word,
         !next_word %in% stop_words$word,
         str_detect(word, "[a-z]"),
         str_detect(next_word, "[a-z]")
  ) %>%
  unite(bigram, word, next_word, sep = ' ')

#corpus_token <- corpus_token_unstem %>%  
# mutate(word = wordStem(word))   # stem words

## count words

word_counts <- corpus_token_unstem %>%
  count(word, sort = TRUE)

word_counts_bi <- corpus_token_unstem_bigrams_train %>%
  count(bigram, sort = TRUE)


## create full document term matrix (stemmed)

# cp <- corpus_token[, c("biz_review_id", "word")]
# cp_count <- count_(cp, c("biz_review_id", "word"))
# cpdtm <- cp_count %>%
#   cast_dtm(biz_review_id, word, n)
# cpdtm_matrix <- sparseMatrix(cpdtm$i, cpdtm$j, x = cpdtm$v,
#                              dims = c(cpdtm$nrow, cpdtm$ncol),
#                              dimnames = cpdtm$dimnames)


## create full document term matrix (unstemmed)

# cp_unstem <- corpus_token_unstem[, c("biz_review_id", "word")]
# cp_unstem_count <- count_(cp_unstem, c("biz_review_id", "word"))
# cpdtm_unstem <- cp_unstem_count %>%
# cast_dtm(biz_review_id, word, n)
# cpdtm_unstem_matrix <- sparseMatrix(cpdtm_unstem$i, cpdtm_unstem$j, x = cpdtm_unstem$v,
# dims = c(cpdtm_unstem$nrow, cpdtm_unstem$ncol),
# dimnames = cpdtm_unstem$dimnames)



## BIGRAMS - create full document term matrix (unstemmed) 
# used by topic_modelling.R

# training
cp_unstem_bi_train <- corpus_token_unstem_bigrams_train[, c("biz_review_id", "bigram")]
cp_unstem_count_train <- count_(cp_unstem_bi_train, c("biz_review_id", "bigram"))
cpdtm_unstem_bi_train <- cp_unstem_count_train %>%
  cast_dtm(biz_review_id, bigram, n)
cpdtm_unstem_bi_matrix_train <- sparseMatrix(cpdtm_unstem_bi_train$i, cpdtm_unstem_bi_train$j, x = cpdtm_unstem_bi_train$v,
                                             dims = c(cpdtm_unstem_bi_train$nrow, cpdtm_unstem_bi_train$ncol),
                                             dimnames = cpdtm_unstem_bi_train$dimnames)

# full
cp_unstem_bi <- corpus_token_unstem_bigrams[, c("biz_review_id", "bigram")]
cp_unstem_count <- count_(cp_unstem_bi, c("biz_review_id", "bigram"))
cpdtm_unstem_bi <- cp_unstem_count %>%
  cast_dtm(biz_review_id, bigram, n)
cpdtm_unstem_bi_matrix <- sparseMatrix(cpdtm_unstem_bi$i, cpdtm_unstem_bi$j, x = cpdtm_unstem_bi$v,
                                       dims = c(cpdtm_unstem_bi$nrow, cpdtm_unstem_bi$ncol),
                                       dimnames = cpdtm_unstem_bi$dimnames)

## save document term matrix as R object
# retrieve data with load("../../data/processed/cpdtm.RData")

#save(cpdtm, cpdtm_matrix, file = "../../data/processed/cpdtm.RData")
#save(cpdtm_unstem, cpdtm_unstem_matrix, file = "../../data/processed/cpdtm_unstem.RData")
save(cpdtm_unstem_bi_train, cpdtm_unstem_bi_matrix_train, file = "../../data/processed/cpdtm_unstem_bi_train.RData")
save(cpdtm_unstem_bi, cpdtm_unstem_bi_matrix, file = "../../data/processed/cpdtm_unstem_bi.RData")
