############################
# Yelp Project
# Text Exploratory Analysis
# 03/04/2016
############################

library(tidyverse)
library(tm)
library(Matrix)
library(wordcloud)

## retrieve data 
load("../../data/processed/cpdtm.RData")
load("../../data/processed/cpdtm_unstem.RData")

## find frequent terms (stemmed)
# by threshold
findFreqTerms(cpdtm, lowfreq = 100000)

# by top x number of terms
freq <- sort(colSums(cpdtm_matrix), decreasing = TRUE)
head(freq, 20)
wf <- data.frame(word = names(freq), freq = freq)
subset(wf, freq > 100000) %>%
  ggplot(aes(word, freq)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## find no. of unique words in each review (stemmed)
summary(rowSums(cpdtm_matrix))

## find associated terms with "word"
findAssocs(cpdtm, "chicken", corlimit = 0.3)

## create wordcloud
# wordcloud for the first review (unstemmed)
wordcloud(colnames(cpdtm_unstem_matrix), cpdtm_unstem_matrix[1, ], min.freq = 1, max.words = 20, random.color = T, ordered.colors = T)

# wordcloud for the most frequent words (unstemmed)
freq_unstem <- sort(colSums(cpdtm_unstem_matrix), decreasing = TRUE)
freq50000 <- freq_unstem[freq_unstem > 50000]
col = rep("#E6AB02", length(freq50000))
col[1:10] = "#1B9E77"
wordcloud(names(freq50000), freq50000, colors = col, ordered.colors = T, max.words = 50)
