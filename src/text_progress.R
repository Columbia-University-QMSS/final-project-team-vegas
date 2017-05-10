---
title: "Textual/Sentiment Analysis"
output:
   html_document:
    theme: united
    highlight: tango
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false

---

```{r include=FALSE, echo = FALSE}
# load necceary packages
packages <- c("topicmodels", "MASS", "RTextTools", "stringr", "ggplot2", "tm", "qdap", "tidytext", 
              "dplyr", "tidyr", "ggthemes", "knitr")
pakcages <- lapply(packages, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

library(plotly)
library(dplyr)
data <- read.csv("C:/Users/Veronica/Documents/GitHub/final-project-team-vegas/data/processed/LV_bis.csv")

#review <- read.csv('C:/Users/Veronica/Downloads/data_r.csv', fileEncoding = "latin1")
review <- read.csv('C:/Users/Veronica/Downloads/data_r.csv')

# exclude non-ASCII texts
review$text <- gsub("[^\x20-\x7E]", "", review$text)

# merge attribute data frame and review text file
total <- merge(data, review, by = "business_id", all = FALSE)

# only keep unique texts (delete duplicated ones)
total_u <- distinct(total, text, .keep_all = TRUE)

# choose only downtown and The Strip
total_s <- total_u %>% filter(neighborhood == "Downtown" | neighborhood == "The Strip")

##### clean text #####
total_s$text <- as.character(total_s$text)
str(total_s$text)
text <- as.data.frame(total_s$text)

df_source <- DataframeSource(text[1])
df_corpus <- VCorpus(df_source)
df_corpus

# check the contents
df_corpus[[1]][1]
df_corpus[[13312]][1]

# Text cleaning
df_corpus <- tm_map(df_corpus, content_transformer(tolower))
df_corpus <- tm_map(df_corpus, content_transformer(removeWords), c(stopwords("english")))
# df_corpus <- tm_map(df_corpus, content_transformer(removeWords), c("list", "w/"))
df_corpus <- tm_map(df_corpus, content_transformer(removeNumbers))
df_corpus <- tm_map(df_corpus, content_transformer(removePunctuation))
# df_corpus <- tm_map(df_corpus, content_transformer(replace_abbreviation))
# df_corpus <- tm_map(df_corpus, replace_symbol)
# delete non-english characters

## stem document
df_corpus_stem <- tm_map(df_corpus, stemDocument)
df_corpus_stem <- tm_map(df_corpus_stem, stripWhitespace)

# check stemmed document
#df_corpus_stem
#df_corpus_stem[[1]][1]
#df_corpus_stem[[2]][1]

# givr row names to data frame to match neighbhorhood in tdm and dtm
total_s$row.names <- 1:nrow(total_s)

# dtm and tdm
tdm <-TermDocumentMatrix(df_corpus_stem)  

tdm_td <- tidy(tdm)
tdm_td$neighborhood <- total_s[match(tdm_td[['document']], total_s[['row.names']]), 'neighborhood']
head(tdm_td)

dtm <- DocumentTermMatrix(df_corpus_stem)

dtm_td <- tidy(dtm)
head(dtm_td)
dtm_td$neighborhood <- total_s[match(dtm_td[['document']], total_s[['row.names']]), 'neighborhood']
head(dtm_td)

tf_idf <- tdm_td %>%
  bind_tf_idf(term, document, count)  %>%
  arrange(desc(tf_idf))

```

```{r include=FALSE, echo = FALSE}
# Downtown
#### by tf_idf
tf_idf %>% filter(neighborhood=="The Strip") %>% 
  top_n(n = 10, wt = tf_idf)  %>%
  ggplot(aes(x = reorder(term, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  xlab(NULL) + ylab("TF-IDF") + coord_flip() + theme_tufte() + 
  ggtitle("The Frequent Words Used in The Strip") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

```


```{r include=FALSE, echo = FALSE}
#### by tf_idf
# The Strip
tf_idf %>% filter(neighborhood=="Downtown") %>% 
  arrange(desc(tf_idf)) %>%
  top_n(n = 10, wt = tf_idf)  %>%
  ggplot(aes(x = reorder(term, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  xlab(NULL) + ylab("TF-IDF") + coord_flip() + theme_tufte() + 
  ggtitle("The Frequent Words Used in Downtown") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

```

```{r include=FALSE, echo = FALSE}
# wordcloud by neighborhood
library(wordcloud)
# Create purple_orange
purple_orange <- brewer.pal(10, "PuOr")
# Drop 2 faintest colors
purple_orange <- purple_orange[-(1:2)]

## The Strip
term_frequency_s <- tf_idf %>% filter(neighborhood=="The Strip")
set.seed(100)

# Create a wordcloud for the review in The Strip
wordcloud(term_frequency_s$term, term_frequency_s$tf, 
          max.words = 200, 
          colors= purple_orange)


### includes a word "overpriced"
```

```{r include=FALSE, echo = FALSE}
## Downtown
term_frequency_DT <- tf_idf %>% filter(neighborhood=="Downtown")
set.seed(213)

# Create a wordcloud for the review in Downtown
wordcloud(term_frequency_DT$term, term_frequency_DT$tf, 
          max.words = 200, colors = purple_orange)

#### includes a word "cheap"
```

```{r include=FALSE, echo = FALSE}
pos <- read.table("C:/Users/Veronica/Documents/GitHub/QMSS-G4063-Data-Visualization/Assignments/Assignment 3/dictionaries/positive-words.txt", as.is=T)
neg <- read.table("C:/Users/Veronica/Documents/GitHub/QMSS-G4063-Data-Visualization/Assignments/Assignment 3/dictionaries/negative-words.txt", as.is=T)

library(quanteda)

sentiment <- function(words=c("really great good stuff bad")){ 
  n = length(words)
  out = rep(NA_real_, n)
  for (i in 1:n){
    tok <- quanteda::tokenize(words[i]) 
    pos.count <- sum(tok[[1]]%in%pos[,1]) 
    neg.count <- sum(tok[[1]]%in%neg[,1]) 
    out[i] <- (pos.count - neg.count)/(pos.count+neg.count) 
  }
  return(out)
}


Strip <- total_s %>% filter(neighborhood == "The Strip")
Downtown <- total_s %>% filter(neighborhood == "Downtown")

Sent_s <- data.frame(matrix(0, ncol = 1, nrow = nrow(Strip)))
colnames(Sent_s)[1] <- "sent"
Sent_s$text <- Strip$text
Sent_s$star <- Strip$starsstrip

Sent_d <- data.frame(matrix(0, ncol = 1, nrow = nrow(Downtown)))
Sent_d$text <- Downtown$text
colnames(Sent_d)[1] <- "sent"
Sent_d$star <- Downtown$stars

#for(i in 1:n) {
#  Sent_s[[i, 1]] <- sentiment(Strip[[i, 495]])
#}

#for(i in 1:nrow(Downtown)) {
#  Sent_d[[i,1]] <- sentiment(Downtown[[i, 495]])
#}

sentiment(Strip[[7, 495]])
sentiment(Downtown$text)

Sent_d$neigh <- "Downtown"
Sent_d$neigh <- factor(Sent_d$neigh)
Sent_s$neigh <- "The Strip"
Sent_s$neigh <- factor(Sent_s$neigh)

Neighborhood <- rbind(Sent_d, Sent_s)
library(plyr)

Neighborhood$rate <- "low"

Neighborhood$rate <- ifelse(Neighborhood$star < 2.5, "low", 
        ifelse(Neighborhood$star < 4,  "mid",  "high"))
table(Neighborhood$rate)
Neighborhood$rate <- factor(Neighborhood$rate, levels = c('low', 'mid', 'high'))
str(Neighborhood$rate)

library(plyr)

```


```{r include=FALSE, echo = FALSE}
p <- ggplot(Neighborhood, aes(x = neigh, y = sent, fill = neigh)) + geom_boxplot()
p <- p + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Sentiment Score") + 
  ggtitle("Sentiment Score of Reviews in Each Neighbhorhood") +
  labs(fill = "Neighbhoorhood") + coord_flip() +
  theme_tufte() 
  #theme(axis.text.y=element_blank())

ggplotly(p)


```


```{r include=FALSE, echo = FALSE}

labels <- c(high = "High [4, 5]", mid = "Mid [2.5, 3.5]", low = "Low [1, 2]")

p2 <- ggplot(Neighborhood, aes(x = neigh, y = sent, fill = neigh)) + geom_boxplot() + 
  theme_tufte() +
  #theme(axis.text.x=element_blank()) + 
  labs(fill = "Neighbhoorhood") + 
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Sentiment Score") +
  ggtitle("Sentiment Score of Reviews of Each Neighbhorhood by Rating")

p2 <- p2  + facet_wrap(~rate, labeller=labeller(rate = labels))

ggplotly(p2)

````
