---
title: "Yelp Las Vegas Project"
author: "Team VL + ES"
date: "4/29/2017"
output:  html_document

---
  
  
setwd("C:/Users/Veronica/Documents/GitHub/QMSS_G4063_final_project_Veronica_Eileen/QMSS_G4063_final_project_Veronica_Eileen")
getwd()
# remove all existing lists
rm(list=ls(all=TRUE)) 
# Install packages
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scale)

packages <- c("devtools","knitr","ggplot2","ggmap","maps","readr","maptools","mapproj","rgeos","rgdal","RColorBrewer","stringr","scales","tidyverse","dplyr","readxl","statebins","RJSONIO","XML","RCurl")

packages <- lapply(packages, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Importing the data
restaurants <- read.csv("./data/lv_business_categories_matrix_v2.csv") 
#head(restaurants)



# Get Nevada's county-level shape files from U.S. census department
-------------------------------------
  
# Get Nevada's county-level shape files from U.S. census department
#install.packages("tigris")
library(tigris)

counties_NV <- counties(state="Nevada")
list_counties(state="Nevada")
head(counties_NV)
LV <- counties_NV@data[counties_NV@data$GEOID == '32003',]



#  Load a vector map of Clark boroughs
#Las Vegas is the city located within Clark County of the state of Nevada. 
#LV (city) - Clark (county) - NV (state)


library(rgdal)

#load Clark County (or what is LV's city name?) boroughs map 
LV_boroughs <- readOGR("/[CLARCK_boroughs_map]/.","[FILE NAME]nybb")
#They don't have boroughs map 

LV_boroughs <-spTransform(LV_boroughs, CRS("+proj=longlat +datum=<WGS84 -- change it to the WGS matching to LV>")) #Converting to a shape object
LV_boroughs <- fortify(LV_boroughs) #Fortify() for ggmap
#plot(nyc_boroughs)
head(nyc_boroughs, n=2)


# 1 Restaurant Count by County in LV 
restaurants_count <- restaurants %>%
  group_by(neighborhood) %>% 
  summarise(
    n = n()) %>%
  arrange(neighborhood)

# Updated to Export Restaurant
str(restaurants_count)


# selected seven restaurants 
s_restaurants_count <- restaurants %>%
  group_by(neighborhood) %>% 
  summarise(
    n = n()
  ) %>%
  filter(neighborhood %in% c("The Strip", "Downtown"))


n1 <- unlist(unique(restaurants_count[, "neighborhood"]))
 

# original graph 
ggplot(restaurants_count, aes(x = reorder(neighborhood, n), y = n)) + 
  geom_col(width = 0.7, fill = "skyblue") + 
  labs(x = "Neighborhood", y = "Number of Restaurants") + 
  theme_tufte() + 
  ggtitle("Restaurants per Neighborhood in Las Vegas") + 
  coord_flip() + 
  geom_text(aes(label = n), vjust = 0)

# Create a new "selected" Column, marks the Strip and DTLV in dummy variables (selected =1, not selected =0)
restaurants_count$selected <- ifelse(grepl("The Strip|Downtown",restaurants_count$neighborhood),1, 0)

# updated graph, highlighting selected cities in different colors, and show counts of restaurants per all cities
ggplot(restaurants_count, aes(x = reorder(neighborhood, n), y = n, fill = as.factor(selected))) + 
  geom_col(width = 0.7) + 
  labs(x = "Neighborhood", y = "Number of Restaurants") + 
  theme_tufte() + 
  ggtitle("Restaurants per Neighborhood in Las Vegas") + 
  coord_flip() + 
  geom_text(aes(label = n), vjust = 0) + theme(legend.position = "None")



# 2 List restaurants by neighborhoods in LV in different colors 
# Provide the Strip and DTLV's total count of restaurants & average star ratings out of 5 
library(RJSONIO)
library(ggmap)

# Get a Las Vegas Google map 
map_lv <- get_map("Las Vegas", zoom=12, 
                            source="google",maptype="terrain")
ggmap(map_lv)

g <- ggmap(map_lv)

g <- g + geom_point(data=restaurants, aes(x=longitude, y=latitude, color=neighborhood), size=0.4, alpha=0.4) +  
  labs(x="longitude", y="latitude") + 
  ggtitle("Overall Restaurants in Las Vegas") +
  annotate("text", x = -115.17, y =  36.12, label = "The Strip", color = "Dark Blue", fontface = 2, size = 5) +
  annotate("text", x = -115.17, y =  36.11, label = "Count:818 & Avg. Star=3.32", color = "Dark Blue", fontface = 1, size = 3) +
  annotate("text", x = -115.14 , y = 36.16, label = "Downtown", color = "Dark Blue", fontface = 2, size = 5)+ 
  annotate("text", x = -115.14, y =  36.15, label = "Count:340 & Avg. Star=3.65", color = "Dark Blue", fontface = 1, size = 3) +
  annotate("rect", xmin=-115.1790843, xmax=-115.1539853, ymin=36.08851014, ymax=36.14938091, alpha=.1, fill="blue") + 
  annotate("rect", xmin=-115.1734184, xmax=-115.0971613, ymin=36.1439997, ymax=36.17313138, alpha=.1, fill="red") + 
  theme(plot.title = element_text(size=12))

g 

#![](figure1.png)

# 3 Proportion of Star Ratings by neighborhoods 
restaurants_rating <- restaurants %>%
  group_by(neighborhood, stars) %>%
#  filter(neighborhood %in% c("The Strip", "Downtown")) %>%
  summarise(
    n = n()
  ) %>%
  spread(key = stars, value = n) %>%
  mutate(
    star1 = `1` + `1.5`,
    star2 = `2` + `2.5`,
    star3 = `3` + `3.5`,
    star4 = `4` + `4.5`,
    star5 = `5`
  ) %>%
  select(neighborhood, star1, star2, star3, star4, star5) %>%
  gather(star1, star2, star3, star4, star5, key = "star", value = "n")

# Omitting neighborhoods with misisng variables 
restaurants_rating %>% na.omit()

s_restaurants_rating <- restaurants_rating %>%
  group_by(neighborhood) %>% 
  filter(neighborhood %in% c("The Strip", "Downtown"))

s_restaurants_rating %>% na.omit()

# distribution of restaurant ratings per neighborhood 
ggplot(restaurants_rating) + 
  geom_bar(aes(x = neighborhood, y = n, fill = star), stat = "identity", position = "fill") + 
  theme_tufte() + 
  theme(axis.text.x = element_text(colour = "grey20", size = 11, angle = 45, hjust = 1, 
                                   vjust = 1, face = "italic")) + 
  ggtitle("Distribution of Restaurants Ratings per Neighborhood in Las Vegas") + 
  labs(x = "Neighborhood", y = "Proportion") 


# distribution of restaurant ratings in DTLV and the Strip

ggplot(s_restaurants_rating) + 
  geom_bar(aes(x = neighborhood, y = n, fill = star), stat = "identity", position = "fill") + 
  annotate("text", x = 'Downtown', y =  0.50, label = "Avg. Star=3.65", family="serif", fontface="italic", colour="darkred", size=4)+
  annotate("text", x = 'The Strip', y =  0.50, label = "Avg. Star=3.32", family="serif", fontface="italic", colour="darkred", size=4)+
  theme_tufte() + 
  theme(axis.text.x = element_text(colour = "grey20", size = 11, angle = 45, hjust = 1, 
                                   vjust = 1, face = "italic")) + 
  ggtitle("Distribution of Restaurants Ratings in DTLV & the Strip") + 
  labs(x = "Neighborhood", y = "Proportion") 



# 4 Review Count by City
restaurants_review_bp <- restaurants %>%
  select(neighborhood, review_count) %>%
  group_by(neighborhood) %>%
  filter(neighborhood %in% n1) %>% arrange(neighborhood)
#View(restaurants_review_bp)

s_restaurants_review_bp <- restaurants %>%
  select(neighborhood, review_count) %>%
  group_by(neighborhood) %>%
  filter(neighborhood %in% c('The Strip','Downtown')) %>% arrange(neighborhood)


#Reviews count per restaurant in each neighborhood in LV
ggplot(restaurants_review_bp, aes(x = reorder(neighborhood, as.numeric(review_count), FUN = median), 
                                  y = as.numeric(review_count))) + 
  geom_boxplot(alpha = 0.3) + 
  coord_flip() + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 50))+
  scale_y_continuous(limits = c(0,500),breaks = scales::pretty_breaks(n = 10))+
  #ylim(0,300)+
  ggtitle("Reviews Count per Restaurant in each Neighborhood in LV") + 
  labs(x = "Neighborhood", y ="Review Count") + 
  theme_tufte()

#Reviews count per restaurants in DTLV & the Strip

ggplot(s_restaurants_review_bp, aes(x = reorder(neighborhood, as.numeric(review_count), FUN = median), 
                                  y = as.numeric(review_count))) + 
  geom_boxplot(alpha = 0.3) + 
  coord_flip() + 
  scale_y_continuous(limits = c(0,500),breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Reviews Count per Restaurants in Downtown & the Strip in LV") + 
  labs(x = "Neighborhood", y ="Review Count") + 
  theme_tufte()



#![](BP.png)


# 5 Bucket Count of Reviews by City
summary(restaurants$review_count) # to see quartiles
restaurants$review_bucket <- ifelse(restaurants$review_count < 7, "1", 
                                    ifelse(restaurants$review_count < 17 & restaurants$review_count >= 7, "2", 
                                           ifelse(restaurants$review_count < 48 & restaurants$review_count >= 17, "3", "4")))

restaurants_review <- restaurants %>%
  group_by(neighborhood, review_bucket) %>%
  filter(neighborhood %in% n1) %>%
  summarise(
    n = n()
  )


s_restaurants_review <- restaurants %>%
  group_by(neighborhood, review_bucket) %>%
  filter(neighborhood %in% c('The Strip','Downtown')) %>%
  summarise(
    n = n()
  )
