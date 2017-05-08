d <- read.csv("C:/Users/Veronica/Desktop/Spring 2017/Data Visualization 4063/final/data.csv")
data <- as.data.frame(d$attributes)


data2 <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
names(data)[1] <- "att"
data$att <- gsub("'", "", data$att)

data2$Alcohol <-  ifelse(grepl("Alcohol: none", data$att, ignore.case = TRUE), 0,
                         ifelse(grepl("Alcohol", data$att, ignore.case = TRUE), 1, "-"))

table(data2$Alcohol)

data2$Ambience <- ifelse(grepl("romantic: True", data$att, ignore.case = TRUE), 1,
                         ifelse(grepl("intimate: True", data$att, ignore.case = TRUE), 2, 
                                ifelse(grepl("classy: True", data$att, ignore.case = TRUE), 3,
                                       ifelse(grepl("hipster: True", data$att, ignore.case = TRUE), 4,
                                              ifelse(grepl("divey: True", data$att, ignore.case = TRUE), 5,
                                                     ifelse(grepl("touristy: True", data$att, ignore.case = TRUE), 6,
                                                            ifelse(grepl("trendy: True", data$att, ignore.case = TRUE), 7,
                                                                   ifelse(grepl("upscale: True", data$att, ignore.case = TRUE), 8,
                                                                          ifelse(grepl("casual: True", data$att, ignore.case = TRUE), 9, "-")))))))))
table(data2$Ambience)

data2$Parking <- ifelse(grepl("garage: True", data$att, ignore.case = TRUE), 1,
                        ifelse(grepl("street: True", data$att, ignore.case = TRUE), 2,
                               ifelse(grepl("validated: True", data$att, ignore.case = TRUE), 3,
                                      ifelse(grepl("lot: True", data$att, ignore.case = TRUE), 4,
                                             ifelse(grepl("valet: True", data$att, ignore.case = TRUE), 5, '-')))))
table(data2$Parking)

data2$Meal <- ifelse(grepl("dessert: True", data$att, ignore.case = TRUE), 1,
                     ifelse(grepl("latenight: True", data$att, ignore.case = TRUE), 2,
                            ifelse(grepl("lunch: True", data$att, ignore.case = TRUE), 3,
                                   ifelse(grepl("dinner: True", data$att, ignore.case = TRUE), 4,
                                          ifelse(grepl("breakfast: True", data$att, ignore.case = TRUE), 5,
                                                 ifelse(grepl("brunch: True", data$att, ignore.case = TRUE), 6, '-'))))))

table(data2$Meal)

data2$attire <- ifelse(grepl("uRestaurantsAttire: casual", data$att, ignore.case = TRUE), 1,
                       ifelse(grepl("uRestaurantsAttire: dressy", data$att, ignore.case = TRUE), 2,
                              ifelse(grepl("uRestaurantsAttire: formal", data$att, ignore.case = TRUE), 3, "-")))

table(data2$attire)

data2$noise <- ifelse(grepl("NoiseLevel: quiet", data$att, ignore.case = TRUE), 1,
                      ifelse(grepl("NoiseLevel: average", data$att, ignore.case = TRUE), 2,
                             ifelse(grepl("NoiseLevel: loud", data$att, ignore.case = TRUE), 3,    
                                    ifelse(grepl("NoiseLevel: very_loud", data$att, ignore.case = TRUE), 4, "-"))))
table(data2$noise)

data2$kids <- ifelse(grepl("GoodForKids: True", data$att, ignore.case = TRUE), 1, 
                     ifelse(grepl("GoodForKids", data$att, ignore.case = TRUE), 0, "-")) 

table(data2$kids)  


data2$price <- ifelse(grepl("RestaurantsPriceRange2: 1", data$att, ignore.case = TRUE), 1, 
                      ifelse(grepl("RestaurantsPriceRange2: 2", data$att, ignore.case = TRUE), 2, 
                             ifelse(grepl("RestaurantsPriceRange2: 3", data$att, ignore.case = TRUE), 3,
                                    ifelse(grepl("RestaurantsPriceRange2: 4", data$att, ignore.case = TRUE), 4,
                                           "-"))))
                             
                             table(data2$price)  
                             
                             
                             write.csv(data2, "data2.csv")
                             
                             
                             
                             