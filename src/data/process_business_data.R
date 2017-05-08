#########################
# Yelp Project
# Process Business CSV
# 02/27/2017
#########################

# Read in Business CSV
print("Reading in interim business csv file")
business <- read.csv("../../data/interim/business.csv")

# Function to convert to vector
convert_to_vector <- function(x) {
  x <- as.character(x)
  x <- gsub("\\[u\\'", "\\'", x)
  x <- gsub(", u\\'", ", \\'", x)
  x <- gsub(',\\s', ',', x)
  x <- gsub('\\[', '', x)
  x <- gsub(']', '', x)
  x <- gsub("\\'", "", x)
  x <- as.vector(strsplit(x, ","))[[1]]
  return(x)
}

# Convert attributes and categories to lists of vectors
print("converting category variable to list of vectors")
business$categories_l <- lapply(business$categories, convert_to_vector)
#business$attributes_l <- lapply(business$attributes, convert_to_vector)

# Create dummy variable for large city vs smaller city
# Dummy variable for Restaurants
print("creating dummy variables")
business$restaurant <- unlist(lapply(business$categories_l, function(x) {unlist(ifelse("Restaurants" %in% x, 1, 0))}))

agg <- aggregate(business$restaurant, by=list(city=business$city), FUN=sum)
large_cities <- subset(agg, x >= 1000)$city
small_cities <- subset(agg, x < 1000)$city
business$city_type <- unlist(lapply(business$city, function (x) {ifelse(x %in% large_cities, 1, 0)}))

# Updated to extract restaurant file for global and restaurant file for US Only
agg_lg_cities <- aggregate(business$restaurant[business$city %in% large_cities], by=list(city=business$city[business$city %in% large_cities]), FUN=sum)
agg_lg_cities <- agg_lg_cities[order(agg_lg_cities$x, decreasing = TRUE),]
agg_gbl_cities <- agg_lg_cities$city[agg_lg_cities$city %in% c("Montréal", "Pittsburgh", "Edinburgh", "Cleveland", "Stuttgart")]
agg_us_cities <- agg_lg_cities$city[agg_lg_cities$city %in% c("Las Vegas", "Phoenix", "Charlotte", "Pittsburgh", "Scottsdale", "Cleveland", "Mesa")]
business$global_restaurant <- ifelse(business$city %in% agg_gbl_cities & business$restaurant == 1, 1, 0)
business$us_restaurant <- ifelse(business$city %in% agg_us_cities & business$restaurant == 1, 1, 0)

# Output files
print("output processed business csv")
write.csv(business[, !(names(business) == "categories_l")], "../../data/processed/business.csv")

print("output new restaurants csv")
write.csv(business[business$restaurant == 1 & business$city_type == 1, !(names(business) == "categories_l")], "../../data/processed/restaurants.csv")

print("output new global restaurants csv")
write.csv(business[business$global_restaurant == 1, !(names(business) == "categories_l")], "../../data/processed/global_restaurants.csv")

print("output new us only restaurants csv")
write.csv(business[business$us_restaurant == 1, !(names(business) == "categories_l")], "../../data/processed/us_restaurants.csv")

# QA against exploratory restaurant counts
# agg <- aggregate(business$restaurant, by=list(city=business$city), FUN=sum)
# agg <- subset(agg, x > 500)
# agg[order(agg$x, decreasing = TRUE),]

# Number of categories per business
# table(unlist(lapply(business$categories_l, length)))

