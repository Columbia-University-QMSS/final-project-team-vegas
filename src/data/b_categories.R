# make each component as a dummy variable
business <- read.csv('../../data/processed/us_restaurants.csv')

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

business$categories_l <- lapply(business$categories, convert_to_vector)
# create a new data frame 
lst <- unique(unlist(business$categories_l))
b <- data.frame(matrix(0, nrow = 16596, ncol = length(lst)))
colnames(b) <- lst # rename the column names
b <- cbind(business, b)

for (i in 1:16596){
  for (j in 1:length(colnames(b))) {
    if (colnames(b)[j] %in% business$categories_l[[i]]){
      b[i,colnames(b)[j]] = 1
    } 
  }
}



