##### Draft Vector Creation #####
## Create a vector to simulate a draft order based on how many picks each player gets.
## First, make sure the data set has the following columns:
## 1. userid - some id to make sure we know who gets each pick.
## 2. rank - the order in which each person will get to pick.
## 3. count - the number of picks each person gets.

draftVector <- function(data) {
    vectorOP <- c()
    data <- data[order(data[ , "rank"], decreasing = FALSE), ]
    
    while(sum(data[ , "count"]) > 0) {
        data <- data[data[ , "count"] > 0, ]
        repVec <- as.vector(
            rep(
                data[ , "userid"],
                length.out = (min(data[ , "count"])*nrow(data))
            )
        )
        vectorOP <- c(vectorOP, repVec)
        data[ , "count"] <- data[ , "count"] - min(data[ , "count"])
    }
    vectorOP
}

##### Serpentine Draft Vector Creation #####
## Create a vector to simulate a draft order based on how many picks each player gets.
## First, make sure the data set has the following columns:
## 1. userid - some id to make sure we know who gets each pick.
## 2. rank - the order in which each person will get to pick.
## 3. count - the number of picks each person gets.

serpdraftVector <- function(data) {
    c = 0
    vectorOP <- c()
    
    while(sum(data[ , "count"]) > 0) {
        c = c + 1
        data <- data[data[ , "count"] > 0, ]
        
        if(c %% 2 == 1) {
            data <- data[order(data[ , "rank"], decreasing = FALSE), ]
        } else {
            data <- data[order(data[ , "rank"], decreasing = TRUE), ]
        }
        repVec <- as.vector(
            rep(
                data[ , "userid"],
                length.out = nrow(data)
            )
        )
        vectorOP <- c(vectorOP, repVec)
        data[ , "count"] <- data[ , "count"] - 1
    }
    vectorOP
}