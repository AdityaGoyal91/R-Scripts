##### Draft Vector Creation #####
## Create a vector to simulate a draft order based on how many picks each player gets.
## First, make sure the data set has the following columns:
## 1. user id - some id to make sure we know who gets each pick.
## 2. rank - the order in which each person will get to pick.
## 3. count - the number of picks each person gets.

draftVector <- function(data) {
    vectorOP <- c()
    data <- data[order(data[ , "Rank"], decreasing = FALSE)]
    
    while(sum(data[ , "Leads"]) > 0) {
        repVec <- as.vector(
            rep(
                data[ , "User ID"],
                length.out = min(data[ , "Leads"])*nrow(data)
            )
        )
        
        vectorOP <- c(vectorOP, repVec)
        data[ , "Leads"] <- data[ , "Leads"] - min(data[ , "Leads"])
    }
    vectorOP
}
