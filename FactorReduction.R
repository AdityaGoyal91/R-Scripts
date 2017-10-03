##### Factor Reduction Code #####

## This code is to reduce the number of levels within a factor variable to help with sparcity.
    ## we will classify them as "_other_"
    ## Also, any missing values will be categorized as "_Unknown_"
## 2 inputs will be needed for this code:
    ## 1. DataSettoTrim <- data set that needs to be cleaned up
    ## 2. numberoffactors <- how many unique levels each factor should have at max

allClass <- function(x) {
    unlist(
        lapply(
            unclass(x),
            class
        )
    )
}

Class.List <- which(allClass(DataSettoTrim) == "factor")
Factor.List <- match(names(Class.List, names(DataSettoTrim)))

for(i in 1:length(Factor.List)) {
    if(length(DataSettoTrim[ ,Factor.List[i]][DataSettoTrim[ ,Factor.List[i]] == ""]) > 0) {
        levels(DataSettoTrim[ ,Factor.List[i]]) <- c(levels(DataSettoTrim[ ,Factor.List[i]]), "_Unknown_")
        DataSettoTrim[ ,Factor.List[i]][DataSettoTrim[ ,Factor.List[i]] == ""] <- "_Unknown_"
    }    
}
DataSettoTrim <- droplevels(DataSettoTrim)

for(j in 1:length(Factor.List)) {
    if(length(levels(DataSettoTrim[ , Factor.List[j]])) > numberoffactors) {
        assign(
            paste(
                names(DataSettoTrim)[Factor.List[j]],
                ".keep",
                sep = ""
            ),
            names(
                sort(
                    summary(
                        DataSettoTrim[ , Factor.List[j]]
                    ),
                    decreasing = TRUE
                )[1:numberoffactors]
            )
        )
        levels(DataSettoTrim[ , Factor.List[j]]) <- c(levels(DataSettoTrim[ , Factor.List[j]]), "_Other_")
        DataSettoTrim[, Factor.List[j]][!(
            DataSettoTrim[ , Factor.List[j]] %in%
            get(
                paste(
                    names(
                        DataSettoTrim
                    )[Factor.List[j]], 
                    ".keep", 
                    sep = ""
                )
            )
        )] <- "_Other_"
        DataSettoTrim <- droplevels(DataSettoTrim)
    }
}
