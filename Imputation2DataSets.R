##### Code to fix a data set to have the same levels and impute others an another data set. #####
## 2 inputs are needed:
    ## DataSettoFix <- Data set that will be made to look like the other data set
    ## DataSettoUse <- Data set that will be used to change the levels in all of the factors.

allClass <- function(x) {
    unlist(
        lapply(
            unclass(x),
            class
        )
    )
}
## Create a list of the columns that are class = integer
Class.List <- which(allClass(DataSettoFix) == "factor")
Factor.List <- match(names(Class.List, names(DataSettoFix)))
nofix <- NULL
ptn <- c('*_Other_*', '*_Unknown_*')
## If there is no "_Other_" factor, then look for a "_Unknown_" factor

##### Case 1: Factor levels match for both Score and Train #####
if(length(Factor.List) > 0) {
    for(i in 1:length(Factor.List)) {
        cname <- colnames(DataSettoFix)[Factor.List[i]]
        cnumber <- which(colnames(DataSettoUse) == cname)
        if(identical(levels(DataSettoUse[ , cnumber]), levels(DataSettoFix[ , Factor.List[i]]))) {
            nofix <- c(nofix, Factor.List[i])
            print(
                paste(
                    "Column:",
                    cname,
                    "does not require any imputation.",
                    sep = " "
                )
            )
            
        } else if(all(levels(DataSettoFix[ , Factor.List[i]]) %in% levels(DataSettoUse[ , cnumber]))) {
            nofix <- c(nofix, Factor.List[i])
            DataSettoFix[ , Factor.List[i]] <- factor(DataSettoFix[ , Factor.List[i]], levels = levels(DataSettoUse[ , cnumber]))
            print(
                paste(
                    "Column:",
                    cname,
                    "does not require any imputation, training data set levels were added to the scoring data.",
                    sep = " "
                )
            )
        }
    }
    Factor.List <- Factor.List[!(Factor.List %in% nofix)]
}
##### Case 2: something in the pattern exists for imputation #####
if((length(ptn) > 0) & (length(Factor.List) > 0)) {
    for(p in 1:length(ptn)) {
        fixed = NULL
        for (i in 1:length(Factor.List)) {
            cname <- colnames(DataSettoFix)[Factor.List[i]]
            cnumber <- which(colnames(DataSettoUse) == cname)
            implevel <- levels(DataSettoUse[ , cnumber])[grep(ptn[p], levels(DataSettoUse[ , cnumber]))]
            if(length(implevel) == 1) {
                levels(DataSettoFix[ , Factor.List[i]]) <- unique(
                    c(
                        levels(DataSettoFix[ , Factor.List[i]]),
                        levels(DataSettoUse[ , cnumber])
                    )
                )
                DataSettoFix[ , Factor.List[i]][
                    !(
                        DataSettoFix[ , Factor.List[i]] %in%
                        levels(DataSettoUse[ , cnumber])
                    )
                ] <- implevel
                DataSettoFix[ , Factor.List[i]] <- droplevels(DataSettoFix[ , Factor.List[i]])
                DataSettoFix[ , Factor.List[i]] <- factor(DataSettoFix[ , Factor.List[i]], levels = levels(DataSettoUse[ , cnumber]))
                fixed <- c(fixed, Factor.List[i])
                print(
                    paste(
                        "Column:",
                        cname,
                        "has been imputed using the level,",
                        implevel,
                        sep = " "
                    )
                )
            }
        }
        Factor.List <- Factor.List[!(Factor.List %in% fixed)]
    }
}
##### Case 3: Random Imputation #####
if(length(Factor.List) > 0) {
    for(i in 1:length(Factor.List)) {
        cname <- colnames(DataSettoFix)[Factor.List[i]]
        cnumber <- which(colnames(DataSettoUse) == cname)
        idx <- which(
            !(DataSettoFix[ , Factor.List[i]] %in% levels(DataSettoUse[ , cnumber]))
        )
        DataSettoFix[idx, Factor.List[i]] <- sample(
            x = as.vector(
                DataSettoUse[ , cnumber]
            ),
            size = length(DataSettoFix[idx, Factor.List[i]]),
            replace = TRUE
        )
        DataSettoFix[ , Factor.List[i]] <- droplevels(DataSettoFix[ , Factor.List[i]])
        DataSettoFix[ , Factor.List[i]] <- factor(DataSettoFix[ , Factor.List[i]], levels = levels(DataSettoUse[ , cnumber]))
        print(
            paste(
                "Column:",
                cname,
                "has been imputed using random levels from the training data.",
                sep = " "
            )
        )
    }
}