truncdf <- 
function(dataframe, end=10, begin=1) {
    x <- as.data.frame(dataframe, stringsAsFactors = FALSE)
    DF <- data.frame(lapply(x, substr, begin, end), check.names=FALSE, 
        stringsAsFactors = FALSE)
    names(DF) <- substring(names(DF), begin, end)
    DF$notes <- as.character(DF$notes)
    DF
}