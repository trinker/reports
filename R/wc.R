wc <- 
function(text.var, byrow = TRUE, missing = NA, digit.remove = TRUE, 
    names = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | len == 1 && (is.na(x) | is.null(x)), missing, len)
    }
    txt <- stopwords(text.var, strip = TRUE,  digit.remove = digit.remove, 
        stopwords = NULL)
    z <- sapply(txt, len2, missing = missing)
    if (!byrow) {
        return(sum(z, na.rm = TRUE)   )
    }
    if(names) {
        names(z) <- text.var
    }
    z
}

