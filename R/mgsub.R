mgsub <-
function(pattern, replacement = NULL, text.var, fixed = TRUE, ...){
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
        )
       return(gsub(" +", " ", x))
    }
    x <- Trim(msubs(K=key, x=text.var, ...))
    return(x)
}
