#internal function
genX <-
function (text.var, left, right, missing = NULL, names = FALSE, scrub = TRUE) {
    if (length(left) != length(right)) {
        stop("left and right must be equal length") 
    }
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")
    left <- mgsub(specchar, paste0("\\", specchar), left, fixed = TRUE)
    right <- mgsub(specchar, paste0("\\", specchar), right, fixed = TRUE)
    FUN <- function(left, right, text.var, missing, names) {
        X <- sapply(text.var, function(x) gsub(paste0(left, ".+?", right), "", x))
        if (scrub) {
            X <- scrubber(gsub(" +", " ", X))
        }
        if (!is.null(missing)) {
            X[X == ""] <- missing
        }
        if (!names) names(X) <- NULL
        X
    }
    invisible(lapply(seq_along(left), function(i) {
        text.var <<- FUN(left[i], right[i], text.var = text.var, 
            missing = missing, names = names)
    }))
    text.var
}
