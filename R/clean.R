clean <-
function(text.var) {
    gsub("\\s+", " ", gsub("\r|\n|\t", " ", text.var))
}
