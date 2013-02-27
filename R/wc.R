wc <- 
function(text.var) {
    y <- tolower(clean(Trim(gsub(".*?($|'|[^[:punct:]]).*?", 
        "\\1", as.character(text.var)))))
    length(unlist(strsplit(gsub("\\s+", " ", y), "\\s")))
}

