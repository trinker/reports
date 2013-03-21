read.notes <- function(file = NULL, rm.nonquote = TRUE, trunc = 50) {
	if (is.null(file)) {
	    loc <- file.path(getwd(), "ARTICLES")
	    locfls <- dir(loc)
	    poss <- locfls[grepl("notes", locfls)]
	    ins <- poss[!grepl("~$", poss, fixed=TRUE)][1]
	    file <- file.path(loc, ins)
    }
    ext <- tools::file_ext(file)
    switch(ext, 
        xlsx = {
            x <- read.xls(file,  header = TRUE, 
                sep = ",", as.is=FALSE, na.strings= c(NA, ""), 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = TRUE)
            },
        csv = {
            x <- read.csv(file,  header = TRUE, 
                sep = ",", as.is=FALSE, na.strings= c(NA, ""), 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = TRUE)
            },
        stop("invalid file extension:\n \bfile must be a .csv .xls or .xlsx" )
    )  
    colnames(x) <- c("bibkey", "page", "quote", "Q", "notes")
	if (rm.nonquote) {
	    x <- x[tolower(as.character(x$Q)) %in% c("yes", "y", "t", "true", "quote"), -4]
	}
	x$bibkey <- mgsub(c("\\{", "}"), "", x$bibkey)
	x[, 1:ncol(x)] <- lapply(1:ncol(x), function(i) as.character(x[, i]))
	if (trunc > 0) {
	    print(truncdf(x, trunc))
	    return(invisible(x))
	} else {
	    x
	}
}