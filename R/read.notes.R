read.notes <-
function(file = NULL, rm.nonquote = TRUE, trunc = 50, 
    notes.col = TRUE, print = TRUE) {
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
        	x <- read.xlsx(file, 1)[, 1:5]
            },
        csv = {
            x <- read.csv(file,  header = TRUE, 
                sep = ",", as.is=FALSE, na.strings= c(NA, ""), 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = TRUE)[, 1:5]
            },
        stop("invalid file extension:\n \bfile must be a .csv .xls or .xlsx" )
    )  
	nis.na <- Negate(is.na)
	x <- x[rowSums(t(apply(x, 1, nis.na))) != 0, ]	#remove empty rows
	x[, 3] <- remove2backslahes(x[, 3])  #remove backslashes for quotes only
    colnames(x) <- c("bibkey", "page", "quote", "Q", "notes")
	if (rm.nonquote) {
	    x <- x[tolower(as.character(x$Q)) %in% c("yes", "y", "t", "true", "quote"), -4]
	}
	x$bibkey <- mgsub(c("\\{", "}"), "", x$bibkey)
	x[, 1:ncol(x)] <- lapply(1:ncol(x), function(i) as.character(x[, i]))
	if (!print) {
    	if (!notes.col) {
     	    if (trunc > 0) {
	            truncdf(x, trunc)[-5]
	            return(invisible(x))
	        } else {
	            x
	        }	
	    } else {
	        if (trunc > 0) {
	            truncdf(x, trunc)
	            return(invisible(x))
	        } else {
	            x
	        }
	    }
	} else {
    	if (!notes.col) {
     	    if (trunc > 0) {
	            print(truncdf(x, trunc)[-5])
	            return(invisible(x))
	        } else {
	            x
	        }	
	    } else {
	        if (trunc > 0) {
	            print(truncdf(x, trunc))
	            return(invisible(x))
	        } else {
	            x
	        }
	    }
    }
}

remove2backslahes <- function(x){
    ## Compliments of mathematical.coffee
    ## browsURL("http://stackoverflow.com/a/15939139/1000343")
    ## split into parts separated by '$'.
    ## Add a space at the end of every string to deal with '$'
    ##   at the end of the string (as
    ##      strsplit('a$', '$', fixed=T)
    ##   is just 'a' in R)
    bits <- strsplit(paste(x, ""), "$", fixed=T)

    ## apply the regex to every second part (starting with the first)
    ## and always to the last bit (because of the ' ' we added)
    sapply(bits, function (x) {
        idx <- unique(c(seq(1, length(x), by=2), length(x)))
        x[idx] <- gsub("\\", "\"", x[idx], fixed=T)
        # join back together
        x <- paste(x, collapse="$")
        # remove that last " " we added
        substring(x, 1, nchar(x) - 1)
    }, USE.NAMES=FALSE)
}
