write_clip <- function(x) {
    ## The code for this helper function comes from the oveRflow package.  
    ## # https://raw.github.com/sebastian-c/oveRflow/master/R/writeClip.R
    ## This is code I submitted but was modified by the package maintainers.
    ## The idea to keep this function as a modular unit makes sense and was 
    ## subsequently applied to the reports package
    OS <- Sys.info()["sysname"]
    
    if(!(OS %in% c("Darwin", "Windows", "Linux"))) {
        stop("Copying to clipboard not supported on your OS")
    }
    
    if (OS != "Windows") {
        writeClipboard <- NULL
    } 
    
    switch(OS, 
        "Darwin"={j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)   
        },
        "Windows"=writeClipboard(x, format = 1),
        "Linux"={
            if(Sys.which("xclip") == "") {
              stop("Clipboard on Linux requires 'xclip'. Try using:\nsudo apt-get install xclip")
            }
            con <- pipe("xclip -i", "w")
            writeLines(x, con=con)
            close(con)
        }
    )
}

prin <- function(x, print) {
    if (print) {
        cat(x)
        invisible(x)
    } else {
        x	
    }
}

read_clip <- function() {
    OS <- Sys.info()["sysname"]
    
    if(!(OS %in% c("Darwin", "Windows"))) {
        stop("Copying to clipboard not supported on your OS")
    }
    
    if (OS != "Windows") {
        readClipboard <- NULL
    } 
    

    switch(OS, 
        "Darwin" = {j <- pipe("pbcopy", "w")                       
            pcon <- pipe("pbpaste")
            out <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        },
        "Windows" = {out <- readClipboard()}
    )
    out
}

scrubber <-
function(text.var, rm.quote = TRUE, fix.comma = TRUE, ...){
    x <- reducer(Trim(clean(text.var)))
    if (rm.quote) {
        x  <- gsub('\"', "", x)
    }
    if (fix.comma) {
        x <- gsub(" ,", ",", x)
    }
    ncx <- nchar(x)
    x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, ncx))
    x[is.na(text.var)] <- NA
    x
}

strWrap <-
function(text = "clipboard", width = 70, copy2clip = TRUE, invisible = FALSE) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- paste(scan(pcon, what="character", 
                quiet=TRUE), collapse=" ")
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(readClipboard(), collapse=" ")
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", text))
    x <- strwrap(x, width = width)
    if(copy2clip){
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(x, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {           
            j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)                                    
        }             
    }
    if (!invisible) {
    	writeLines(x)
    }
    return(invisible(x))
}

Trim <-
function (x) gsub("^\\s+|\\s+$", "", x)

truncdf <- 
function(dataframe, end=10, begin=1) {
    x <- as.data.frame(dataframe, stringsAsFactors = FALSE)
    DF <- data.frame(lapply(x, substr, begin, end), check.names=FALSE, 
        stringsAsFactors = FALSE)
    names(DF) <- substring(names(DF), begin, end)
    DF
}


unblanker <-
function(x)subset(x, nchar(x)>0)

reducer <- 
function(x) gsub("\\s+", " ", x)

wc <- 
function(text.var) {
    y <- tolower(clean(Trim(gsub(".*?($|'|[^[:punct:]]).*?", 
        "\\1", as.character(text.var)))))
    length(unlist(strsplit(gsub("\\s+", " ", y), "\\s")))
}

wheresPandoc <- function() {
    myPaths <- c("pandoc",  "~/.cabal/bin/pandoc", 
        "~/Library/Haskell/bin/pandoc", "C:\\PROGRA~1\\Pandoc\\bin\\pandoc")
    panloc <- Sys.which(myPaths)
    temp <- panloc[panloc != ""]
    if (identical(names(temp), character(0))) {
        ans <- readline("Pandoc not installed in one of the typical locations.\n 
            Do you know where Pandoc is installed? (y/n) ")
        if (ans == "y") {
        	temp <- readline("Enter the (unquoted) path to Pandoc: ")
        } else {
            if (ans == "n") {
            	stop("Pandoc not installed or not found.")
            }
        }
    } 
    temp
}

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
	            truncdf(x, trunc)[, 1:3]
	            return(invisible(x[, 1:3]))
	        } else {
	            x[, 1:3]
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
	            print(truncdf(x, trunc)[, 1:3])
	            return(invisible(x[, 1:3]))
	        } else {
	            x[, 1:3]
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

paste2 <-
function(multi.columns, sep=".", handle.na=TRUE, trim=TRUE){
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns)
    }
    if (trim) multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        }
    )
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call('cbind', multi.columns)
    } 
    m <- if (handle.na){
                 apply(multi.columns, 1, function(x){
                     if (any(is.na(x))){
                         NA
                     } else {
                         paste(x, collapse = sep)
                     }
                 }
             )   
         } else {
             apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}

#NO LONGER EXPORTED: USE installr instead
#
#Download Pandoc
#
#Download Pandoc from the command line (Windows users). 
#
#@return Installs Pandoc on your system.
#@author Gergely Daroczi and Gabor Grothendieck
#@references \url{http://stackoverflow.com/a/15072501/1000343} 
#@section Pandoc Website: \url{http://johnmacfarlane.net/pandoc/}
#@export
#@examples
#\dontrun{
#install_pandoc()
#}
install_pandoc <- function() {
    page <- readLines('http://code.google.com/p/pandoc/downloads/list', warn = FALSE)
    pat  <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+-setup.exe"
    line <- grep(pat, page, value = TRUE); m <- regexpr(pat, line)
    url  <- paste('http', regmatches(line, m), sep = ':')
    tmp <- tempfile(fileext = '.exe')
    download.file(url, tmp, mode = 'wb')
    system(tmp)
    on.exit(unlink(tmp))
}

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

genXtract <- 
function(text.var, left, right, with = FALSE, merge = TRUE){
    if (length(left) != length(right)) {
        stop("left and right must be equal length") 
    }
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")
    left <- mgsub(specchar, paste0("\\", specchar), left, fixed = TRUE)
    right <- mgsub(specchar, paste0("\\", specchar), right, fixed = TRUE)
    FUN <- function(left, right, text.var, with){   
        fmt <- if (with==TRUE) {
            "(%s).*?(%s)"
        } else {
            "(?<=%s).*?(?=%s)"
        }
        re <- sprintf(fmt, as.character(left), as.character(right))
        if(length(text.var)==1){
            unlist(regmatches(text.var, gregexpr(re, text.var, perl=TRUE)))
        }else{  
            regmatches(text.var, gregexpr(re, text.var, perl=TRUE)) 
        }
    }
    out <- invisible(lapply(seq_along(left), function(i) {
        FUN(left[i], right[i], text.var = text.var, with = with)
    }))
    names(out) <- paste(left, " : ", "right")
    if (length(left) == 1) {
        return(unlist(out, recursive = FALSE))
    } else {
        if (merge) {
            out <- invisible(lapply(seq_along(text.var), function(i) {
                unlist(invisible(lapply(seq_along(out), function(j) {
                    out[[j]][[i]]
                })))
            }))            
        }
    }
    out
}

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

