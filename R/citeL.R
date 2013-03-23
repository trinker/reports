#' @export
citeL <- function(text = NULL, copy2clip = TRUE, citation = TRUE) {
	out <- CITEhelper(text = text)
    text <- out[[1]]
    if (wc(text) > 39) {
        LONG <- TRUE
        L <- "\\begin{quote}\n"
        R <- "\n\\end{quote}"
    } else {
    	LONG <- FALSE    	
        L <- "``"
        R <- "'' "
    }
	citeK <- NULL
    if (citation) {
    	PP <- grepl("-", out[[2]])
    	PP <- ifelse(PP, "pp", "p")
    	citeK <- paste("\\cite[", PP, ". ", out[[2]], "]{", out[[3]], "}", sep="")
        if (!LONG) {
            x <- paste(L, text, R, citeK, sep="")	
        } else {
        	x <- paste(L, text, citeK, R, collapse = "\n")
        }    	
    }
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
    if (LONG) {
    	bod <- strWrap(paste(text, citeK), copy2clip = FALSE, invisible=TRUE)
    	body <- paste(paste(" ", bod), collapse="\n")
        cat(L); cat(body); cat(R); cat("\n")
    } else {
        cat(x)
    }
	invisible(x)
}

  