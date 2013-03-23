#' Format Quotes with Citations
#' 
#' Citation tools to grab a quote and format the text taken from the 
#' notes.xlsx/notes.csv for LaTeX with optional citation included.  Functions 
#' attempt to copy the output to the clipboard for easy paste inclusion.     
#' 
#' @param text.loc The row number (integer value) from notes.xlsx/notes.csv of 
#' the text to insert.  The user may also input a character string of partial 
#' matches of the bibkeys (a quasi-author search).  If NULL the interactive use 
#' allows the user to view the notes.xlsx/notes.csv and chose a row number.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @param citation logical.  If TRUE will include the formatted citation + the 
#' quote.
#' @details Each of the functions in the cite family follow a pattern of 
#' (cite, textcite, posscite, poscite) prefix and (L or M) suffix.  The 
#' cite and textcite are in the form of LaTeX commands by the same name.  
#' posscite and poscite are user defined LaTeX function styles that are 
#' extensions of the textcite command to fit possessive and -s- ending 
#' possessives.  They can be defined as:
#' 
#' \code{\\newcommand\\posscite[1]{\\citeauthor{#1}'s (\\citeyear{#1})}} \cr
#' \code{\\newcommand\\poscite[1]{\\citeauthor{#1}' (\\citeyear{#1})}} \cr
#'
#' The L and M correspond to LaTeX or markdown outputs; markdown relies on the 
#' \code{knitcitations} package.
#' @section Note: It is expected that the user maintains notes.xlsx/notes.csv 
#' with markdown notation (e.g., ** for bold type and * for italics).
#' @rdname cite
#' @return Returns a character vector with LaTeX/markdown formatted text.
#' @export
citeL <- function(text.loc = NULL, copy2clip = TRUE, citation = TRUE) {
	out <- CITEhelper(text.loc = text.loc)
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

#' @rdname cite 
#' @export 
textciteL <- function(text.loc = NULL, copy2clip = TRUE, citation = TRUE) {
	out <- CITEhelper(text.loc = text.loc)
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
    	citeK <- paste("\\textcite{", out[[3]], "}", sep="")
    	pgs <- paste("(", PP, ". ", out[[2]], ")", sep="")
        if (!LONG) {
            x <- paste(citeK, " ", L, text, R, pgs, sep="")	
        } else {
        	x <- paste(citeK, "\n", L, paste(text, pgs), R, collapse = "\n")
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
    	bod <- strWrap(paste(text, pgs), copy2clip = FALSE, invisible=TRUE)
    	body <- paste(paste(" ", bod), collapse="\n")
        cat(citeK); cat("\n"); cat(L); cat(body); cat(R); cat("\n")
    } else {
        cat(x)
    }
	invisible(x)
}

#' @rdname cite 
#' @export 
possciteL <- function(text.loc = NULL, copy2clip = TRUE, citation = TRUE) {
	out <- CITEhelper(text.loc = text.loc)
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
    	citeK <- paste("\\posscite{", out[[3]], "}", sep="")
    	pgs <- paste("(", PP, ". ", out[[2]], ")", sep="")
        if (!LONG) {
            x <- paste(citeK, " ", L, text, R, pgs, sep="")	
        } else {
        	x <- paste(citeK, "\n", L, paste(text, pgs), R, collapse = "\n")
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
    	bod <- strWrap(paste(text, pgs), copy2clip = FALSE, invisible=TRUE)
    	body <- paste(paste(" ", bod), collapse="\n")
        cat(citeK); cat("\n"); cat(L); cat(body); cat(R); cat("\n")
    } else {
        cat(x)
    }
	invisible(x)
}

#' @rdname cite 
#' @export 
posciteL <- function(text.loc = NULL, copy2clip = TRUE, citation = TRUE) {
	out <- CITEhelper(text.loc = text.loc)
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
    	citeK <- paste("\\poscite{", out[[3]], "}", sep="")
    	pgs <- paste("(", PP, ". ", out[[2]], ")", sep="")
        if (!LONG) {
            x <- paste(citeK, " ", L, text, R, pgs, sep="")	
        } else {
        	x <- paste(citeK, "\n", L, paste(text, pgs), R, collapse = "\n")
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
    	bod <- strWrap(paste(text, pgs), copy2clip = FALSE, invisible=TRUE)
    	body <- paste(paste(" ", bod), collapse="\n")
        cat(citeK); cat("\n"); cat(L); cat(body); cat(R); cat("\n")
    } else {
        cat(x)
    }
	invisible(x)
}

  
