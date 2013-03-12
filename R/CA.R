#' Capitalize Every Word of String
#' 
#' Capitalizes every word of a string. 
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This capitalizes every word of a string.  
#' @return Returns a character vector every word capitalized.
#' @section Warning: Ligatures are assumed to be "fi", however, these elements 
#' may be "ff", "fi", "fl", "ffi" or "ffl".
#' @export
#' @examples
#' CA("the flexible, efficient way to do reports.")
CA <- function(text = "clipboard", copy2clip = TRUE) { 
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
    text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
    ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
    nligs <- length(ligs)
    if (ligs[1] > 0) {
        plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
        warning(paste(ligs, plural, "found: \nCheck output!"))
    }
    simpleCap <- function(x) { 
        s <- strsplit(x, " ")[[1]] 
        paste(toupper(substring(s, 1,1)), substring(s, 2), 
            sep="", collapse=" ") 
    } 
    x <- unlist(lapply(tolower(text), simpleCap)) 
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
    return(x)
} 
