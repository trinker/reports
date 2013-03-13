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
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
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
    ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
    text <- gsub("([\\?])([aeiouy])", "\\fl\\2", text)
    text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
    nligs <- length(ligs)
    if (nligs[1] > 0) {
        plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
        warning(paste(nligs, "possible", plural, "found: \nCheck output!"))
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
