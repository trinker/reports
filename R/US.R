#' Insert Underscores in Path
#' 
#' A convenience wrapper to replace black spaces with underscores. 
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a character vector every space replaced with an underscore.
#' @section Warning: Ligatures are assumed to be "fi", however, these elements 
#' may be "ff", "fi", "fl", "ffi" or "ffl".
#' @export
#' @examples
#' US("bad path with spaces")
US <- function(text = "clipboard", copy2clip = TRUE){
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
    ligs <- length(gregexpr("([\\?])([a-z])", text)[[1]])
    if (ligs > 0) {
        plural <- ifelse(ligs > 1, "ligatures were", "ligature was")
        warning(paste(ligs, plural, "found: \nCheck output!"))
    }
    und <- function(x) { 
        gsub("\\s+", "_", x)
    } 
    x <- unlist(lapply(tolower(text), und)) 
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