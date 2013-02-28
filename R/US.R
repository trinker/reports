#' Insert Underscores in Path
#' 
#' A convienence wrapper to replace black spaces with underscores. 
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a character vector every space repalced with an underscore.
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