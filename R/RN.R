#' Insert reveal.js Notes
#' 
#' Creates an aside of the class "notes" for reveal.js slides
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If FALSE returns to the console.
#' @return Returns a character vector wrapped with a courier new font tag.
#' @export
#' @examples
#' RN("some fancy notes")
#' RN("1) some 
#' 2) fancy 
#' 3) notes")
RN <- function(text = "clipboard", copy2clip = TRUE, print = FALSE) { 
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- readClipboard()
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    text <- chartr("\\", "/", text)
    x <- paste0(paste(paste("  ", unlist(strsplit(text, "\n"))), collapse="\n"), "\n")
    x <- paste0("<aside class=\"notes\">\n",x, "</aside>\n")
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
    if (print) {
        cat(x)
        invisible(x)
    } else {
        x	
    }
}
