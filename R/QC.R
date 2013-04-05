#' Pandoc Convert a String
#' 
#' Uses Pandoc to convert from one form to another. 
#' 
#' @param to Pandoc type you're converting to (defualt is \code{"latex"}).
#' @param from Pandoc type you're converting from (defualt is \code{"markdown"}).
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This capitalizes every word of a string.  
#' @return Returns a character vector every word capitalized.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @author Michael Nelson and Tyler Rinker<tyler.rinker@@gmail.com>.
#' @references
#' \href{http://johnmacfarlane.net/pandoc/demos.html}{Pandoc Website}
#' \url{http://stackoverflow.com/a/15539563/1000343}
#' @export
#' @examples
#' x <- "\\*note: I *like* chocolate **milk** too ***much***!"
#' QC(text=x)
#' QC(text=x, "html")
QC <- function(to = "latex", from = "markdown", text = "clipboard", 
    copy2clip = TRUE) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (length(text) == 1 && text == "clipboard") {
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
    if (from != "latex") {
        ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
        text <- gsub("([\\?])([aeiouy])", "\\fl\\2", text)
        text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
        nligs <- length(ligs)
        if (ligs[1] > 0) {
            plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
            warning(paste(nligs, "possible", plural, "found: \nCheck output!"))
        }    
    }   
    x <- system2(wheresPandoc(), paste0("-f ", from, " -t ", to), input = text, 
        stdout = TRUE)
    x <- paste(x, collapse=" ")
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