#' Count Words
#' 
#' Tool to count words from text taken from articles.
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @details This function counts the number of words in a text chunk.  Useful 
#' for determining if block quotes are needed in APA6.  While the function 
#' accepts vectors the real increase in work flow productivity is copying from 
#' the clipboard.
#' @export
#' @examples
#' \dontrun{
#' CW("I like icecream")
#' }
CW <- 
function(text = "clipboard") {
    if (Sys.info()["sysname"] != "Windows") {
        readClipboard <- NULL
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
    wc(paste2(text), names=F)
}
