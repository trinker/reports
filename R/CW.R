#' Count Words
#' 
#' Tool to count words from text taken from articles.
#' 
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @details This function counts the number of words in a text chunk.  Useful 
#' for determining if block quotes are needed in APA6.  While the function 
#' accepts vectors the real increase in work flow productivity is copying from 
#' the clipboard.
#' @export
#' @examples
#' CW("I like icecream")
CW <- 
function(text = "clipboard") {
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    wc(text_fix(paste2(text)))
}
