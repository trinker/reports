#' Insert Presenter Notes
#' 
#' Creates an aside of the class "notes" for reveal.js slides
#' 
#' @param text A character vector or text copied to the clipboard.  Default is 
#' to read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector wrapped with HTML aside tags.
#' @export
#' @examples
#' PN("some fancy notes", print = TRUE)
#' PN("1) some\n2) fancy\n3) notes")
#' PN("1) some 
#' 2) fancy 
#' 3) notes", print = TRUE)
PN <- function(text = "clipboard", copy2clip = TRUE, print = FALSE) { 
    if (text == "clipboard") {
        text <- read_clip()
    } 
    text <- chartr("\\", "/", text)
    x <- paste0(paste(paste(unlist(strsplit(text, "\n")), "<br>"), 
        collapse="\n"), "\n")
    x <- paste0("<aside class=\"notes\">\n",x, "</aside>\n")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
