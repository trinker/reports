#' Convert email to HTML email Tag
#' 
#' Wrap an email to generate an HTML email tag.
#' 
#' @param email A character vector email copied to the clipboard. Default is to 
#' read from the clipboard.  
#' @param text A character vector of text to hyperref from.  Defualt uses the 
#' string passed to \code{email}.
#' @param new_win logical.  If \code{TRUE} the link will open in a new window.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector of an HTML email tag. 
#' @references
#' \url{http://www.w3schools.com/tags/tag_address.asp}
#' @export
#' @examples
#' EM("tyler.rinker@@gmail.com", print = TRUE)
EM <- function(email = "clipboard", text = NULL, new_win = TRUE, 
	copy2clip = interactive(), print = FALSE) {
	
    if (email == "clipboard") {
        email <- read_clip()
    } 
    if (is.null(text)) {
        text <- email
    }
    if (new_win) {
        tar <- " target=\"_blank\""     
    } else {
        tar <- NULL
    }
    x <- paste0("<a href=\"mailto:", email, "\"", tar, ">", text, "</a>")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
