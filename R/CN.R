#' Wrap Text With HTML Courier New Font Tag
#' 
#' Wraps text with a courier new font tag. A specified version of \code{FT} but 
#' more convient for constant use of courier new tags. 
#' 
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a character vector wrapped with a courier new font tag.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @export
#' @examples
#' CN("new_report()")
CN <- function(text = "clipboard", copy2clip = TRUE) { 
    if (text == "clipboard") {
        text <- read_clip()
    } 
    text <- text_fix(text)
    x <- noquote(paste0("\"<font face=\"courier\">", text, "</font>"))
    if(copy2clip){
        write_clip(x)
    }
    return(x)
} 
