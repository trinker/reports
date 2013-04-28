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
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    text <- text_fix(text)
    x <- unlist(lapply(tolower(text), simpleCap)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
} 
