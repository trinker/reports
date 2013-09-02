#' Quick Convert: Pandoc Convert a String
#' 
#' Uses Pandoc to convert from one form to another. 
#' 
#' @param to Pandoc type you're converting to (defualt is \code{"latex"}).
#' @param from Pandoc type you're converting from (defualt is \code{"markdown"}).
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This capitalizes every word of a string.  
#' @return Returns a character vector every word capitalized.
#' @section Warning: Ligatures parsing is very good, however, these elements may
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
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    if (from != "latex") {
        ext <- text_fix(text)  
    }   
    x <- system2(wheresPandoc(), paste0("-f ", from, " -t ", to), input = text, 
        stdout = TRUE)
    x <- paste(x, collapse=" ")
    if(copy2clip){
        write_clip(x)
    }
    return(x)    
}
