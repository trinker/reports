#' Insert Underscores in Path
#' 
#' A convenience wrapper to replace black spaces with underscores. 
#' 
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a character vector every space replaced with an underscore.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @export
#' @examples
#' US("bad path with spaces")
US <- function(text = "clipboard", copy2clip = TRUE){
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    text <- text_fix(text)
    und <- function(x) { 
        gsub("\\s+", "_", x)
    } 
    x <- unlist(lapply(tolower(text), und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}
