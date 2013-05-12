#' Path/URL: Fill Blank Spaces
#' 
#' \code{UF} - A convenience wrapper to replace black spaces of paths with 
#' underscores. 
#' 
#' @param path A character vector or url/path copied to the clipboard.  Default 
#' is to read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @param fill The character symbol to fill the blank spaces with.
#' @return Returns a character vector every space replaced with an 
#' underscore/percent.  \code{UF} also converts the text to lower case.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @rdname path_fill
#' @export
#' @examples
#' UF("bad path with spaces")
#' PF("https://github.com/trinker/reports/fictional path to nowhere.pdf")
UF <- function(path = "clipboard", copy2clip = TRUE, fill = "_"){
    if (length(path) == 1 && path == "clipboard") {
        path <- read_clip()
    } 
    text <- text_fix(path)
    und <- function(x) { 
        gsub("\\s+", fill, x)
    } 
    x <- unlist(lapply(tolower(text), und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}

#' Path/URL: Fill Blank Spaces
#'
#' \code{PF} - A convenience wrapper to replace black spaces of URLs with 
#' percents.
#' 
#' @export
#' @rdname path_fill
PF <- function(path = "clipboard", copy2clip = TRUE, fill = "%"){
    if (length(path) == 1 && path == "clipboard") {
        path <- read_clip()
    } 
    text <- text_fix(path)
    und <- function(x) { 
        gsub("\\s+", fill, x)
    } 
    x <- unlist(lapply(text, und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}
