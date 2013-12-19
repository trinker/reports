#' Path/URL: Fill Blank Spaces
#' 
#' \code{UF} - A convenience function to replace black spaces of paths with 
#' underscores. 
#' 
#' @param path A character vector or url/path copied to the clipboard.  Default 
#' is to read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @param fill The character symbol to fill the blank spaces with.
#' @return Returns a character vector every space replaced with an 
#' underscore/percent And apostophes removed.  \code{UF} also converts the text 
#' to lower case.
#' @section Warning: Ligatures parsing is very good, however, these elements may
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @rdname path_fill
#' @export
#' @examples
#' UF("bad path with spaces")
#' PF("https://github.com/trinker/reports/fictional path to nowhere.pdf")
UF <- function(path = "clipboard", copy2clip = interactive(), fill = "_"){
    if (length(path) == 1 && path == "clipboard") {
        path <- read_clip()
    } 
    text <- text_fix(path)
    und <- function(x) { 
        gsub("'", "", gsub("\\s+", fill, x))
    } 
    x <- unlist(lapply(tolower(text), und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}

#' Path/URL: Fill Blank Spaces
#'
#' \code{PF} - A convenience function to replace black spaces of URLs with 
#' percents.
#' 
#' @export
#' @rdname path_fill
PF <- function(path = "clipboard", copy2clip = interactive(), fill = "%"){
    if (length(path) == 1 && path == "clipboard") {
        path <- read_clip()
    } 
    text <- text_fix(path)
    und <- function(x) { 
        gsub("'", "", gsub("\\s+", fill, x))
    } 
    x <- unlist(lapply(text, und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}

## Remove in next update of reports:
#' Path/URL: Fill Blank Spaces
#' 
#' \code{US} - A convenience function to replace black spaces of paths with 
#' underscores. 
#'
#' @export
#' @section Notice: \code{US} is deprecated and will be removed from reports in 
#' a version subsequent to version 0.2.0  Please use \code{UF} instead.
#' @rdname path_fill
US <- function(path = "clipboard", copy2clip = interactive(), fill = "_"){
    if (length(path) == 1 && path == "clipboard") {
        path <- read_clip()
    } 
    text <- text_fix(path)
    und <- function(x) { 
        gsub("'", "", gsub("\\s+", fill, x))
    } 
    x <- unlist(lapply(tolower(text), und)) 
    if(copy2clip){
        write_clip(x)
    }
    return(x)
}
