#' Format Text Lines to LaTeX List
#' 
#' Itemizes lines of text into a LaTeX list.   
#' 
#' @param enumerate logical.  If \code{TRUE} uses the enumerate environment.  If 
#' \code{FALSE} itemize is used instead.
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This function formats text for use with LaTeX as a list.  
#' @return Returns a character vector with a LaTeX list formatted text.
#' @section Warning: Ligatures parsing is very good, however, these elements may
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @export
#' @examples
#' x <- c(
#' "one, two buckle my shoe",
#' "three, four close the door",
#' "five, six pick up sticks")
#' LL(, x)
#' LL(FALSE, x)
LL <- latexlist <- function(enumerate=TRUE, text = "clipboard", 
	copy2clip = interactive()) {
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    text <- sapply(text, text_fix)
    if (enumerate) {
        x <- "enumerate"
    } else {
        x <- "itemize"
    }
    z <- paste("  \\item ", text, sep ="")
    zz <- as.matrix(unlist(list(paste0("\\begin{", x, "}"), z,
        paste0("\\end{", x, "}"))), ncol=1)
    if(copy2clip){
        write_clip(zz)
    }
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    return(noquote(zz))
}
