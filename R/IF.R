#' url to iframe HTML Tag
#' 
#' Wrap a url to generate an HTML iframe tag. 
#' 
#' @param path A character vector url/tag copied to the clipboard. Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param width The width of the player.
#' @param height The height of the player.
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE}returns to the console. 
#' @param text character vector of text to display for browser that do not support 
#' iframe tags.
#' @return Returns a character vector of an HTML iframe tag that embeds a document.
#' @export
#' @examples
#' IF("https://dl.dropboxusercontent.com/u/61803503/MWE.html", print = TRUE)
IF <- function(path = "clipboard", copy2clip = TRUE, width = 640, height = 360, 
    print = FALSE, text = "Your browser does not support iframes.") {
    if (path == "clipboard") {
        path <- read_clip()
    } 

    A <- "<iframe src=\""
    B <- "</iframe>"

    extras <- NULL
    if (!is.null(width)) {
        extras <- paste0(" width=\"", width, "\"")
    }
    if (!is.null(height)) {
        extras <- paste0(extras, " height=\"", height, "\"")
    }

    x <- paste0(A, path, "\"", extras, ">", text, B)
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

