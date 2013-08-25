#' url to HTML iframe Tag
#' 
#' Wrap a url to generate an HTML iframe tag. 
#' 
#' @param path A character vector url/tag copied to the clipboard. Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param width The width of the iframe.
#' @param height The height of the iframe.
#' @param center logical. If \code{TRUE} the image will be centered, if 
#' \code{FALSE} image will be left justified.
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE}returns to the console. 
#' @param text character vector of text to display for browser that do not support 
#' iframe tags.
#' @return Returns a character vector of an HTML iframe tag that embeds a document.
#' @references 
#' \url{http://www.w3schools.com/tags/tag_iframe.asp}
#' @export
#' @examples
#' IF("https://dl.dropboxusercontent.com/u/61803503/MWE.html", print = TRUE)
#' IF("http://www.online-stopwatch.com/countdown-clock/full-screen/", 
#'   height=400, center = TRUE, print = TRUE)
#' IF("http://www.dosketch.com/", width=1100, height=650, print=TRUE)
#' IF("http://glimmer.rstudio.com/pssguy/TVShowRatings/", width="100%", 
#'   height=650, print=TRUE)
#' IF("https://dl.dropboxusercontent.com/u/61803503/Slides/reports/index.html", 
#'   width=1150, height=750, print=TRUE)
#' IF("https://dl.dropboxusercontent.com/u/61803503/presentations/tmp.html", 
#'   width=770, height=680, print=TRUE)
IF <- function(path = "clipboard", copy2clip = TRUE, width = 640, height = 360, 
    center = TRUE, print = FALSE, text = "Your browser does not support iframes.") {
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

    if (center) {
        x <- paste0("<div style=\"text-align:center;\">\n ", 
            paste0("    ", x, "\n"), "</div>\n")
    } 

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

