#' Convert path/url to HTML Image Tag
#' 
#' \code{IM} - Wrap a path/url to generate an HTML tag.  Often markup code: 
#' \code{![](url)} lacks flexibility with centering and sizing.  \code{IM} 
#' enables conrol of centering via altering the sty/center commands and control 
#' of sizing via the numeric values supplied to height and width.
#' 
#' @param path A character vector url/path to the image. Default is to 
#' read from the clipboard.  Note that Windows users do not have to reorient 
#' slashes in local paths if reading from the clipboard.
#' @param width The width of the image.  If \code{NULL} the defualt image width 
#' is used.
#' @param height The height of the image.  If \code{NULL} the defualt image 
#' height is used.
#' @param sty The width of the style (used for centering).
#' @param center logical.  If \code{TRUE} the image will be centered, if FALSE image 
#' will be left justified.
#' @param link character vector url/path to hyperlink the image to.
#' @param new_win logical.  If \code{TRUE} the link will open in a new window.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the output to the 
#' console.  If FALSE returns to the console.
#' @return Returns a character vector of an HTML image tag that embeds an image. 
#' @export
#' @rdname image
#' @examples
#' IM("http://cran.r-project.org/Rlogo.jpg", width= NULL, print=TRUE)
#' IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print =TRUE)
#' IM("http://cran.r-project.org/Rlogo.jpg", NULL, print=TRUE, link = "http://cran.r-project.org")
IM <- function(path = "clipboard", width = 540, height = IE(width, 360), 
	sty = IE(width, width*1.05, 480), center = TRUE, link = NULL, new_win = TRUE,
    copy2clip = TRUE, print = FALSE) { 
    if (path == "clipboard") {
        path <- read_clip()
    } 
    path <- chartr("\\", "/", path)
    sty <- sty 
    front <- paste0("<div style=\"width:", sty, "px;margin:auto;\">\n    <p><img src=\"")
    if(!is.null(height)){
    	height <- paste0(" ", "height=\"", height, "\"")
    }
    if(!is.null(width)) {
    	width <- paste0(" ", "width=\"", width, "\"")
    }
    end <- paste0(width, height, "></p>\n</div>\n")
    if (center & is.null(link)) {
        x <- paste0(front, path, "\"", end)
    } else {
        x <- paste0("<img src=\"", path, "\"", width, height, ">")
    }
    if (!is.null(link)) {
        if (new_win) {
            tar <- "target=\"_blank\""	
        } else {
            tar <- NULL
        }    	
        x <- paste0("<a href=\"", link, "\"", tar, ">", x, "</a>")
        if (center) {
            x <- paste0("<div style=\"width:", sty, "px;margin:auto;\">\n    <p>", x,
                "</p>\n</div>\n")
        } else {
            x <- paste0(x, "\n") 
        }
    }
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

#' Convert path/url to HTML Image Tag
#'
#' \code{IM2} - A wrapper for \code{IM} that sets the base path to "assets/img/".  
#' This allows the users to just specify the image name that resides in the 
#' directory: "assets/img/".
#' 
#' @param image A character vector name of the image. Default is to read from the 
#' clipboard. 
#' @param \ldots Other arguments passed to \code{IM}. 
#' @export
#' @rdname image
IM2 <- function(image = "clipboard", ...) { 
    if (image == "clipboard") {
        image <- read_clip()
    } 
    path <- file.path("assets/img", chartr("\\", "/", image))
    IM(path = path, ...)
}
