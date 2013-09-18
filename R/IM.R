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
#' @param center logical. If \code{TRUE} the image will be centered, if 
#' \code{FALSE} image will be left justified.
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
#' cat(IW("http://www.talkstats.com/images/misc/logo.png", "http://www.talkstats.com/", 
#'     width=140, height=75), rep("So much text! ", 100))
IM <- function(path = "clipboard", link = NULL, width = 540,  
	height = IE(width, round(width/1.5)), sty = IE(width, width*1.05, 480), 
    center = TRUE, new_win = TRUE, copy2clip = TRUE, print = FALSE) { 
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
            tar <- " target=\"_blank\""	
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
#' This allows the users to just specify the image name that resides in one of 
#' the following directories: 1-"\code{~/assets/img}" or 2-"\code{~/figure}".
#' 
#' @param image A character vector name of the image. Default is to read from the 
#' clipboard. 
#' @param loc The location of the image: 1-/assets/img and 2-/figure.
#' @param \ldots Other arguments passed to \code{IM}. 
#' @export
#' @rdname image
IM2 <-
function(image = "clipboard", loc = 1, ...) { 
    if (image == "clipboard") {
        image <- read_clip()
    } 
    switch(loc, 
        `1` = {path <- paste("assets/img", chartr("\\", "/", image), sep="/")},
        `2` = {path <- paste("figure", chartr("\\", "/", image), sep="/")},
        stop("enter:\n  1-assets/img\n  2-figure\nfor location of image file")
    )
    IM(path = path, ...)
}
## Removed abs file path let's try it out for a while
## IM2 <-
## function(image = "clipboard", loc = 1, ...) { 
##     if (image == "clipboard") {
##         image <- read_clip()
##     } 
##     switch(loc, 
##         `1` = {path <- file.path("assets/img", chartr("\\", "/", image))},
##         `2` = {path <- file.path("figure", chartr("\\", "/", image))},
##         stop("enter:\n  1-assets/img\n  2-figure\nfor location of image file")
##     )
##     IM(path = path, ...)
## }


#' Convert path/url to HTML Image Tag
#'
#' \code{IW} - Text wrapped images.
#' 
#' @param side The side the image should appear on c(\code{"left"}, \code{"right"}). 
#' @param top Space between top margin and top of text.
#' @param right Space on the right margin.
#' @param left Space on the left margin..
#' @param bottom Space between bottom margin and bottom of text (0 is defualt).
#' @export
#' @rdname image
IW <- 
function(path = "clipboard", link = NULL, side = "right", width = 540, 
    height = IE(width, round(width/1.5)), new_win = TRUE, top = -15, right = 20, 
    left = 20, bottom = 0, copy2clip = TRUE, print = FALSE) { 
    if (path == "clipboard") {
        path <- read_clip()
    } 
    if(!is.null(height)){
        height <- paste0(" ", "height=\"", height, "\"")
    }
    if(!is.null(width)) {
        width <- paste0(" ", "width=\"", width, "\"")
    }
    path <- chartr("\\", "/", path)
    margs <- paste(paste0(c(top, right, bottom, left), "px"), collapse = " ")
    A <- paste0("<div style=\"float:", side, ";margin:", margs, ";\">")
    C <- "</div>"
    B <- paste0("<img src=\"", path, "\"", width, height, ">")
    if (!is.null(link)) {
        if (new_win) {
            tar <- " target=\"_blank\""  
        } else {
            tar <- NULL
        }
        if (!is.null(link)) {       
            B <- paste0("    <a href=\"", link, "\"", tar, ">", B, "</a>")
        }
    }
    x <- paste(c(A, B, C), collapse="\n")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}


#' Convert path/url to HTML Image Tag
#'
#' \code{IMF} - A wrapper for \code{IM} that attempts to find the image in expected 
#' image directories.  Uses the first available image matching the image name.  No 
#' path/url is specified.  IMS stands for `image locate`.
#' 
#' @export
#' @rdname image
IMF <- 
function(image = "clipboard", ...) { 
	
    if (image == "clipboard") {
        image <- read_clip()
    } 
    if (any(basename(getwd()) %in% c("REPORT", "PRESENTATION"))) {
        root <- dirname(getwd())		
    }    else {
        root <- getwd()	
    }
    fls <- file.path(root, c("figure", "PRESENTATION/assets/img",
        file.path(c("REPORT", "PRESENTATION"), "figure")))
    fls2 <- sapply(fls, file.exists)
    
    if (sum(fls2) == 0) stop("no image directories found")
       
    fls <- fls[fls2]
    imgs <- file.path(fls, image)
    imgs2 <- sapply(imgs, file.exists)	
    if (sum(imgs2) == 0)  stop(sprintf("no files match \"%s\"", image))

    IM(path = imgs[imgs2][1], ...)
}
