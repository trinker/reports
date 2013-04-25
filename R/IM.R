#' Convert path/url to HTML Image Tag
#' 
#' Wrap a path/url to generate an HTML tag.  Often markup code: \code{![](url)} 
#' lacks flexibility with centering and sizing.  \code{IM} enables conrol of 
#' centering via altering the sty/center commands and control of sizing via 
#' the numeric values supplied to height and width.
#' 
#' @param path character vector url/path to the image. Default is to 
#' read from the clipboard.  Note that Windows users do not have to reorient 
#' slashes in local paths if reading from the clipboard.
#' @param width the width of the image.
#' @param height the height of the image.
#' @param sty the width of the style (used for centering).
#' @param center logical.  If TRUE the image will be centered, if FALSE image 
#' will be left justified.
#' @param link character vector url/path to hyperlink the image to.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If FALSE returns to the console.
#' @return Returns a character vector of an HTML image tag that embeds an image. 
#' @export
#' @examples
#' IM("http://cran.r-project.org/Rlogo.jpg", print=TRUE)
#' IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print =TRUE)
#' IM("http://cran.r-project.org/Rlogo.jpg", print=TRUE, link = "http://cran.r-project.org")
IM <- function(path = "clipboard", width = 400, height = 300, sty = width*1.05, 
    center = TRUE, link = NULL, copy2clip = TRUE, print = FALSE) { 
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (length(path) == 1 && path == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            path <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            path <- readClipboard()
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    path <- chartr("\\", "/", path)
    front <- paste0("<div style=\"width:", sty, "px;margin:auto;\">\n    <p><img src=\"")
    end <- paste0("\" width=\"", width, "\" height=\"", height, "\"></p>\n</div>\n")
    if (center & is.null(link)) {
        x <- paste0(front, path, end)
    } else {
        x <- paste0("<img src=\"", path, "\" width=\"", width, "\" height=\"", height, "\">")
    }
    if (!is.null(link)) {
        x <- paste0("<a href=\"", link, "\">", x, "</a>")
        if (center) {
            x <- paste0("<div style=\"width:", sty, "px;margin:auto;\">\n    <p>", x,
                "</p>\n</div>\n")
        } else {
            x <- paste0(x, "\n") 
        }
    }
    if(copy2clip){
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(x, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {           
            j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)                                    
        }             
    }
    if (print) {
        cat(x)
        invisible(x)
    } else {
        x	
    }
}
