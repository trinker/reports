#' Convert path/url to HTML Image Tag
#' 
#' Wrap a path/url to generate an HTML tag.  Often markup code: \code{![](url)} 
#' lacks flexibility with centering and sizing.  \code{IM} enables conrol of 
#' centering via altering the numeric value in \code{width:420px} and sizing via 
#' the numeric values supplied to height and width.
#' 
#' @param text character vector url/path copied to the clipboard. Default is to 
#' read from the clipboard.  Note that Windows users do not have to reorient 
#' slashes in local paths.
#' @param width the width of the image.
#' @param height the height of the image.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE prints the output to the console.
#' @return Returns a character vector of an HTML image tag that embeds an image. 
#' @export
#' @examples
#' IM("http://cran.r-project.org/Rlogo.jpg")
IM <- function(text = "clipboard", width = 400, height = 300, copy2clip = TRUE, 
    print = TRUE) { 
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (length(text) == 1 && text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- readClipboard()
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    text <- chartr("\\", "/", text)
    front <- "<div style=\"width:420px;margin:auto;\">\n    <p><img src=\""
    end <- paste0("\" width=\"", width, "\" height=\"", height, "\"></p>\n</div>\n")
    x <- paste0(front, text, end)
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
