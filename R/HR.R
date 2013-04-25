#' Convert path/url to HTML href Tag
#' 
#' Wrap a path/url to generate an HTML href tag.
#' 
#' @param text character vector of text to hyperref from.  Defualt uses the 
#' \code{\link[base]{basename}} of the path.
#' @param path character vector url/path copied to the clipboard. Default is to 
#' read from the clipboard.  Note that Windows users do not have to reorient 
#' slashes in local paths if reading from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If FALSE returns to the console.
#' @return Returns a character vector of an HTML href tag. 
#' @export
#' @examples
#' ## HR("assets/img/fry_admin_1.mp4","new")
#' 
#' HR(path="http://dl.dropbox.com/u/61803503/Likert.pdf", print = TRUE)
#' HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", print = TRUE)
#' HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", "click me", print = TRUE)
HR <- function(path = "clipboard", text = NULL, copy2clip = TRUE, 
    print = FALSE) {
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
    if (is.null(text)) {
        text <- basename(path)
    }
    x <- paste0("<a href=\"", path, "\">", text, "</a>\n")
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

