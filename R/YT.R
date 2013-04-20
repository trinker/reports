#' YouTube url to iframe HTML Tag
#' 
#' Wrap a YouTube tag or url to generate an HTML iframe tag. 
#' 
#' @param text character vector url/tag copied to the clipboard. Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @return Returns a character vector of an HTML iframe tag that embeds a YouTube 
#' video.
#' @export
#' @examples
#' ## YT("kws1PX1Dw9w")
#' ## YT("http://www.youtube.com/watch?v=kws1PX1Dw9w")
YT <- function(text = "clipboard", copy2clip = TRUE) { 
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
    yt1 <- strsplit(text, "v=")
    yt2 <- strsplit(sapply(yt1, function(x) x[length(x)]), "&")
    tags <- sapply(yt2, function(x) x[1])
    x <- paste0("<iframe class=\"youtube-player\" type=\"text/html\" width=\"640\" height=\"360\" src=\"http://www.youtube.com/embed/", 
        tags, "?autoplay=0", "\" frameborder=\"0\"></iframe>")
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
    return(x)
} 