#' Format Text Lines to LaTeX List
#' 
#' Itemizes lines of text into a LaTeX list.   
#' 
#' @param enumerate logical.  If \code{TRUE} uses the enumerate environment.  If 
#' \code{FALSE} itemize is used instead.
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This function formats text for use with LaTeX as a list.  
#' @return Returns a charcter vector with a LaTeX list formatted text.
#' @export
#' @examples
#' \dontrun{
#' x <- readLines(n = 3)
#' one, two buckle my shoe
#' three, four close the door
#' five, six pick up sticks
#' LL(, x)
#' LL(FALSE, x)
#' }
LL <- latexlist <- function(enumerate=TRUE, text = "clipboard", 
	copy2clip = TRUE) {
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
    if (enumerate) {
        x <- "enumerate"
    } else {
        x <- "itemize"
    }
    z <- paste("  \\item ", text, sep ="")
    zz <- as.matrix(unlist(list(
        paste0("\\begin{", x, "}"),
        z,
        paste0("\\end{", x, "}"))), 
    ncol=1)
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
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    return(noquote(zz))
}
