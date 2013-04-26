#' Convert Windows Path
#' 
#' Reorients Windows path backslashes to forward slashes.
#' 
#' @param clipboard logical.  If TRUE copies the path from the clipboard, 
#' if FALSE \code{WP} will ask for the path interactively.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @return Returns a character vector path with slashes oriented for R.
#' @export
#' @examples
#' ## WP(FALSE)  #using readline <br>    
#' ## C:\Users\trinker\Desktop\doc
WP <- 
function(clipboard = TRUE, quotes = TRUE, copy2clip = TRUE) {
    if (!clipboard) {
        cat("Please enter the path:\n\n")
        text <- readline()
    } else {
        if (Sys.info()["sysname"] != "Windows") {
            writeClipboard <- NULL
        }  
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- paste(scan(pcon, what="character", 
                quiet=TRUE), collapse=" ")
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(readClipboard(), collapse=" ")
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    }
    z <- chartr("\\", "/", text)
    if (quotes) {
        x <- paste0("\"", z, "\"")
    } else {
        x <- z
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
    return(z)
}

