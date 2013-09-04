#' Convert Windows Path
#' 
#' Reorients Windows path backslashes to forward slashes.
#' 
#' @param path A character vector url/path copied to the clipboard. Default is 
#' to read from the clipboard.  
#' @param quotes logical.  If \code{TRUE} quotes will be added to front/end of 
#' the string.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @return Returns a character vector path with slashes oriented for R.
#' @export
#' @examples
#' ## WP(FALSE)  #using readline <br>    
#' ## C:\Users\trinker\Desktop\doc
WP <- 
function(path = "clipboard", quotes = TRUE, copy2clip = TRUE) {
    if (path != "clipboard") {
        message("Please enter the path:\n\n")
        path <- readline()
    } else {
        path <- read_clip()
    }
    z <- chartr("\\", "/", path)
    if (quotes) {
        x <- paste0("\"", z, "\"")
    } else {
        x <- z
    }
    if(copy2clip){
        write_clip(x)
    }
    return(z)
}

