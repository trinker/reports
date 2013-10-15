#' Convert Windows Path
#' 
#' Reorients Windows path backslashes to forward slashes.
#' 
#' @param from.clip logical.  If \code{TRUE} path is read from the clipboard. 
#' If \code{FALSE} user inputs path inteactively. 
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
function(from.clip = TRUE, quotes = TRUE, copy2clip = TRUE) {
    if (!from.clip) {
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

