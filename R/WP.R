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
function(text = "clipboard", quotes = TRUE, copy2clip = TRUE) {
    if (text != "clipboard") {
        cat("Please enter the path:\n\n")
        text <- readline()
    } else {
        text <- read_clip()
    }
    z <- chartr("\\", "/", text)
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

