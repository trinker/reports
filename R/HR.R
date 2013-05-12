#' Convert path/url to HTML href Tag
#' 
#' \code{HR} - Wrap a path/url to generate an HTML href tag.
#' 
#' @param path A character vector url/path copied to the clipboard. Default is to 
#' read from the clipboard.  Note that Windows users do not have to reorient 
#' slashes in local paths if reading from the clipboard.
#' @param text A character vector of text to hyperref from.  Defualt uses the 
#' \code{\link[base]{basename}} of the path.
#' @param new_win logical.  If \code{TRUE} the link will open in a new window.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector of an HTML href tag. 
#' @export
#' @rdname hyperref
#' @examples
#' ## HR("assets/img/fry_admin_1.mp4","new")
#' 
#' HR(path="http://dl.dropbox.com/u/61803503/Likert.pdf", print = TRUE)
#' HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", print = TRUE)
#' HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", "click me", print = TRUE)
#' HR2("https://github.com/trinker/reports", "reports", print = TRUE)
#' BT("http://trinker.github.io/reports/dependencies", "Click Here!", print = TRUE)
HR <- function(path = "clipboard", text = NULL, new_win = FALSE, copy2clip = TRUE, 
    print = FALSE) {
    if (path == "clipboard") {
        path <- read_clip()
    } 
    path <- chartr("\\", "/", path)
    if (is.null(text)) {
        text <- basename(path)
    }
    if (new_win) {
        tar <- " target=\"_blank\""	
    } else {
    	tar <- NULL
    }
    x <- paste0("<a href=\"", path, "\"", tar, ">", text, "</a>\n")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
} 


#' Convert path/url to HTML href Tag
#'
#' \code{HR2} - Convenience version of \code{HR} with \code{new_win} set to \code{TRUE}.
#' 
#' @export
#' @rdname hyperref
HR2 <- function(path = "clipboard", text = NULL, new_win = TRUE, copy2clip = TRUE, 
    print = FALSE) {
    if (path == "clipboard") {
        path <- read_clip()
    } 
    path <- chartr("\\", "/", path)
    if (is.null(text)) {
        text <- basename(path)
    }
    if (new_win) {
        tar <- " target=\"_blank\""	
    } else {
    	tar <- NULL
    }
    x <- paste0("<a href=\"", path, "\"", tar, ">", text, "</a>\n")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
} 

#' Convert path/url to HTML href Tag
#'
#' \code{BT} - Wrap a path/url to generate an HTML hyperlinked text button tag.
#' 
#' @export
#' @rdname hyperref
BT <- 
function(path = "clipboard", text = NULL, new_win = FALSE, copy2clip = TRUE, 
    print = FALSE) {
    if (path == "clipboard") {
        path <- read_clip()
    } 
    path <- chartr("\\", "/", path)
    if (is.null(text)) {
        text <- basename(path)
    }
    if (new_win) {
        tar <- " target=\"_blank\""     
    } else {
        tar <- NULL
    }

    x <- paste0("<form action=\"", path, "\">\n", 
        paste0("    <input type=\"submit\" value=\"", text, "\">\n"), 
        "</form>\n")

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
