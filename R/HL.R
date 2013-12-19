#' Highlight HTML Text
#' 
#' Wraps text with a background color specific font tags. 
#'
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param color A character string taken from R's built-in color names or a 
#' hexidecimal color.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console.
#' @export
#' @examples
#' cat(HL("Do not trust robots!"), "They are bent on destruction.")
#' cat(HL("Jake is a cookie scientist,", color="pink"), "an honrable profession.")
HL <-
function(text = "clipboard", color = "yellow", copy2clip = interactive(), 
	print = FALSE) { 
	
    if (text == "clipboard") {
        text <- read_clip()
    }
    a <- "<font style=\"background-color: "
    if (!grepl("#", color)) {
        color <- col2hex(color)
    }
    b <- ";\">"
    d <- "</font>"
    x <- paste0(a, color, b, text, d)

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

