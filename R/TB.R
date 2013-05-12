#' Wrap text in text boxes.
#' 
#' Wrap text to generate an HTML text box tag.
#' 
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param width The width, in characters, to make the box.
#' @param text_align Alingment of text in the box; takes the values c("left", 
#' "right", "top", "middle", "bottom").
#' @param box_align Alingment of textbox; takes the values c("left", "right", 
#' "top", "middle", "bottom").
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector of an HTML text box tag. 
#' @export
#' @examples
#' TB("I like ice cream!", print=TRUE)
#' TB("Free cookies for a year!", print=TRUE)
TB <- 
function(text = "clipboard", width = nchar(text), text_align="center", 
    box_align = "left", copy2clip = TRUE, print = FALSE) {

    if (text == "clipboard") {
        text <- read_clip()
    }
    a <- paste0("<div align=\"", box_align, "\">")
    b <- paste0("    <input style=\"text-align:", text_align, 
        "\" name=\"box1\" type=\"text\" value=\"", text, "\" size=\"", width, "\" />")
    d <- "</div>"

    x <- paste(a, b, d, sep="\n")

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}



