#' Wrap text in text boxes.
#' 
#' \code{TB} - Generates an HTML text box tag.
#' 
#' @param text A character vector or text copied to the clipboard.  Default is 
#' to read from the clipboard.
#' @param \ldots Other arguments passed to \bold{style} in the HTML \code{div} 
#' tag.
#' @param col The color(s) to fill or shade the rectangle with.
#' @param border The color for rectangle border(s).
#' @param padding The distance (in px) between the text and the border.
#' @param lty The line type for borders (either \code{"solid"} or 
#' \code{"dashed"}). 
#' @param lwd The line width (in px) for borders and shading.
#' @param bold logical.  If \code{TRUE} the font will be boldfaced.
#' @param font.col The color of the font.
#' @param bor.rad The degree (in px) to which the corners are rounded; 0 results 
#' in square corners.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector of an HTML text box tag. 
#' @export
#' @rdname TB
#' @examples
#' TB("I like ice cream!", print=TRUE)
#' TB("Free cookies for a year!", col = "red", font.col="white", print=TRUE)
#' TB("Bad Robot!", "font-style:italic", print=TRUE)
#' TB2("I like ice cream!", print=TRUE)
#' TB2("Free cookies for a year!", print=TRUE)
TB  <-
function(text = "clipboard", ..., col = "white", border = "black", padding = 10, 
    lty = "solid", lwd = 1, bor.rad = 5, bold = FALSE, font.col = "black", 
    copy2clip = interactive(), print = FALSE) {

    if (text == "clipboard") {
        text <- read_clip()
    }

    bold <- ifelse(bold, "font-weight:bold;", "")
    
    sty <- sprintf("background-color: %s; border-radius: %spx; padding:%spx; border: %spx %s %s; color: %s;",
        col, bor.rad, padding, lwd, lty, border, font.col)

    style <- paste(sty, bold, ...)

    x <- sprintf("<div style=\"%s\">\n    %s\n</div>", style, text)

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

#' Wrap text in text boxes.
#' 
#' \code{TB2} - Generates an HTML writable text box tag.
#' 
#' @param width The width, in characters, to make the box.
#' @param text_align Alingment of text in the box; takes the values 
#' c(\code{"left"}, \code{"right"}, \code{"top"}, \code{"middle"}, 
#' \code{"bottom"}).
#' @param box_align Alingment of textbox; takes the values c(\code{"left"}, 
#' \code{"right"}, \code{"top"}, \code{"middle"}, \code{"bottom"}).
#' @export
#' @rdname TB
TB2 <- 
function(text = "clipboard", width = nchar(text), text_align="center", 
    box_align = "left", copy2clip = interactive(), print = FALSE) {

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



