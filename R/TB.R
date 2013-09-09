#' Wrap text in text boxes.
#' 
#' \code{TB} - Generates an HTML textbox tag.
#' 
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param col The color(s) to fill or shade the rectangle with.
#' @param border The color for rectangle border(s).
#' @param padding The distance (in px) between the text and the border.
#' @param lty The line type for borders (either "solid" or "dashed"). 
#' @param lwd The line width (in px) for borders and shading.
#' @param bor.rad The degree (in px) to which the corners are rounded; 0 results 
#' in square corners.
#' @param \ldots Other arguments passed to \bold{style} in the HTML \code{div} 
#' tag.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @param print logical.  If TRUE \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console.
#' @return Returns a character vector of an HTML text box tag. 
#' @export
#' @rdname TB
#' @examples
#' TB("I like ice cream!", print=TRUE)
#' TB("Free cookies for a year!", print=TRUE)
#' TB2("I like ice cream!", print=TRUE)
#' TB2("Free cookies for a year!", print=TRUE)
TB  <-
function(text = "clipboard", col = "#FFFFCC", border = "black", padding = 10, 
    lty = "solid", lwd = 1, bor.rad = 5, ..., copy2clip = TRUE, print = FALSE) {

    if (text == "clipboard") {
        text <- read_clip()
    }

    sty <- sprintf("background-color: %s; border-radius: %spx; padding:%spx; border: %spx %s %s;",
        col, bor.rad, padding, lwd, lty, border)

    style <- paste(sty, ...)

    x <- sprintf("<div style=\"%s\">\n    %s\n</div>", style, text)

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

#' Wrap text in text boxes.
#' 
#' \code{TB2} - Generates an HTML writeable textbox tag.
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



