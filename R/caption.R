#' Captions for Figures and Tables
#' 
#' \code{caption} - Keep track of numbering for table and figure captions.
#' 
#' @param type Either "figure" or "table" or accompanying numeric reference:
#' figure = 1, table = 2.
#' @param label A hyperlink reference label.
#' @param caption A caption to place below/above a figure/table.
#' @param style Additional style elements to be passed to the html \code{p} tag.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE} returns to the console. 
#' @return Returns a character vector of a caption.
#' @keywords caption
#' @rdname caption
#' @export
#' @examples
#' caption_figure("fig1", "A random figure.")
#' caption_table("tab1", "A table")
cap <- function(type = 1, label, caption, style="margin-bottom: 3em;",
	copy2clip = interactive(), print = FALSE) {
	
	if (any(type %in% c(1, "figure"))) {
        caption_figure(label = label, caption = caption, style = style, 
        	copy2clip = copy2clip, print = print)		
	} else {
        if (any(type %in% c(2, "table"))) {
    		    caption_table(label = label, caption = caption, 
    		    	style = style, copy2clip = copy2clip, print = print)
    		
        } else {
        	stop("type must be \"figure\", \"table\", 1, or 2")
        }

	}	
}

#' Captions for Figures and Tables
#' 
#' \code{caption_figure} - Keep track of numbering for figure captions.
#' 
#' @export
#' @rdname caption
caption_figure <- function(label, caption, style = "margin-bottom: 3em;", 
    copy2clip = interactive(), print = FALSE) {

    err.out <- try(is.null(.fig), silent = TRUE)

    if (class(err.out) == "try-error") .fig <- 0
    .fig <<- .fig <- .fig + 1
	
    style <- ifelse(is.null(style), "", sprintf("style=\"%s\"", style))
    x <- sprintf("<p id=\"%s\" %s><em>Figure %s</em>: %s</p>", label, style, 
       .fig, caption)
	
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}

#' Captions for Figures and Tables
#' 
#' \code{caption_table} - Keep track of numbering for table captions.
#' 
#' @export
#' @rdname caption
caption_table <- function(label, caption, style = "margin-top: 4em;",
	copy2clip = interactive(), print = FALSE, spaces = 2) {

    err.out <- try(is.null(.tab), silent = TRUE)

    if (class(err.out) == "try-error") .tab <- 0
    .tab <<- .tab <- .tab + 1

	spaces <- paste(rep("<br>", spaces), collapse = "")
    style <- ifelse(is.null(style), "", sprintf("style=\"%s\"", style))
	
    x <- sprintf("<p id=\"%s\" %s>Table %s%s<em>%s</em></p>", label, 
        style, .tab, spaces, caption)
	
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
