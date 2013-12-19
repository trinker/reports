#' Captions for Figures and Tables
#' 
#' \code{caption} - Keep track of numbering for table and figure captions.
#' 
#' @param type Either "figure" or "table" or accompanying numeric reference:
#' figure = 1, table = 2.
#' @param label A hyperlink reference label.
#' @param caption A caption to place below/above a figure/table.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE} returns to the console. 
#' @return Returns a character vector of a caption.
#' @keywords caption
#' @rdname caption
#' @export
#' @examples
#' \dontrun{
#' caption_figure("fig1", "A random figure.")
#' caption_table("tab1", "A table")
#' }
caption <- function(type = 1, label, caption, copy2clip = interactive(), 
	print = FALSE) {
	
	if (any(type %in% c(1, "figure"))) {
        caption_figure(label = label, caption = caption, copy2clip = copy2clip, 
        	print = print)		
	} else {
        if (any(type %in% c(2, "table"))) {
    		    caption_table(label = label, caption = caption, 
    		    	copy2clip = copy2clip, print = print)
    		
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
caption_figure <- function(label, caption, copy2clip = interactive(), 
	print = FALSE) {

    if (is.null(.fig)) .fig <- 0
    .fig <<- .fig <- .fig + 1
	
    x <- sprintf("<p id=\"%s\"><em>Figure %s</em>: %s</p>", label, .fig, caption)
	
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
caption_table <- function(label, caption, copy2clip = interactive(), 
	print = FALSE) {

    if (is.null(.tab)) .tab <- 0
    .tab <<- .tab <- .tab + 1
	
    x <- sprintf("<p id=\"%s\">Table %s<br><em>%s</em></p>", label, .tab, caption)
	
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
