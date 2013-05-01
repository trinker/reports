#' Wrap Text With HTML Font Tag
#' 
#' Wraps text with a font tags. Conveniently detects c(`face`, `size` and/or 
#' `color`) and creates a font tag with the supplied text.
#'
#' @param \ldots 1 to 3 arguments of c(`face`, `size` and/or `color`).  face 
#' accepts one of the following c("arial", "arial_black", "comic_sans_ms", 
#' "courier", "courier_new", "georgia", "helvetica", "impact", "palatino", 
#' "times_new_roman", "trebuchet_ms", "verdanaBrowse").  size is any valid whole 
#' number.  color can be any R color or hex value.
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @return Returns a character vector wrapped with a font tag.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @export
#' @examples
#' FT(6, text="guy")
#' FT(6, blue, text="guy")
#' FT(6, red, times_new_roman, text="guy")
FT <- function(..., text = "clipboard", copy2clip = TRUE) { 
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    text <- text_fix2(text)
    x <- substitute(...())
    if (is.null(x)) {
        stop(paste("supply \"font\", \"size\", and/or \"color\"", 
            "argument(s) to ldots"))
    }
    x <- unlist(lapply(x, as.character))
    params <- c()
    numCheck <- !is.na(suppressWarnings(as.numeric(x)))
    if (any(numCheck)) {
        params <- c(params, paste0("size=\"", x[numCheck][1], "\""))
    } 
    cols <- c(colors(), rgb(t(col2rgb(colors())), maxColorValue=255))
    colCheck <- cols %in% x
    if (sum(colCheck) > 0){
        params <- c(params, paste0("color=\"", cols[colCheck][1], "\""))
    }
    faces <- c("arial", "arial_black", "comic_sans_ms", "courier", 
        "courier_new", "georgia", "helvetica", "impact", "palatino", 
        "times_new_roman", "trebuchet_ms", "verdanaBrowse")
    faceCheck <- faces %in% x
    if (sum(faceCheck) > 0){
        params <- c(params, paste0("face=\"", gsub("_", " ", 
            faces[faceCheck][1]), "\""))
    }
    if (is.null(params)) stop("supply a valid face, color or size to ldots")
    params <- paste(params, collapse=" ")    
    x <- paste("<font",  paste0(paste0(params, ">"), 
        paste0(text, "</font>")), collapse="")
    if(copy2clip){
        write_clip(x)
    }
    return(noquote(x))
} 
