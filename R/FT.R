#' Wrap Text With Font Tag
#' 
#' Wraps text with a font tags. Conveniently detects c(`face`, `size` and/or 
#' `color`) and creates a font tag with the supplied text.
#'
#' @param \\ldots 1 to 3 arguments of c(`face`, `size` and/or `color`).  face 
#' accepts one of the following c("arial", "arial_black", "comic_sans_ms", 
#' "courier", "courier_new", "georgia", "helvetica", "impact", "palatino", 
#' "times_new_roman", "trebuchet_ms", "verdanaBrowse").  size is any valid whole 
#' number.  color can be any R color or hex value.
#' @param text character vector or text copied to the clipboard.  Default is to 
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
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (length(text) == 1 && text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- readClipboard()
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
    text <- gsub("([\\?])([aeiouy])", "\\fl\\2", text)
    text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
    nligs <- length(ligs)
    if (ligs[1] > 0) {
        plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
        warning(paste(nligs, "possible", plural, "found: \nCheck output!"))
    }
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
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(x, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {           
            j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)                                    
        }             
    }
    return(noquote(x))
} 