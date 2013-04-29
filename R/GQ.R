#' Format Quotes
#' 
#' \code{GQ} - Tool to grab quote and format the text taken from articles for LaTeX.  
#' Combines multiple stringed text into one string.  Removes non ascii 
#' characters and hyphens.
#' 
#' @param quotes logical or c(\code{l}, \code{r}, \code{L}, \code{R}, \code{left} 
#' or \code{right}).  If \code{TRUE} LaTeX style quotes (2 backticks and 
#' two single quotes) are wrapped around the text.  If (\code{l}, \code{L} or 
#' \code{left}) left ticks only are used. If (\code{r}, \code{R} or \code{right}) 
#' right ticks only are used. 
#' @param block logical.  If \code{TRUE} LaTeX block quote code tags are used 
#' instead of the backticks and single quotes.
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @param invisible logical.  If \code{TRUE} will not print to the console.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @return \code{GQ} - Returns formatted text for use with LaTeX documents. 
#' @rdname QQ
#' @export
#' @examples
#' x1 <- "Note Many functions in reports assume (a) you're using RStudio (b) the
#' main report directory is set as the working directory. While
#' the default behavior is less flexible it increases efficiency."
#' GQ(text=x1)
#' QQ(x1)
GQ <- 
function(quotes = TRUE, block = TRUE, text = "clipboard", copy2clip = TRUE, 
    invisible = FALSE){
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    text <- QC(text=text_fix(text), from="markdown", to="latex") 
    quotes <- as.character(substitute(quotes))
    if (!quotes %in% c("FALSE", "F")) {
        if (quotes %in% c("l", "L", "left")){
            R <- ""
        } else {
            R <- "''"         
        }
        if (quotes %in% c("r", "R", "right")){
            L <- ""
        } else {
            L <- "``"         
        }
        LONG <- FALSE
        if (wc(text) > 39  & block) {
            LONG <- TRUE
            L <- "\\begin{quote}\n"
            R <- "\n\\end{quote}"
        }
        x <- paste0(L, text, R)
    } else {
        LONG <- FALSE   
        x <- text       
    }
    if(copy2clip){
        write_clip(x)
    }
    xout <- strWrap(text, copy2clip = FALSE, invisible=TRUE)
    if (!invisible) {
        if (LONG) {
            body <- paste0(paste(paste(" ", xout), collapse="\n"), "\n")
            cat("\\begin{quote}\n"); cat(body)
            cat("\\end{quote}\n")
        } else {
            body <- paste0("``", paste2(xout), "''\n")
            cat(body)
        }
    }
    invisible(x)
}


#' Format Quotes
#' 
#' \code{QQ} - Tool to grab a quote and format the text taken from articles for 
#' use in the directory notes.xslx or notes.csv.  Combines multiple stringed 
#' text into one string.  Removes non ascii characters and hyphens.
#' 
#' @return \code{QQ} - Returns formatted text for use with notes.xslx or 
#' notes.csv. 
#' @rdname QQ
#' @export
QQ <- 
function(text = "clipboard", copy2clip = TRUE){
    if (length(text) == 1 && text == "clipboard") {
        text <- read_clip()
    } 
    x <- text_fix(text)
    if(copy2clip){
        write_clip(x)
    }
    strWrap(x, copy2clip = FALSE)
    invisible(x)
}
