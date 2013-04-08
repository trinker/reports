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
#' @param block If \code{TRUE} LaTeX block quote code tags are used instead of 
#' the backticks and single quotes.
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @param invisible logical.  If TRUE will jot print to the console.
#' @section Warning: Ligatures parsing is very good, however, these elements my
#' be incorrect.  If a warning is thrown check the use of "ff", "fi", "fl", 
#' "ffi" and "ffl".
#' @return \code{GQ} - Returns formatted text for use with LaTeX documents. 
#' @rdname QQ
#' @return Returns a character vector with LaTeX formatted text.
#' @export
GQ <- 
function(quotes = TRUE, block = TRUE, text = "clipboard", copy2clip = TRUE, 
    invisible = FALSE){
    if (Sys.info()["sysname"] != "Windows") {
        readClipboard <- writeClipboard <- NULL
    }    
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- paste(scan(pcon, what="character", 
                quiet=TRUE), collapse=" ")
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(readClipboard(), collapse=" ")
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    text <- clean(paste2(text, " "))
    ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
    text <- gsub("([\\?])([aeiouy])", "\\fl\\2", text)
    text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
    nligs <- length(ligs)
    if (ligs[1] > 0) {
        plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
        warning(paste(nligs, "possible", plural, "found: \nCheck output!"))
    }
    text <- Trim(iconv(text, "", "ASCII", "byte"))
    ser <- c("<91>", "<92>", "- ", "<93>", "<94>", "<85>", "<e2><80><9c>", "<e2><80><9d>", 
        "<e2><80><98>", "<e2><80><99>", "<e2><80><9b>", "<ef><bc><87>", 
    	"<e2><80><a6>", "<e2><80><93>", "<e2><80><94>", "<c3><a1>", "<c3><a9>", 
    	"<c2><bd>", "<eb>")
    reps <- c("`", "'", "", "``", "''", "\\ldots", "", "", "'", "'", "'", "'", "\\ldots", 
        "\\textendash", "\\textemdash", "a", "e", "half", "\\\"a")
    Encoding(text) <-"latin1"
    text <- clean(mgsub(ser, reps, text))
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
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(x, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {           
            j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)                                    
        }             
    }
    xout <- strWrap(text, copy2clip = FALSE, invisible=TRUE)
    if (!invisible) {
        if (LONG) {
    	    body <- paste0(paste(paste(" ", xout), collapse="\n"), "\n")
            cat("\\begin{quote}\n"); cat(body)
            cat("\\end{quote}\n")
        } else {
        	body <- paste0(paste(xout, collapse="\n"), "\n")
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
#' @return \code{GQ} - Returns formatted text for use with notes.xslx or 
#' notes.csv. 
#' @rdname QQ
#' @export
QQ <- 
function(text = "clipboard", copy2clip = TRUE){
    if (Sys.info()["sysname"] != "Windows") {
        readClipboard <- writeClipboard <- NULL
    }  
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- paste(scan(pcon, what="character", 
                quiet=TRUE), collapse=" ")
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(readClipboard(), collapse=" ")
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    text <- clean(paste2(text, " "))
    ligs <- gregexpr("([\\?])([a-z])", text)[[1]]
    text <- gsub("([\\?])([aeiouy])", "\\fl\\2", text)
    text <- gsub("([\\?])([a-z])", "\\fi\\2", text)
    nligs <- length(ligs)
    if (ligs[1] > 0) {
        plural <- ifelse(nligs > 1, "ligatures were", "ligature was")
        warning(paste(nligs, "possible", plural, "found: \nCheck output!"))
    }
    text <- Trim(iconv(text, "", "ASCII", "byte"))
    ser <- c("<91>", "<92>", "- ", "<93>", "<94>", "<85>", "<e2><80><9c>", "<e2><80><9d>", 
        "<e2><80><98>", "<e2><80><99>", "<e2><80><9b>", "<ef><bc><87>", 
    	"<e2><80><a6>", "<e2><80><93>", "<e2><80><94>", "<c3><a1>", "<c3><a9>", 
    	"<c2><bd>", "<97>", "<eb>")
    reps <- c("`", "'", "", "\"", "\"", "...", "", "", "'", "'", "'", "'", "...", 
        "&ndash;", "&mdash;", "a", "e", "half", "&mdash;", "&euml;")
    Encoding(text) <-"latin1"
    x <- clean(mgsub(ser, reps, text))
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
    strWrap(x, copy2clip = FALSE)
    invisible(x)
}