#' Format Quotes
#' 
#' Tool to format text taken from articles for LaTeX.  Combines multiple stringed text into one string.  Removes non ascii characters and hyphens
#' 
#' @param quotes logical.  If \code{TRUE} LaTeX style quotes (2 backticks and two single quotes) are wrapped around the text.
#' @param block If \code{TRUE} LaTeX block quote code tags are used instead of the backticks and single quotes.
#' @param text character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @details This function formats text for use with LaTeX documents.  
#' @return Returns a charcter vector with LaTeX formatted text.
#' @import qdap
#' @export
gq <- 
function(quotes = TRUE, block = TRUE, text = "clipboard", copy2clip = TRUE){
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
    text <- Trim(iconv(text, "", "ASCII", "byte"))
    ser <- c("<91>", "<92>", "- ", "<93>", "<94>", "<85>", "<e2><80><9c>", "<e2><80><9d>", 
        "<e2><80><98>", "<e2><80><99>", "<e2><80><9b>", "<ef><bc><87>", 
    	"<e2><80><a6>", "<e2><80><93>", "<e2><80><94>", "<c3><a1>", "<c3><a9>", 
    	"<c2><bd>")
    reps <- c("`", "'", "", "``", "''", "\\ldots", "", "", "'", "'", "'", "'", "\\ldots", 
        "$-$", "$-$", "a", "e", "half")
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
        if (wc(text) > 39 ) {
            LONG <- TRUE
            L <- "\\begin{quote}\n"
            R <- "\n\\end{quote}"
        }
        x <- paste0(L, text, R)
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
    if (LONG) {
        strWrap(text, copy2clip = FALSE)
    } else {
        strWrap(x, copy2clip = FALSE)
    }
    invisible(x)
}

