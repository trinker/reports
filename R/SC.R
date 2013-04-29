#' HTML Special Characters 
#' 
#' A system to produce HTML special characters.
#' 
#' @param text A character vector or text copied to the clipboard.  Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.  
#' @return Returns an HTML special character entity.
#' @details The general use of \code{SC}is to provide a character string of length 
#' 2.  The first character is a letter and the second is one of the following 
#' symbols: \cr
#' \cr
#' c("'", "`", ":", "~", "^", "/", "o", ",") \cr
#' \cr
#' Which corresponds to: \cr
#' \cr
#' c("acute", "grave", "uml", "tilde", "circ", "slash", "ring", "cedil") \cr
#' \cr
#' See examples for cases not conforming to this use.    
#' @section Warning: The user can create non-HTML characters with \code{SC} that will not 
#' be converted (i.e., \code{SC("b~")} would yield \code{"&btilde;"} and would 
#' not be converted appropriately).
#' @export
#' @examples
#' SC("A'")
#' SC('a\'')  #can use single quotes with escape 
#' SC("a`")
#' SC("n~")
#' SC("o:")
#' SC("(c)")
#' SC("(r)")
#' SC("c|")
#' SC("o/")
#' SC("ao")
#' SC("c,")
#' SC("p") 
#' SC("P") 
#' SC("E")
#' SC("Y")
#' SC("/")
#' SC("+-")
#' SC("L")
#' SC("tm") 
#' SC("S") 
SC <- function(text, copy2clip = TRUE) {
    out <- unlist(strsplit(text, NULL))
    seconds <- c("'", "`", ":", "~", "^", "/", "o", ",")
    if (length(out) == 2 && out[2] %in% seconds) {
        repalcements <- c("acute", "grave", "uml", "tilde", "circ", 
            "slash", "ring", "cedil")
        x <- paste0("&", out[1], repalcements[seconds %in% out[2]], ";")
    } else {
        ins <- c("(c)", "(r)", "c|", "tm", "s", "S", "(R)", "(C)", "L", "E", 
            "P", "p", "Y", "y", "+-", "/")
        outs <- c("&copy;", "&reg;", "&cent;", "&trade;", "&sect;", "&sect;", 
            "&reg;", "&copy;", "&pound;", "&euro;", "&para;", "&para;", "&yen;", 
            "&yen;", "&plusmn;", "&divide;")
        x <- outs[ins %in% text]
    }
    if(copy2clip){
        write_clip(x)
    }
    x 
}

