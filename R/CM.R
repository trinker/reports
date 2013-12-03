#' HTML Comment CHaracter
#' 
#' Converts text to an HTML comment tag
#' 
#' @param comment A text comment.  Default copies from the clipboard.
#' @param copy2clip  If \code{TRUE} attempts to copy the output to the clipboard.
#' @param print logical. If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the console. If \code{FALSE} \code{\link[base]{return}}s to the 
#' console.
#' @return Returns a character vector of an HTML comment tag.
#' @references \url{http://www.w3schools.com/tags/tag_comment.asp}
#' @keywords comment
#' @export
#' @examples
#' \dontrun{
#' CM("strike me from the record")
#' }
CM <- function(comment = "clipboard", copy2clip = TRUE, print = FALSE) {


    ## Optional read from the clipboard
    if (comment == "clipboard") {
        comment <- read_clip()
    } 

    x <- sprintf("<!--%s-->", comment)

    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}