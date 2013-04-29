#' Convert R colors to Hexadecimal
#' 
#' Convert R colors() to Hexadecimal.
#' 
#' @param rcolor A character string taken from R's built-in color names.
#' @return Returns a character string of converted hexadecimal colors. 
#' @export
#' @seealso 
#' \code{\link[grDevices]{colors}},
#' \code{\link[grDevices]{rgb}},
#' \code{\link[grDevices]{col2rgb}}
#' @examples
#' col2hex("darkblue")
#' col2hex(c("darkblue", "red"))
#' cat(paste0("<hr color=\"", col2hex("red"), "\" size=\"4\">"))
col2hex <- function(rcolor) {
    cols <- c(rgb(t(col2rgb(colors())), maxColorValue=255))
    colCheck <- colors() %in% rcolor
    cols[colCheck]
}


