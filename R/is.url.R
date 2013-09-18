#' Check If String is a URL
#' 
#' Uses a regex approach to check if a string is a url.  This approach is faster
#' than \code{\link[RCurl]{url.exists}} but does do the actual verification.
#' 
#' @param x A character string.
#' @return Returns a logical evalution as to whether a string is a url.
#' @keywords url
#' @export
#' @seealso \code{\link[RCurl]{url.exists}}
#' @examples
#' urls <- c("a", "f/g/h", "www.talkstats.com", "https://github.com/trinker")
#' is.url(urls)
is.url <-function(x) {
    grepl("www.|http:|https:", x)
}


