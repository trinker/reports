#' Video (YouTube/Vimeo) url to HTML iframe Tag
#' 
#' Wrap a YouTube/Vimeo tag or url to generate an HTML iframe tag. 
#' 
#' @param path A character vector url/tag copied to the clipboard. Default is to 
#' read from the clipboard.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param width The width of the player.
#' @param height The height of the player.
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE} returns to the console. 
#' @details Use \code{YT} for YouTube videos and \code{VM} for Vimeo videos.
#' @return Returns a character vector of an HTML iframe tag that embeds a YouTube 
#' or Vimeo video.
#' @export
#' @rdname video
#' @examples
#' YT("AZnVM0_ru7o", print = TRUE)
#' YT("http://www.youtube.com/watch?v=AZnVM0_ru7o", print = TRUE)
#' VM("http://vimeo.com/54007714", print = TRUE)
YT <- function(path = "clipboard", copy2clip = TRUE, width = 640, height = 360,
    print = FALSE) { 
    if (path == "clipboard") {
        path <- read_clip()
    } 
    yt1 <- strsplit(path, "v=")
    yt2 <- strsplit(sapply(yt1, function(x) x[length(x)]), "&")
    tags <- sapply(yt2, function(x) x[1])
    x <- paste0("<iframe class=\"youtube-player\" type=\"text/html\" width=\"", 
        width, "\" height=\"", height, "\" src=\"http://www.youtube.com/embed/", 
        tags, "?autoplay=0", "\" frameborder=\"0\"></iframe>")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
} 


#' @export
#' @rdname video
VM <- function(path = "clipboard", copy2clip = TRUE, width = 640, height = 360,
    print = FALSE) { 
    if (path == "clipboard") {
        path <- read_clip()
    } 
    vm <- rev(unlist(strsplit(path, "/")))[1]
    link <- paste0("src=\"http://player.vimeo.com/video/", vm, "\"")
    x <- paste0("<iframe ", link, 
       " width=\"", width, "\" height=\"", height, 
       "\" frameborder=\"0\" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
