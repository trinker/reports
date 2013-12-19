#' Video (YouTube/Vimeo/local) url to HTML iframe/video Tag
#' 
#' \code{YT} - Wrap a YouTube tag or url to generate an HTML iframe tag. 
#' 
#' @param path A url/tag/path. Default is to read from the clipboard.  Note that 
#' \code{VD} requires a .mp4 file.
#' @param width The width of the player.
#' @param height The height of the player.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the  console.  If \code{FALSE} returns to the console. 
#' @details Use \code{YT} for YouTube videos and \code{VM} for Vimeo videos.
#' @return Returns a character vector of an HTML iframe tag that embeds a YouTube 
#' or Vimeo video.
#' @note For YouTube videos it may be better to utilize a slide layout class as 
#' a slide with a link followed by a slide with a youtube video may cause the 
#' link to become in active.  To create a youtube slide use the form: 
#' \code{--- .YT yt:ArHQjQyIS70 &youtube} where the portion after \code{yt:} is 
#' the youtube tag.
#' @export
#' @rdname video
#' @references \url{http://blog.teamtreehouse.com/building-custom-controls-for-html5-videos}
#' @seealso \code{\link[reports]{slidify_layouts}},
#' \code{\link[reports]{js_copy}}
#' @examples
#' YT("ArHQjQyIS70", print = TRUE)
#' YT("http://www.youtube.com/watch?v=ArHQjQyIS70", print = TRUE)
#' VM("http://vimeo.com/54007714", print = TRUE)
YT <- function(path = "clipboard", width = 640, height = 360, 
	copy2clip = interactive(), print = FALSE) { 
	
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

#' Video (YouTube/Vimeo/local) url to HTML iframe/video Tag
#' 
#' \code{VM} - Wrap a Vimeo tag or url OR to generate an HTML iframe tag. 
#' 
#' @export
#' @rdname video
VM <- function(path = "clipboard", width = 640, height = 360,
    copy2clip = interactive(), print = FALSE) { 
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


#' Video (YouTube/Vimeo/local) url to HTML iframe/video Tag
#' 
#' \code{VD} - Wrap a local path to generate an HTML video tag. 
#' 
#' @param video.js.path The path to the vidscript.js.  If the file is not found 
#' in the \code{\link[base]{dirname}} of \code{video.js.path} then 
#' \code{\link[reports]{js_copy}} will be utilized to place the appropriate 
#' video file in the correct location.
#' @param indent.controls An integer value for number of indents to push the 
#' control panel.
#' @keywords video
#' @export
#' @rdname video
VD <- function(path = "clipboard", video.js.path = "assets/js/vidscript.js", 
    width = "100%", height = 520, indent.controls = 4, 
	copy2clip = interactive(), print = FALSE) {

    if (path == "clipboard") {
        path <- read_clip()
    } 

    check <- file.exists(video.js.path)
    check2 <- file.exists(dirname(video.js.path))
    check3 <- sum(check, check2) 
    if (check3 == 0) {
        warning("javascript folder not located; use `video.js.path`")
    }
    if (check3 == 1) {
        suppressMessages(js_copy(dirname(video.js.path)))
    }
    

    vid <- system.file("extdata/reports_layouts/video.html", 
        package = "reports")
    vid <- readLines(vid)

    x <- sprintf(paste(vid, collapse = "\n"), as.character(width), 
    	as.character(height), path, HS(indent.controls, copy2clip = FALSE), 
    	video.js.path)
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)    
}

