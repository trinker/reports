#' Audio url/path to HTML Audio Tag
#' 
#' Converts a url/path to an HTML audio tag.  \code{AD} attempts to determine  
#' the location of the audio file if it is not a path or url.
#' 
#' @param audio A url, path or file located in the audio directory which is an 
#' audio file.
#' @param is_url logical.  If \code{TRUE} \file{audio} is located in the 
#' \file{~audio} or \file{../audio} directory of \file{REPORTS} or 
#' \file{PRESENTATION} directories.
#' @param copy2clip  If \code{TRUE} attempts to copy the output to the clipboard.
#' @param print logical. If \code{TRUE} \code{\link[base]{cat}} prints the 
#' output to the console. If \code{FALSE} \code{\link[base]{return}}s to the 
#' console.
#' @return Returns a character vector of an HTML source tag that embeds an audio
#' file.
#' @references \url{http://www.w3schools.com/html/html5_audio.asp}
#' @keywords audio
#' @export
#' @examples
#' \dontrun{
#' AD("foo.mp4")
#' }
AD <- 
function(audio = "clipboard", is_url = is.url(audio), copy2clip = interactive(), 
    print = FALSE) { 

    ## Optional read from the clipboard
    if (audio == "clipboard") {
        audio <- read_clip()
    } 

    ## determine if audio is located in the audio folder
    if(is_url | file.exists(audio)) {
    	spath <- ""
    } else {
        if (file.exists(sprintf("audio/%s", audio))) {
            spath <- "audio/"
        } else {
        	insdir <- ifelse(basename(getwd()) == "REPORT", "PRESENTATION", "REPORT")
            if (file.exists(sprintf("../%s/audio/%s", insdir, audio))) {
                spath <- sprintf("../%s/audio/", insdir)           
            } else {
                stop(paste0("The following audio file does not exist:\n",
                    sprintf(QP("audio/%s"), audio)))  
            }
        }
    }
       
    src <- paste0("    <source src=\"", spath, "%s\" type=\"audio/mpeg\">\n")
    x <- paste0("<audio controls>\n",
        sprintf(src, audio),
        "    Your browser does not support the audio element.\n</audio>")
    if(copy2clip){
        write_clip(x)
    }
    prin(x = x, print = print)
}
