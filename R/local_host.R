#' Generate Local Host Bash/Batch
#' 
#' Create a .sh/.bat file to open a presentation with a local host. 
#' 
#' @param file The path to the presentation directory.
#' @return Creates a .sh/.bat file that opens a local host to that directory.
#' @note The .sh/.bat file can not be opened from within RStudio.
#' @details The user must have \href{http://www.python.org/}{Python} installed 
#' and on their path to be able to utilize this function.  For more details see:
#' \url{http://www.youtube.com/watch?v=VlqpiKHz7Gw&feature=youtu.be}.
#' @keywords localhost
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@@gmail.com>
#' @importFrom tools file_ext
#' @export
#' @examples
#' ## local_host()
local_host <- function(out.dir = file.path(getwd(), "PRESENTATION")) {
    OS <- Sys.info()["sysname"]
    ext <- "local_host.sh"
    if (OS == "Windows") {
    	loc <- system.file("extdata/bash/win/local_host.bat", 
    	    package = "reports")
        ext <- "local_host.bat"
    } else {
    	if (OS == "Darwin") {
        	loc <- system.file("extdata/bash/mac/local_host.sh", 
        	    package = "reports")    	
    	} else {
            if (OS == "Linux") {
                loc <- system.file("extdata/bash/linux/local_host.sh", 
                    package = "reports")
            } else {
                warning(paste0("Operating system nust be Windows, Darwin or Linux:\n",
                    "local host not generated"))
                return(NULL)
            }
        }
    }
    script <- suppressWarnings(readLines(loc))
    location <- grep("cd ALTER_PATH", script)
    script[location] <- paste("cd", out.dir)
    out <- file.path(out.dir, ext)
    cat(paste(script, collapse="\n"), file = out)
    message(paste0("To open a local host click:\n", out))
}

