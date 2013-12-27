#' Create Mouseovers
#' 
#' Generate mousever events for text.
#' 
#' @param presentation logical.  If \code{TRUE} created popup.js documentation 
#' in the PRESENTATION directory.  \code{FALSE} generates the documentation in 
#' the REPORT directory.  
#' @param rmd.path An optional path to a .Rmd file if 
#' \code{presentation = FALSE}.
#' @return Generates an assets directory with appropriate javascript and css to
#' make mousever popups.
#' @details The user must become familiar with Nicolas Honing's 
#' \href{https://github.com/nhoening/popup.js}{popup.js}.  Currently no shortcut
#' reports package tag generating function is offered for popups that utilize 
#' popup.js.  To view example uses see \url{https://github.com/trinker/popup_example}.
#' @note If adding popup.js documents to non-slidify, .Rmd based presentations 
#' the user will have to manually add: \cr 
#'      
#' \code{<link rel="stylesheet" href="./assets/css/pop_style.css" />} \cr      
#' \code{<script type="text/javascript" src="./assets/js/jquery-1.9.1.min.js"></script>} \cr    
#' \code{<script type="text/javascript" src="./assets/js/nhpup_1.1.js"></script>} \cr     
#' 
#' to the document.  
#' @references 
#' \url{https://github.com/nhoening/popup.js}
#' @keywords popup mouseover
#' @export
#' @importFrom tools file_ext
popup <- function(presentation = FALSE, rmd.path = NULL) {
     
    ## Function based on the work of:
    ## Nicolas Honing
    ## https://github.com/nhoening/popup.js/blob/master/test/test.html

    ## Location of popup.js documents
    root <- system.file("extdata/popup", package = "reports")
    css <- file.path(root, "popup_style.css")
    js <- file.path(root, "popup_js")
	
    if (is.logical(presentation) && isTRUE(presentation)) {
        file.copy(css, "PRESENTATION/assets/css")
        file.copy(file.path(js, dir(js)), "PRESENTATION/assets/js")    	
    } else {
        if (is.logical(presentation) && !isTRUE(presentation)) {
            folder(folder.name = c("REPORT/assets/css", "REPORT/assets/js"))
            file.copy(css, "REPORT/assets/css")
            file.copy(file.path(js, dir(js)), "REPORT/assets/js") 
        	if (is.null(rmd.path)) {
        	    rmds <- file_ext(dir("REPORT")) == "Rmd"
        		if (sum(rmds) == 0) {
        			warning(sprintf("No .Rmd file founf in %s/REPORT", getwd()))
        		}
        		outfile <- file.path("REPORT", outfile)
        		infile <- readLines(outfile)	
        	}
        	
        } else {
            folder(folder.name = file.path(dirname(rmd.path), 
            	c("assets/css", "assets/js")))
            file.copy(css, file.path(dirname(rmd.path), "assets/css"))
            file.copy(file.path(js, dir(js)), 
            	file.path(dirname(rmd.path), "assets/js")) 
            outfile <- rmd.path
            infile <- readLines(outfile)
        }
        top <- c(
            "<link rel=\"stylesheet\" href=\"./assets/css/pop_style.css\" />",
            "<script type=\"text/javascript\" src=\"./assets/js/jquery-1.9.1.min.js\"></script>",
            "<script type=\"text/javascript\" src=\"./assets/js/nhpup_1.1.js\"></script>\n"
        )
        cat(paste(c(top, infile), collapse = "\n"), file = outfile)

    }
    message("popup.js documents generate")
}


## PU <- function(text, note) {
##      
## 	js <- c("jquery-1.9.1.min.js", "nhpup-ioshack_1.0.js", "nhpup_1.0.js", 
## 		"nhpup_1.1.js")
## 
## 	##  warning 
## 	# all(file.exists(file.path("PRESENTATION/assets/js", js)))
## 
## }

