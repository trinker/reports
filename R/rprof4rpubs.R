#' Rpubs .Rprofile
#' 
#' When publishing to Rpubs an error may occur as seen below.  
#' \code{rprof4rpubs} adds/appends a .Rprofile with 
#' \code{options(rpubs.upload.method = "internal")} necessary for publishing to 
#' \href{http://rpubs.com/}{Rpubs} from \href{http://www.rstudio.com/}{Rstudio}.
#' 
#' @param path The path to the directory with the document to be published.  
#' Also takes the values of \code{1} corresponding to \file{~PRESNETATION} or
#' \code{2} corresponding to \file{~REPORT}.
#' @references \url{http://support.rstudio.org/help/discussions/problems/2582-publishing-to-rpubs-from-behind-proxy#comment_16691989}
#' @details If you see the following error \code{rprof4rpubs} may be of help:
#' 
#' Error in function (type, msg, asError = TRUE)  : \cr
#'   SSL certificate problem, verify that the CA cert is OK. Details: \cr
#'   error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed \cr
#'   Calls: rpubsUpload ... <Anonymous> -> .postForm -> .Call -> <Anonymous> -> fun
#' @export
rprof4rpubs <- function(path = 1) {
	
	if (path %in% c("1", "2")) {
		if (path == "1") {
		    path <- "PRESENTATION"
		} else {
			path <- "REPORT"
		}
	}
	
	checkp <- file.path(path, ".Rprofile")
	if (file.exists(checkp)) {
        message(paste0("'", checkp, 
            "' already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "1") {
            append <- FALSE
        } else {
            append <- TRUE
        }		
	} else {
	    append <- FALSE
	}
	
	cat(paste0(ifelse(append, "\n", ""), 
		"options(rpubs.upload.method = \"internal\")"), 
		file=sprintf("%s/.Rprofile", path), append=append)
	
}

