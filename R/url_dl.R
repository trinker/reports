#' Download Instructional Documents
#' 
#' This function enables downloading documents for future instructional training. 
#' 
#' @param \ldots Document names to download. 
#' @param url The download url. 
#' @return Places a copy of the downloaded document in the users working 
#' directory.
#' @note Not intended for general use.
#' @export
url_dl <-
function(..., url = "http://dl.dropbox.com/u/61803503/") {
    mf <- match.call(expand.dots = FALSE)
    payload <- as.character(mf[[2]])
    FUN <- function(x, url) {
        bin <- getBinaryURL(paste0(url, x), ssl.verifypeer=FALSE)  
        con <- file(x, open = "wb")
        writeBin(bin, con)
        close(con)
        message(noquote(paste(x, "read into", getwd())))
    }
    invisible(lapply(payload, function(z) FUN(x = z, url = url)))
}