#' Download Pandoc
#' 
#' Windows users download Pandoc from the command line. 
#' 
#' @return Installs Pandoc on your system.
#' @author Gergely Daroczi, Gabor Grothendieck and Tyler Rinker<tyler.rinker@@gmail.com>.
#' @references \url{http://stackoverflow.com/a/15072501/1000343}
#' \url{http://johnmacfarlane.net/pandoc/}
#' @keywords transcript
#' @seealso \code{\link[qdap]{dir_map}}
#' @examples
#' \dontrun{
#' install_pandoc()
#' }
install_pandoc <- function() {
    page <- readLines('http://code.google.com/p/pandoc/downloads/list', warn = FALSE)
    pat  <- "//pandoc.googlecode.com/files/pandoc-[0-9.]+-setup.exe"
    line <- grep(pat, page, value = TRUE); m <- regexpr(pat, line)
    url  <- paste('http', regmatches(line, m), sep = ':')
    tmp <- tempfile(fileext = '.exe')
    download.file(url, tmp, mode = 'wb')
    system(tmp)
    on.exit(unlink(tmp))
}