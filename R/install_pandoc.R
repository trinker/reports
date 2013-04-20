#NO LONGER EXPORTED: USE installr instead
#
#Download Pandoc
#
#Download Pandoc from the command line (Windows users). 
#
#@return Installs Pandoc on your system.
#@author Gergely Daroczi and Gabor Grothendieck
#@references \url{http://stackoverflow.com/a/15072501/1000343} 
#@section Pandoc Website: \url{http://johnmacfarlane.net/pandoc/}
#@export
#@examples
#\dontrun{
#install_pandoc()
#}
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
