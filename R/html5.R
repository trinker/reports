#' Convert md to HTML5 Slides
#' 
#' Use \href{pandoc}{http://johnmacfarlane.net/pandoc/} to convert md to HTML5 
#' slides.
#' 
#' @param in.file A character vector of the md file.
#' @param out.file A character vector of the outfile.  If NULL over writes the 
#' original HTML file.
#' @param path The path to where the project should be created.  Default is the 
#' PRESENTATION directory.
#' @details The user must have pandoc installed and on their path.  pandoc can 
#' be instaleld from: \cr \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
#' @return Creates a report template.
#' @export
#' @examples 
#' \dontrun{
#' html5()  #assumes location of html file out of the box
#' }
html5 <- 
function(in.file = NULL, out.file = NULL, 
    path = paste0(getwd(), "/PRESENTATION")) {
    WD <- getwd()
    on.exit(setwd(WD))
    setwd(path)    
    if (is.null(in.file)) {
        in.file <- dir(path)[tools::file_ext(dir(path)) == "md"][1]
    }
    if (is.null(out.file)) {
        out.file <- paste0(unlist(strsplit(in.file, "\\."))[1], "2.html")
    }
    action <- paste0(wheresPandoc(), " -s -S -i -t dzslides --mathjax ", in.file, " -o ", out.file)
    system(action)
    cat("HTML5 file generated!")
}