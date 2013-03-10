#' Convert tex to docx 
#' 
#' Use \href{pandoc}{http://johnmacfarlane.net/pandoc/} to convert tex to docx 
#' for those colleagues who use docx. 
#' 
#' @param in.file A character vector of the md file.
#' @param out.file A character vector of the outfile.  If \code{"replace"} over 
#' writes the original HTML file.  Default, \code{NULL}, uses the root name of 
#' the \code{in.file} plus a number 2.
#' @param path The path to where the documents reside/should be created.  
#' Default is the REPORT directory.  This conveniently allows for non paths to 
#' be supplied to \code{in.file} and \code{out.file}.  Paths can be supplied to 
#' \code{in.file} and \code{out.file} by setting \code{path} to \code{NULL}.
#' @param bib.loc Optional path to a .bib resource.
#' @details The user must have pandoc installed and on their path.  pandoc can 
#' be installed from: \cr \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
#' @export
#' @examples 
#' \dontrun{
#' DOC <- system.file("extdata/doc_library/apa6.qual_tex", 
#'    package = "reports")
#' tex2docx(DOC, "test.docx")  
#' }
tex2docx <-
function(in.file = NULL, out.file = NULL, path = paste0(getwd(), "/REPORT"), 
    bib.loc = getOption("bib.loc")) {
    if (!is.null(path)) {
        WD <- getwd()
        on.exit(setwd(WD))
        setwd(path)   
        if (is.null(in.file)) {
            in.file <- dir(path)[tools::file_ext(dir(path)) == "tex"][1]
        }
        if (is.null(out.file)) {
            out.file <- paste0(unlist(strsplit(in.file, "\\."))[1], ".docx")
        }
    }
    action <- paste0(wheresPandoc(), " -s ", in.file, " -o ", out.file)
    if (!is.null(bib.loc)) {
        action <- paste0(action, " --bibliography=", bib.loc)
    }
    system(action)
    cat("docx file generated!\n")
}