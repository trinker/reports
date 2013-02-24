#' Convert md to HTML5 Slides
#' 
#' Use \href{pandoc}{http://johnmacfarlane.net/pandoc/} to convert md to HTML5 
#' slides.
#' 
#' @param in.file A character vector of the md file.
#' @param out.file A character vector of the outfile.  If \code{"replace"} over 
#' writes the original HTML file.  Default, \code{NULL}, uses the root name of 
#' the \code{in.file} plus a number 2.
#' @param ref.page The title of the reference page (adds hanging indent and 
#' reduces font size).  If \code{NULL} references slide will not be adjusted.  If 
#' reference title is not found a warning will print.
#' @param refs.font.size The font size to make the references.
#' @param path The path to where the project should be created.  Default is the 
#' PRESENTATION directory.
#' @details The user must have pandoc installed and on their path.  pandoc can 
#' be instaleld from: \cr \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
#' @return Creates a report template.
#' @export
#' @examples 
#' \dontrun{
#' #Run after running knitr on an Rmd file
#' html5()  #assumes location of html file out of the box
#' }
html5 <-
function(in.file = NULL, out.file = NULL, ref.page = "References", 
        refs.font.size = 14, path = paste0(getwd(), "/PRESENTATION")) {
    WD <- getwd()
    on.exit(setwd(WD))
    setwd(path)   
    if (is.null(in.file)) {
        in.file <- dir(path)[tools::file_ext(dir(path)) == "md"][1]
    }
    if (!is.null(out.file) && out.file=="replace") {
        out.file <- paste0(unlist(strsplit(in.file, "\\."))[1], ".html")
    }
    if (is.null(out.file)) {
        out.file <- paste0(unlist(strsplit(in.file, "\\."))[1], "2.html")
    }
    action <- paste0(wheresPandoc(), " -s -S -i -t dzslides --mathjax ", in.file, 
        " -o ", out.file)
    system(action)
    if (!is.null(ref.page)) {
        HI <- c(".hangingindent {", "    padding-left: 40px ;", 
            "    text-indent: -35px ;", "}")
        HTML5 <- suppressWarnings(readLines(out.file))
        start <- paste0("<h1>", ref.page, "</h1>")
        start <- which(grepl(start, HTML5))                      
        if (identical(start, integer(0))) {
                warning("ref.page not found; argument was ignored")
        } else {
            end <- "</section>"
            end <- which(grepl(end, HTML5))
            end <- end[c(end - start) > 0][1]
            len <- seq_along(HTML5)
            reps <- which(grepl("<p", HTML5) & HTML5 != "<p>" & 
                len > start & len < end)
            start2 <- reps[1]  
            HTML5[start2:end][HTML5[start2:end] == "<p>"] <- "DELETEMEIMEDIATELY"
            SUB <- paste0("<p class=\"hangingindent\" style=\"font-size:", 
                refs.font.size, "px;\">")
            HTML5[reps] <- gsub("<p>", SUB, HTML5[reps])
            reprms <- which(HTML5 == "<p>" & len > (start + 1) & len < end)            
            HTML5 <- HTML5[HTML5 != "DELETEMEIMEDIATELY"]
            splpoint <-which(grepl("Transition effect", HTML5))
            NEW <- c(HTML5[1:(splpoint - 1)], HI, na.omit(HTML5[splpoint:max(len)]))
        }
        cat(paste0(NEW, collapse = "\n"), file=out.file)                                 
    }
    cat("HTML5 file generated!")
}
