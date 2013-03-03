#' Convert md to HTML5 DZSlides
#' 
#' Uses \href{pandoc}{http://johnmacfarlane.net/pandoc/} to convert md to HTML5 
#' \href{http://paulrouget.com/dzslides/}{DZSlides} slides and provides minor 
#' modifications (e.g., embedded youtube and hanging indent text).
#' 
#' @param in.file A character vector of the md file.
#' @param out.file A character vector of the outfile.  If \code{"replace"} over 
#' writes the original HTML file.  Default, \code{NULL}, uses the root name of 
#' the \code{in.file} plus a number 2.
#' @param ref.page The title of the reference page (adds hanging indent and 
#' reduces font size).  If \code{NULL} references slide will not be adjusted.  
#' If reference title is not found a warning will print.
#' @param refs.cex The font size to make the references.
#' @param path The path to where the documents reside/should be created.  
#' Default is the PRESENTATION directory.  This conveniently allows for non 
#' paths to be spplied to \code{in.file} and \code{out.file}.  Paths can be 
#' supplied to \code{in.file} and \code{out.file} by setting \code{path} to 
#' \code{NULL}.
#' @param hi.cex The font size to make the hanging indent coded text if \code{hi}
#' code chunk is used.
#' @details The user must have Pandoc installed and on their path.  Pandoc can 
#' be instaleld from: \cr \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
#' @section Code Chunks: The following convience code chunks are implemented:
#' \enumerate{ 
#'   \item{\bold{hi} - Wraping text with this code chunk will result in 
#'   hangingin indentation.  Use \code{hi.cex} to control the font size of the text.}
#'   \item{\bold{yt} - Wrap a youtube url or tag to embed a youtube video}
#' }
#' Code chunks use the following form: \code{\bold{[[[text]]]=code.tag}} (e.g.,
#' \bold{[[[cokNUTGtoM4]]]=yt} embeds a youtube video.  Currently this 
#' is a convenience feature that may have unexpected results and may need 
#' additional tweaking within the html output.  When using embedded youtube, 
#' slide titles and text are ignored but may effect the spacing of the player.  
#' User additions are welcomed.
#' @author Ananda Mahto & Tyler Rinker <tyler.rinker@@gmail.com>
#' @references \url{http://stackoverflow.com/a/14971683/1000343}
#' @export
#' @examples 
#' \dontrun{
#' #Run after running knitr on an Rmd file
#' dzslides()  #assumes location of html file out of the box
#' }
dzslides <-
function(in.file = NULL, out.file = NULL, ref.page = "References", 
        refs.cex = 15, path = file.path(getwd(), "PRESENTATION"), 
		hi.cex = 25) {
    if (!is.null(path)) {
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
                refs.cex, "px;\">")
            HTML5[reps] <- gsub("<p>", SUB, HTML5[reps])
            reprms <- which(HTML5 == "<p>" & len > (start + 1) & len < end)            
            HTML5 <- HTML5[HTML5 != "DELETEMEIMEDIATELY"]
            splpoint <-which(grepl("Transition effect", HTML5))
            NEW <- c(HTML5[1:(splpoint - 1)], HI, na.omit(HTML5[splpoint:max(len)]))
        }
    } else {
        NEW <- suppressWarnings(readLines(out.file))	
    }
    lchunk <- grepl("[[[", NEW, fixed = TRUE)
    yt <- which(lchunk & grepl("]]]=yt", NEW, fixed = TRUE)) 
    if (!identical(yt, integer(0))) {
        yt2 <- mgsub(c("<p>[[[", "]]]=yt</p>"), "", NEW[yt], fixed = TRUE) 
        yt2b <- strsplit(yt2, "v=")
        yt2c <- strsplit(sapply(yt2b, function(x) x[length(x)]), "&")
        tags <- sapply(yt2c, function(x) x[1])
        yt3 <- paste0("<iframe class=\"youtube-player\" type=\"text/html\" width=\"800\" height=\"500\" src=\"http://www.youtube.com/embed/", 
            tags, "\" frameborder=\"0\"> </iframe>")
        NEW[yt] <- yt3  
  	}
    lchunk <- grepl("[[[", NEW, fixed = TRUE)
    hi <- which(lchunk & grepl("]]]=hi", NEW, fixed = TRUE)) 
    if (!identical(hi, integer(0))) {
        reps <-lapply(strsplit(NEW[hi], "]]]=hi<br>"), function(x) {
    	    mgsub(pattern = c("[[[", "<p>", "]]]=hi</p>"), replacement = "", x)
        })
        reps <- lapply(reps, function(x) {
            paste0("<p class=\"hangingindent\" style=\"font-size:", hi.cex, 
                "px;\">", x, "</p>")	
        })
        ends <- c(hi -1, length(NEW))
        starts <- c(1, hi + 1)
        out <- c()
        for (i in seq_along(length(reps))){
        	out <- c(out, NEW[starts[i]:ends[i]], unlist(reps[[i]]))
   	    }
        NEW <- c(out, NEW[starts[length(starts)]:ends[length(ends)]])
    }
    cat(paste0(NEW, collapse = "\n"), file=out.file)     
    cat("HTML5 DZSlides file generated!\n")
}
