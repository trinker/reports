#' Convert md to HTML5 reveal.js  
#' 
#' Uses \href{pandoc}{http://johnmacfarlane.net/pandoc/} to convert md to HTML5 
#' \href{http://lab.hakim.se/reveal-js/}{reveal.js} slides and provides minor 
#' modifications (e.g., embedded youtube and hanging indent text, etc.).
#' 
#' @param theme possible reveal.js themes: c(\code{"sky"}, \code{"beige"}, 
#' \code{"simple"}, \code{"serif"}, \code{"night"}, \code{"default"}).
#' @param transition possible reveal.js transitions: c(\code{"default"}, 
#' \code{"cube"}, \code{"page"}, \code{"concave"}, \code{"zoom"}, 
#' \code{"linear"}, \code{"fade"}, \code{"none"}).
#' @param in.file A character vector of the md file.
#' @param out.dir A character vector of the out directory.  Default uses the 
#' root name and outputs to a directory called reveal_js. 
#' @param ref.page The title of the reference page (adds hanging indent and 
#' reduces font size).  If \code{NULL} references slide will not be adjusted.  
#' If reference title is not found a warning will print.
#' @param refs.cex The font size to make the references.
#' @param path The path to where the documents reside.  Default is the 
#' PRESENTATION directory.  This conveniently allows for non paths to be supplied 
#' to \code{in.file} and \code{out.file}.  Paths can be supplied to 
#' \code{in.file} and \code{out.file} by setting \code{path} to \code{NULL}.
#' @param figure.dir The name of the figures directory where figures are stored.  
#' This should be in the presentation folder.  By default it is called figure.
#' @param hi.cex The font size to make the hanging indent coded text if \code{hi}
#' code chunk is used.
#' @details Outputs a directory, revel_js, with an index.html and necessary 
#' \href{http://lab.hakim.se/reveal-js/}{reveal.js} files. The user must have 
#' Pandoc installed and on their path.  
#' Pandoc can be installed from: \cr \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
#' @section Code Chunks: The following convenience code chunks are implemented:
#' \enumerate{ 
#'   \item{\bold{bg-} - Place after a slide title to change background color.  
#'   Currently takes \code{bg-soothe}, \code{bg-blackout}, \code{bg-alert}, 
#'   \code{ red }, \code{ orange }, \code{ yellow }, \code{ green }, 
#'   \code{ blue }, \code{ indigo }, \code{ violet }, or \code{ brown }.} 
#'   \item{\bold{frag-} - Using a dash/space (- ) has the usual effect of text 
#'   appearing however special effects can be applied to text by using the 
#'   dash/space followed by the text and followed by \code{[[[]]]=frag-} and 
#'   last a fragment style.  Valid styles include: \code{grow}, \code{shrink}, 
#'   \code{roll-in}, \code{fade-out}, \code{highlight-red}, \code{highlight-green}, 
#'   or \code{highlight-blue}.}
#'   \item{\bold{hi} - Wrapping text with this code chunk will result in 
#'   hanging indentation.  Use \code{hi.cex} to control the font size of the text.}
#'   \item{\bold{notes} - Wrap presenter notes that work when slides are uploaded 
#'   to the Internet. Press "s" to get the speaker notes window.}
#'   \item{\bold{small} - Wrap text to produce small font.}
#'   \item{\bold{sud} & \bold{eud}- Wrap a group of slides to give the nested up 
#'   and down capabilities. start-up-down (sud) goes directly above the beginning 
#'   side in the .Rmd file, end-up-down (eud) goes directly at the end of the 
#'   last slide in the nested group, however there should be a space between 
#'   text and this code tag.}
#'   \item{\bold{yt} - Wrap a youtube url or tag to embed a 
#'   youtube video.}
#' }
#' Code chunks use the following form: \bold{[[[text]]]=code.tag} (e.g.,
#' \bold{[[[cokNUTGtoM4]]]=yt} embeds a youtube video.  Currently this 
#' is a convenience feature that may have unexpected results and may need 
#' additional tweaking within the html output.  When using embedded youtube, 
#' slide titles and text are ignored but may effect the spacing of the player.  
#' User additions are welcomed.
#' 
#' reports based code chunks are for convenience.  For more control many HTML 
#' tags work with Rmarkdown.  The output code can also be manipulated for finer 
#' control.  A \code{reveal.js} demo can be found \url{http://trinker.github.com/reports/examples/}.
#'   
#' @export
#' @section Warning: reveal.js is deprecated and will be removed from reports in 
#' a version subsequent to version 1.3.  It is recomended for the user to become 
#' familiar with the slidify approach to html5 presentation as this will be the 
#' framework used by the reports package.
#' @seealso \code{\link[reports]{html5}}
#' @examples 
#' \dontrun{
#' #Run after running knitr on an Rmd file
#' reveal.js()  #assumes location of html file out of the box
#' 
#' # An example .Rmd file can be found:
#' system.file("extdata/docs/example.Rmd", package = "reports")
#' }
reveal.js <-
function(theme = "default", transition = "default", in.file = NULL, 
		out.dir = path, ref.page = "References", refs.cex = 15, 
		path = file.path(getwd(), "PRESENTATION"), figure.dir = "figure", 
		hi.cex = 25) {
    .Deprecated(msg = paste("reveal.js is deprecated.  Please use the",
        "functionality of the slidify package."), 
        old = as.character(sys.call(sys.parent()))[1L])
    if (!is.null(path)) {
        WD <- getwd()
        on.exit(setwd(WD))
        setwd(path) 
    }
    if(file.exists(file.path(path, "reveal_js"))) {
        delete(file.path(path, "reveal.js"))
    }
    if(is.null(out.dir)) {
        out.dir <- path	
    }      
    root <- system.file("extdata/reveal_js", package = "reports")
    file.copy(root, out.dir, recursive = TRUE)
    x <- file.path(out.dir, "reveal_js") 
    if (is.null(in.file)) {
        in.file <- dir(path)[tools::file_ext(dir(path)) == "md"][1]
    }
    if (is.na(in.file)) stop("run Knit HTML on the .Rmd file first")
    out.file <- file.path(x, paste0(unlist(strsplit(in.file, "\\."))[1], ".html"))
    tmp <- tempdir()
    file.copy(in.file, tmp)
    of <- file.path(tmp, basename(out.dir))
    action <- paste0(wheresPandoc(), " -s -S -i -t dzslides --mathjax ", in.file, 
        " -o ", of) 
    system(action)
    if (!is.null(ref.page)) {
        HI <- c(".hangingindent {", "    padding-left: 40px ;", 
            "    text-indent: -35px ;", "}")
        HTML5 <- suppressWarnings(readLines(of))
        start <- paste0("<h1>", ref.page)
        start <- which(grepl(start, HTML5))                      
        if (identical(start, integer(0))) {
            warning("ref.page not found; argument was ignored")
            NEW <- suppressWarnings(readLines(of))
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
        NEW <- suppressWarnings(readLines(of))	
    }  
    small <- which(grepl("]]]=small", NEW, fixed = TRUE)) 
    if (length(small) > 0) {
        NEW[small] <- mgsub(c("[[[", "]]]=small"), 
            c("\n<p>\n<small>\n", "\n</small>\n</p>\n"), NEW[small], fixed = TRUE) 
  	} 
    tmp <- tempdir()
    cat(paste0(NEW, collapse = "\n"), file=file.path(tmp, "temphtml"))
    NEW <- suppressWarnings(readLines(file.path(tmp, "temphtml")))    
    frag <- which(grepl("[[[]]]=frag", NEW, fixed = TRUE))     
    if (length(frag) > 0) {
        NEW[frag] <-gsub("<li>", "<li class=\"fragment\">", NEW[frag])  
        fraginst <- genXtract(NEW[frag], "[[[]]]=frag-", "</li>")
        fraginst <- 	gsub("</p>", "", fraginst)   
        inserts <- 	paste0(genX(NEW[frag], "[[[]]]=frag-", "</li>"), "</li>")
        inserts <- 	gsub("<p>", "", inserts)
    	A <- unlist(strsplit(inserts, "fragment>"))[c(TRUE, FALSE)]
        B <- unlist(strsplit(inserts, "fragment>"))[c(FALSE, TRUE)]
        NEW[frag] <- mgsub(c("<li", "</li>"), c("<p", "</p>"), paste0(A, 
        	"\"fragment ", fraginst, "\">", B), fixed=TRUE)
        NEW[frag] <- gsub("[[[]]]=frag", "", NEW[frag], fixed=TRUE)   
  	}
    ud1 <- which(grepl("[[[]]]=sud", NEW, fixed = TRUE))
    ud2 <- which(grepl("[[[]]]=eud", NEW, fixed = TRUE)) 
    ext1 <- which.max(c(length(ud1), length(ud2)))
    ext2 <- which.min(c(length(ud1), length(ud2)))
    if (length(ud1) != length(ud1)) {
    	codes <- c("[[[]]]=sud","[[[]]]=eud")
    	stop(paste0("More ", codes[ext1], " code chunks than ", codes[ext2]))
    }  
    if (length(ud1) > 0) { 
        reps1 <- mgsub(c("<p>[[[]]]=sud # ", "<p>[[[]]]=sud #", "</p>"), 
            c("</section>\n<section>\n<section>\n<h1>",
                "</section>\n<section>\n<section>\n<h1>", "</h1>"), 
        	    NEW[ud1], fixed = TRUE) 
        reps2 <- mgsub(c("<p>[[[]]]=eud</p>"), 
            c("</section>"), NEW[ud2], fixed = TRUE) 
        NEW[ud1] <- reps1
        NEW[ud2] <- reps2        
  	}
    notes <- which(grepl("]]]=notes</p>", NEW, fixed = TRUE)) 
    if (length(notes) > 0) {
        nts <- mgsub(c("]]]=notes</p>", "[[["), 
            c("</aside>\n", "\n<aside class=\"notes\">\n"), NEW[notes], fixed = TRUE) 
        NEW[notes] <- nts
  	}
    lchunk <- grepl("[[[", NEW, fixed = TRUE)
    yt <- which(lchunk & grepl("]]]=yt-ap", NEW, fixed = TRUE)) 
    if (!identical(yt, integer(0))) {
        yt2 <- mgsub(c("<p>[[[", "]]]=yt-ap</p>"), "", NEW[yt], fixed = TRUE) 
        yt2b <- strsplit(yt2, "v=")
        yt2c <- strsplit(sapply(yt2b, function(x) x[length(x)]), "&")
        tags <- sapply(yt2c, function(x) x[1])
        yt3 <- paste0("<iframe class=\"youtube-player\" type=\"text/html\" width=\"800\" height=\"500\" src=\"http://www.youtube.com/embed/", 
            tags, "?autoplay=1", "\" frameborder=\"0\"> </iframe>")
        NEW[yt] <- yt3  
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
    tmp <- tempdir()
    cat(paste0(NEW, collapse = "\n"), file=file.path(tmp, "temphtml"))
    NEW <- suppressWarnings(readLines(file.path(tmp, "temphtml")))
    bq <- which(grepl("<blockquote>", NEW, fixed = TRUE)) + 1
    if (!identical(bq, integer(0))) {      
        front <- "<blockquote cite=\"http://searchservervirtualization.techtarget.com/definition/Our-Favorite-Technology-Quotations\">\n"  
        NEW <- mgsub(c("<blockquote>"), c(front), NEW, fixed = TRUE)  
        NEW[bq] <- mgsub(c("<p>", "</p>"), "", NEW[bq], fixed = TRUE)  
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
#browser()
    bg <- which(grepl("[[[]]]=bg", NEW, fixed = TRUE)) 
    if (length(bg) > 0) {
    	colscheme <- gsub("^\\s+|\\s+$", "", genXtract(NEW[bg], "bg-", "</h1>"))
        NEW[bg] <- paste0(genX(NEW[bg], "[[[]]]=bg-", "</h1>"), "</h1>") 
        NEW[bg-1] <- paste0("<section data-state=\"", colscheme,"\">")
  	}  
    end <- which(grepl("<!-- {{{{ dzslides core", NEW, fixed=TRUE)) - 1
    start <- which(grepl("<body>", NEW, fixed=TRUE))[1] + 1 
    insert <- NEW[start:end]
    out <- suppressWarnings(readLines(file.path(x, "index.html")))
    key <- "<!-- SLIDE STUFF HERE -->" 
    locs <- which(grepl(key, out, fixed=TRUE)) 
    NEW <- c(out[1:locs[1]], insert, out[locs[2]:(length(out))])  
    trn <- grepl("transition: Reveal.getQueryHash().transition ||", NEW, fixed=TRUE)
    NEW[trn] <- paste0("transition: Reveal.getQueryHash().transition || '", 
        transition, "', // default/cube/page/concave/zoom/linear/fade/none")
    thm <- grepl("<link rel=\"stylesheet\" href=\"css/theme/", NEW, fixed=TRUE)
    NEW[thm] <- paste0("<link rel=\"stylesheet\" href=\"css/theme/", theme,
        ".css\" id=\"theme\">")
    if (file.exists(file.path(path, figure.dir))) {
	    fig <- file.path(path, figure.dir)
	    file.copy(fig, x, recursive = TRUE)
    }
    NEW <- gsub("</section>", "</section>\n\n", NEW)    
    cat(paste0(NEW, collapse = "\n"), file=file.path(x, "index.html"))     
    cat("HTML5 reveal.js directory generated: click \"reveal_js/index.html\"!\n")
}
